(* C 后端：从 MIR 生成 C。每个基本块是一个标签，块之间用 goto 跳转；
   MIR 已经按从左到右的顺序把操作数存进了局部变量，这里不需要再处理求值顺序。 *)
open Ast

(* 和程序无关的 C 运行时在 runtime/sinter.h，构建时编进编译器（src/dune）。 *)
let prelude = Runtime_src.prelude

(* 泛型实例（名字里有 <）的 C 名字：编号加上去掉符号的写法，和用户写的名字（S_、u_ 开头）不会重名。 *)
let gen_names : (string, string) Hashtbl.t = Hashtbl.create 16

(* 名字本身就是 C 的标识符（只有字母、数字、下划线）时直接用；元组的实例 (I64, Bool) 等另起名字 *)
let is_ident n = String.for_all (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false) n

let cname prefix n =
  (* 泛型实例（Option<I64>）和别的模块里的名字（geom.area）不是 C 的标识符，另起一个名字 *)
  if is_ident n then prefix ^ n
  else
    match Hashtbl.find_opt gen_names n with
    | Some c -> c
    | None ->
        let clean = String.map (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '_') n in
        let c = Printf.sprintf "g%d_%s" (Hashtbl.length gen_names) clean in
        Hashtbl.add gen_names n c;
        c

(* 结构体、枚举。只差区域的实例（Option<&I64 in a> 和 Option<&I64 in b>）是同一个 C 类型。 *)
let sname n = cname "S_" (match Typeck.erase (TStruct n) with TStruct m -> m | _ -> n)
let fname n = cname "u_" n   (* 函数 *)

(* Result<T, E> 的 C 名字由两个类型拼出来（每个用到的 Result 是一个具体的枚举）。 *)
let rec mangle = function
  | TI64 -> "I64"
  | TF64 -> "F64"
  | TChar -> "Char"
  | TBool -> "Bool"
  | TStr -> "Str"
  | TBytes -> "Bytes"
  | TView -> "View"
  | TUnit -> "U"
  | TStruct n -> if is_ident n then n else sname n
  | TBox t -> "B" ^ mangle t
  | TVec t -> "V" ^ mangle t ^ "_"
  | TResult (a, b) -> "R" ^ mangle a ^ "_" ^ mangle b ^ "_"
  | TRef (t, _) -> "P" ^ mangle t
  | TArena _ -> "A"
  | TOFn _ -> "Fn"   (* 所有 Fn(…) 的 C 表示相同 *)
  | TApp _ | TParam _ | TFn _ -> failwith "泛型的类型参数和函数类型不会出现在生成的 C 里"

let rec cty = function
  | TI64 -> "int64_t"
  | TF64 -> "double"
  | TChar -> "uint32_t"
  | TBool -> "bool"
  | TStr -> "sn_str"
  | TBytes -> "sn_str"   (* 字节串和字符串的表示相同 *)
  | TView -> "sn_view"
  | TUnit -> "sn_unit"   (* 存值的地方（变量、字段、载荷、Vec 元素）占一个字节；函数返回 () 时仍是 void *)
  | TStruct n -> sname n
  | TBox t -> cty t ^ "*"
  | TResult _ as t -> "S_" ^ mangle t
  | TVec _ as t -> "S_" ^ mangle t
  | TRef (TView, _) -> "sn_view"   (* 字符串切片：指针和长度，按值传 *)
  | TRef (t, _) -> cty t ^ "*"
  | TArena _ -> "sn_arena"
  | TOFn _ -> "sn_fn"
  | TApp _ | TParam _ | TFn _ -> failwith "泛型的类型参数和函数类型不会出现在生成的 C 里"

(* 析构一个值：先析构 Box 里面的值，再释放 Box；字符串释放它的字节；结构体、枚举（含 Result）调用这个类型的析构函数。
   析构函数每个类型一个（type_defs 生成），所以经过 Box 的递归类型（链表、树）的析构是递归调用（Lean `Datatypes.Recursive`）。 *)
let drop_fn t = "sn_drop_" ^ cty t

let rec drop_code ty lv =
  match ty with
  | TBox t -> drop_code t ("(*" ^ lv ^ ")") ^ Printf.sprintf "sn_free(%s); " lv
  | TStr | TBytes -> Printf.sprintf "if (!%s.lit) sn_free(%s.p); " lv lv
  | TOFn _ -> Printf.sprintf "%s.drop(%s.env); " lv lv
  | (TStruct _ | TResult _ | TVec _) when Typeck.needs_drop ty -> Printf.sprintf "%s(&%s); " (drop_fn ty) lv
  | TI64 | TF64 | TChar | TBool | TView | TUnit | TRef _ | TArena _ | TStruct _ | TResult _ | TVec _ | TApp _ | TParam _ | TFn _ -> ""

(* 析构函数的函数体：结构体逐个析构字段；枚举按运行时标签析构对应变体的载荷（Lean `Datatypes.Enum` 的 dropOne）；
   Vec 逐个析构元素，再释放缓冲区（Lean `Datatypes.Vec`）。 *)
let drop_body ty lv =
  match ty, Typeck.variants ty with
  | _, Some vs ->
      let cases =
        List.concat
          (List.mapi
             (fun i (_, t) ->
               match t with
               | Some t ->
                   let d = drop_code t (Printf.sprintf "%s.u.v%d" lv i) in
                   if d = "" then [] else [ Printf.sprintf "case %d: %sbreak; " i d ]
               | None -> [])
             vs)
      in
      if cases = [] then "" else Printf.sprintf "switch (%s.tag) { %sdefault: break; } " lv (String.concat "" cases)
  | TVec t, _ ->
      Printf.sprintf "for (int64_t i_ = 0; i_ < %s.len; i_++) { %s} if (%s.data) sn_free(%s.data); " lv
        (drop_code t (lv ^ ".data[i_]")) lv lv
  | TStruct n, None ->
      String.concat ""
        (List.map (fun (f : field) -> drop_code f.fty (lv ^ ".f_" ^ f.fname)) (Hashtbl.find Typeck.struct_tbl n))
  | _ -> ""

(* Vec 内建函数的辅助函数名。 *)
let vec_fn b e = Printf.sprintf "sn_v%s_%s" b (cty (TVec e))

(* 类型定义：先给所有结构体、枚举写前向声明，再按依赖顺序写定义（按值包含的类型先定义；
   Box 指向的类型只需要前向声明，放到后面再处理，所以经过 Box 的递归没有问题）；
   最后是需要析构的类型的析构函数（先写原型，析构函数之间可以互相调用）。 *)
let type_defs (p : program) (fs : Mir.func list) =
  let fwd = Buffer.create 256 and defs = Buffer.create 1024 in
  let protos = Buffer.create 256 and bodies = Buffer.create 1024 in
  let seen = Hashtbl.create 16 in
  let later = Queue.create () in
  let declare t =
    Hashtbl.replace seen (cty t) ();
    Buffer.add_string fwd (Printf.sprintf "typedef struct %s %s;\n" (cty t) (cty t));
    if Typeck.needs_drop t then begin
      Buffer.add_string protos (Printf.sprintf "static void %s(%s *p);\n" (drop_fn t) (cty t));
      Buffer.add_string bodies
        (Printf.sprintf "static void %s(%s *p) { %s}\n" (drop_fn t) (cty t) (drop_body t "(*p)"))
    end
  in
  let rec visit t =
    match t, Typeck.variants t with
    | (TBox t | TRef (t, _)), _ -> Queue.add t later
    | (TStruct _ | TResult _ | TVec _), _ when Hashtbl.mem seen (cty t) -> ()
    | TVec e, _ ->
        (* 动态数组：缓冲区指针、长度、容量；元素类型只需要前向声明 *)
        declare t;
        Queue.add e later;
        Buffer.add_string defs
          (Printf.sprintf "struct %s { %s *data; int64_t len, cap; };\n" (cty t) (cty e))
    | _, Some vs ->
        (* 枚举（含 Result）：标签加上各个载荷的 union *)
        declare t;
        List.iter (fun (_, t) -> Option.iter visit t) vs;
        let pays =
          List.concat
            (List.mapi
               (fun i (_, t) ->
                 match t with Some t -> [ Printf.sprintf " %s v%d;" (cty t) i ] | None -> [])
               vs)
        in
        Buffer.add_string defs
          (Printf.sprintf "struct %s { int64_t tag;%s };\n" (cty t)
             (if pays = [] then "" else Printf.sprintf " union {%s } u;" (String.concat "" pays)))
    | TStruct n, _ ->
        declare t;
        let fields = Hashtbl.find Typeck.struct_tbl n in
        List.iter (fun (f : field) -> visit f.fty) fields;
        Buffer.add_string defs (Printf.sprintf "struct %s {" (sname n));
        List.iter
          (fun (f : field) -> Buffer.add_string defs (Printf.sprintf " %s f_%s;" (cty f.fty) f.fname))
          fields;
        Buffer.add_string defs " };\n"
    | (TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TArena _ | TResult _ | TApp _ | TParam _ | TFn _ | TOFn _), _ -> ()
  in
  List.iter
    (function
      | Struct { name; tparams = []; _ } | Enum { name; tparams = []; _ } -> visit (TStruct name)
      | Struct _ | Enum _ | Fn _ | Trait _ | Impl _ -> ())   (* 泛型定义本身没有 C 类型，用到的实例从函数的局部变量找到 *)
    p;
  List.iter
    (fun (f : Mir.func) ->
      visit f.ret;
      Array.iter (fun (l : Mir.local_info) -> visit l.lty) f.locals)
    fs;
  (* Vec 内建函数的辅助函数用到的类型（vec_pop 返回 Option<T>） *)
  Hashtbl.iter
    (fun _ (b, e) ->
      visit (TVec e);
      if b = "pop" then visit (Typeck.instantiate "Option" [ e ]))
    Typeck.vec_calls;
  (* read_line、str_to_i64 返回的 Option<Str>、Option<I64>：辅助函数总是生成，类型也总要有 *)
  let opt_str = Typeck.instantiate "Option" [ TStr ] and opt_i64 = Typeck.instantiate "Option" [ TI64 ] in
  let opt_f64 = Typeck.instantiate "Option" [ TF64 ] in
  visit opt_f64;
  (* 字符：char_from 返回的 Option<Char>，str_chars 返回的 Vec<Char>：辅助函数总是生成，类型也总要有 *)
  let opt_char = Typeck.instantiate "Option" [ TChar ] in
  visit opt_char;
  visit (TVec TChar);
  visit opt_str;
  visit opt_i64;
  (* args、read_file、write_file 的返回类型 *)
  let vstr = TVec TStr and rstr = TResult (TStr, TStr) and runit = TResult (TUnit, TStr) in
  visit vstr;
  visit rstr;
  visit runit;
  (* read_bytes 的返回类型 *)
  let rbytes = TResult (TBytes, TStr) in
  visit rbytes;
  (* std.os 的返回类型 *)
  let rvstr = TResult (TVec TStr, TStr) and ri64 = TResult (TI64, TStr) in
  visit rvstr;
  visit ri64;
  (* read_file_in 的返回类型 *)
  let rview = TResult (TRef (TView, "_"), TStr) in
  visit rview;
  while not (Queue.is_empty later) do
    visit (Queue.pop later)
  done;
  (* Vec 内建函数：每个用到的（函数, Vec 的 C 类型）一行宏（runtime/sinter.h 的 SN_VEC_*），区域名不同的元素类型共用一个 *)
  let helpers = Buffer.create 256 in
  let emitted = Hashtbl.create 16 in
  Hashtbl.iter
    (fun _ (b, e) ->
      let fn = vec_fn b e in
      if not (Hashtbl.mem emitted fn) then begin
        Hashtbl.add emitted fn ();
        let v = cty (TVec e) and te = cty e in
        Buffer.add_string helpers
          (match b with
          | "new" -> Printf.sprintf "SN_VEC_NEW(%s, %s)\n" fn v
          | "push" -> Printf.sprintf "SN_VEC_PUSH(%s, %s, %s)\n" fn v te
          | "pop" -> Printf.sprintf "SN_VEC_POP(%s, %s, %s)\n" fn v (cty (Typeck.instantiate "Option" [ e ]))
          | "swap" -> Printf.sprintf "SN_VEC_SWAP(%s, %s, %s)\n" fn v te
          | _ -> Printf.sprintf "SN_VEC_LEN(%s, %s)\n" fn v)
      end)
    Typeck.vec_calls;
  (* runtime/sinter_lib.h（I/O、解析、std.os）用到的内建实例类型的 C 名字，然后原样放这一段 *)
  List.iter
    (fun (m, t) -> Buffer.add_string helpers (Printf.sprintf "#define %s %s\n" m (cty t)))
    [
      ("SN_OPT_CHAR", opt_char); ("SN_VEC_CHAR", TVec TChar); ("SN_OPT_F64", opt_f64); ("SN_OPT_STR", opt_str);
      ("SN_OPT_I64", opt_i64); ("SN_VEC_STR", vstr); ("SN_RES_STR", rstr); ("SN_RES_UNIT", runit);
      ("SN_RES_BYTES", rbytes); ("SN_RES_VIEW", rview); ("SN_RES_VEC_STR", rvstr); ("SN_RES_I64", ri64);
    ];
  Buffer.add_string helpers Runtime_src.library;
  Buffer.contents fwd ^ Buffer.contents defs ^ Buffer.contents protos ^ Buffer.contents bodies
  ^ Buffer.contents helpers

(* ---------- 从 MIR 生成 ---------- *)

(* let / inout 参数和投影在 C 里是指针，用到时先解引用。 *)
let local (f : Mir.func) l =
  if f.locals.(l).lref then Printf.sprintf "(*l%d)" l else Printf.sprintf "l%d" l

let place f (p : Mir.place) =
  List.fold_left
    (fun s -> function
      | Mir.PField f -> Printf.sprintf "%s.f_%s" s f
      | Mir.PDeref -> Printf.sprintf "(*%s)" s
      | Mir.PVar i -> Printf.sprintf "%s.u.v%d" s i
      | Mir.PIndex i -> Printf.sprintf "%s.data[sn_idx(%s.len, %s)]" s s (local f i))
    (local f p.base) p.proj

let operand f = function
  | Mir.Const (Mir.CInt n) -> Printf.sprintf "INT64_C(%Ld)" n
  | Mir.Const (Mir.CChar c) -> Printf.sprintf "UINT32_C(%d)" c
  | Mir.Const (Mir.CF64 x) -> Printf.sprintf "%h" x   (* 十六进制浮点数：和字面量的值完全相同 *)
  | Mir.Const (Mir.CBool b) -> if b then "true" else "false"
  | Mir.Const Mir.CUnit -> "0"
  | Mir.Const (Mir.CStr s) ->
      (* C 的字符串字面量：可打印的 ASCII 原样写，其余字节（包括 UTF-8 的中文）写成八进制转义 *)
      let b = Buffer.create (String.length s + 2) in
      Buffer.add_char b '"';
      String.iter
        (fun c ->
          match c with
          | '"' | '\\' -> Buffer.add_char b '\\'; Buffer.add_char b c
          | ' ' .. '~' -> Buffer.add_char b c
          | c -> Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c)))
        s;
      Buffer.add_char b '"';
      Buffer.contents b
  | Mir.Copy p | Mir.Move p -> place f p
  | Mir.Ref p | Mir.RefMut p -> Printf.sprintf "&%s" (place f p)

let rvalue fn rv =
  let operand = operand fn in
  match rv with
  | Mir.Use o -> operand o
  | Mir.Bin (op, a, b) -> (
      let a = operand a and b = operand b in
      let f name = Printf.sprintf "sn_%s(%s, %s)" name a b in
      let c sym = Printf.sprintf "(%s %s %s)" a sym b in
      match op with
      | Add -> f "add" | Sub -> f "sub" | Mul -> f "mul" | Div -> f "div" | Rem -> f "rem"
      | Eq -> c "==" | Ne -> c "!=" | Lt -> c "<" | Le -> c "<=" | Gt -> c ">" | Ge -> c ">="
      | And -> c "&&" | Or -> c "||")
  | Mir.Un (Neg, a) -> Printf.sprintf "sn_neg(%s)" (operand a)
  | Mir.Un (Not, a) -> Printf.sprintf "(!%s)" (operand a)
  | Mir.Call (f, args) ->
      let name = match Hashtbl.find_opt Typeck.vec_calls f with Some (b, e) -> vec_fn b e | None -> fname f in
      Printf.sprintf "%s(%s)" name (String.concat ", " (List.map operand args))
  | Mir.Agg (n, fs) ->
      let fs = List.map (fun (f, o) -> Printf.sprintf ".f_%s = %s" f (operand o)) fs in
      Printf.sprintf "((%s){ %s })" (sname n) (String.concat ", " fs)
  | Mir.Unbox (t, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = %s; %s v_ = *p_; sn_free(p_); v_; })" t (operand o) t
  | Mir.BoxNew (t, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = sn_alloc(sizeof(%s)); *p_ = %s; p_; })" t t (operand o)
  | Mir.MkEnum (t, i, o) ->
      let payload = match o with Some o -> Printf.sprintf ", .u.v%d = %s" i (operand o) | None -> "" in
      Printf.sprintf "((%s){ .tag = %d%s })" (cty t) i payload
  | Mir.Payload (o, i) -> Printf.sprintf "%s.u.v%d" (operand o) i
  | Mir.TagIs (o, i) -> Printf.sprintf "(%s.tag == %d)" (operand o) i
  | Mir.Consume _ -> failwith "Consume 只出现在 Eval 里"
  | Mir.Alloc (t, a, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = sn_arena_alloc(&%s, sizeof(%s)); *p_ = %s; p_; })" t (place fn a) t
        (operand o)

(* 从入口能走到的块；走不到的块（return 之后的死代码）不输出。 *)
let reachable (f : Mir.func) =
  let seen = Array.make (Array.length f.blocks) false in
  let rec go i =
    if not seen.(i) then begin
      seen.(i) <- true;
      match f.blocks.(i).term with
      | Mir.Goto j -> go j
      | Mir.Branch (_, a, b) -> go a; go b
      | Mir.Return | Mir.Halt | Mir.Unterminated -> ()
    end
  in
  go 0;
  seen

(* 局部变量的 C 类型：指针局部多一层 *。 *)
let lty (l : Mir.local_info) = if l.lref then cty l.lty ^ "*" else cty l.lty

let signature (f : Mir.func) =
  let ps =
    if f.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun l -> Printf.sprintf "%s l%d" (lty f.locals.(l)) l) f.params)
  in
  Printf.sprintf "static %s %s(%s)" (if f.ret = TUnit then "void" else cty f.ret) (fname f.name) ps

let func b (f : Mir.func) =
  let add s = Buffer.add_string b s in
  add (signature f ^ " {\n");
  Array.iteri
    (fun i (l : Mir.local_info) ->
      if not (List.mem i f.params) then
        add (Printf.sprintf "  %s l%d;%s\n" (lty l) i
               (if l.lname = "" then "" else Printf.sprintf "  /* %s */" l.lname)))
    f.locals;
  let seen = reachable f in
  let place = place f and operand = operand f and rvalue = rvalue f in
  Array.iteri
    (fun i (blk : Mir.block) ->
      if seen.(i) then begin
        add (Printf.sprintf "bb%d: ;\n" i);
        List.iter
          (function
            | Mir.Assign (p, rv) -> add (Printf.sprintf "  %s = %s;\n" (place p) (rvalue rv))
            | Mir.Eval (Mir.Consume (t, o)) -> add (Printf.sprintf "  %s\n" (drop_code t (operand o)))
            | Mir.Eval (Mir.Payload _) -> ()   (* 只把值标记为已移走，没有代码 *)
            | Mir.Eval rv -> add (Printf.sprintf "  %s;\n" (rvalue rv))
            | Mir.Proj (q, p) -> add (Printf.sprintf "  l%d = &%s;\n" q (place p))
            | Mir.DropNow l ->
                add (Printf.sprintf "  %s\n" (drop_code f.locals.(l).lty (local f l)))
            | Mir.DropIf (l, fl) ->
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code f.locals.(l).lty (local f l)))
            | Mir.DropAt p -> add (Printf.sprintf "  %s\n" (drop_code (Mir.place_ty f p) (place p)))
            | Mir.DropField (l, path) ->
                let p = { (Mir.whole l) with proj = List.map (fun g -> Mir.PField g) path } in
                add (Printf.sprintf "  %s\n" (drop_code (Mir.place_ty f p) (place p)))
            | Mir.DropFieldIf (l, path, fl) ->
                let p = { (Mir.whole l) with proj = List.map (fun g -> Mir.PField g) path } in
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code (Mir.place_ty f p) (place p)))
            | Mir.Live _ | Mir.Raise -> ()
            | Mir.Enter l -> add (Printf.sprintf "  %s.head = 0;\n" (local f l))
            | Mir.Leave l -> add (Printf.sprintf "  sn_arena_free(&%s);\n" (local f l))
            | Mir.Drop _ -> failwith "Drop 应当已由 Own.elaborate 展开")
          blk.stmts;
        match blk.term with
        | Mir.Goto j -> add (Printf.sprintf "  goto bb%d;\n" j)
        | Mir.Branch (o, a, c) ->
            add (Printf.sprintf "  if (%s) goto bb%d; else goto bb%d;\n" (operand o) a c)
        | Mir.Halt -> add "  __builtin_unreachable();\n"   (* u_panic 不会返回 *)
        | Mir.Return | Mir.Unterminated -> (
            match f.ret_local with
            | Some r -> add (Printf.sprintf "  return l%d;\n" r)
            | None -> add "  return;\n")
      end)
    f.blocks;
  add "}\n"

(* Fn(…)（Lean `Closures.Stored`）：每个造闭包的 Fn#… 一个环境结构体（捕获按值存进去）、一个转发函数（把环境的字段按指针
   交给提升出的函数，后面接上调用的实参）、一个析构函数（析构每个捕获，释放环境）；每种 Fn 类型一个 call#…，
   按类型的签名经过 code 调用。let / inout 参数在 C 里是指针，sink 参数是值。 *)
let closures () =
  let b = Buffer.create 256 in
  let add s = Buffer.add_string b s in
  let pty (m, t) = if m = MSink then cty t else cty t ^ " *" in
  let rty r = if r = TUnit then "void" else cty r in
  let names = List.sort compare (Hashtbl.fold (fun n _ acc -> n :: acc) Typeck.packs []) in
  List.iteri
    (fun k pk ->
      let (c : Typeck.closure), t = Hashtbl.find Typeck.packs pk in
      match t with
      | TOFn (pts, r) ->
          let fields = List.mapi (fun i (_, _, t) -> Printf.sprintf " %s c%d;" (cty t) i) c.caps in
          add (Printf.sprintf "struct sn_env%d {%s };\n" k (if fields = [] then " char c_;" else String.concat "" fields));
          let ps = List.mapi (fun i p -> Printf.sprintf ", %s a%d" (pty p) i) pts in
          let fwd =
            List.mapi (fun i _ -> Printf.sprintf "&x->c%d" i) c.caps @ List.mapi (fun i _ -> Printf.sprintf "a%d" i) pts
          in
          add
            (Printf.sprintf "static %s sn_code%d(void *e%s) { struct sn_env%d *x = e; (void)x; %s%s(%s); }\n" (rty r) k
               (String.concat "" ps) k
               (if r = TUnit then "" else "return ")
               (fname c.cname) (String.concat ", " fwd));
          let drops = List.mapi (fun i (_, _, t) -> drop_code t (Printf.sprintf "x->c%d" i)) c.caps in
          add
            (Printf.sprintf "static void sn_envdrop%d(void *e) { struct sn_env%d *x = e; (void)x; %ssn_free(e); }\n" k k
               (String.concat "" drops));
          let args = List.mapi (fun i (_, _, t) -> Printf.sprintf "%s c%d" (cty t) i) c.caps in
          let sets = List.mapi (fun i _ -> Printf.sprintf "x->c%d = c%d; " i i) c.caps in
          add
            (Printf.sprintf
               "static sn_fn %s(%s) { struct sn_env%d *x = sn_alloc(sizeof(struct sn_env%d)); %sreturn (sn_fn){ x, (void (*)(void))sn_code%d, sn_envdrop%d }; }\n"
               (fname pk)
               (if args = [] then "void" else String.concat ", " args)
               k k (String.concat "" sets) k k)
      | _ -> ())
    names;
  let calls = List.sort compare (Hashtbl.fold (fun n t acc -> (n, t) :: acc) Typeck.fcalls []) in
  List.iter
    (fun (n, t) ->
      match t with
      | TOFn (pts, r) ->
          let ps = List.mapi (fun i p -> Printf.sprintf ", %s a%d" (pty p) i) pts in
          let cast = Printf.sprintf "(%s (*)(void *%s))" (rty r) (String.concat "" (List.map (fun p -> ", " ^ pty p) pts)) in
          add
            (Printf.sprintf "static %s %s(sn_fn *f%s) { %s(%sf->code)(f->env%s); }\n" (rty r) (fname n) (String.concat "" ps)
               (if r = TUnit then "" else "return ")
               cast
               (String.concat "" (List.mapi (fun i _ -> Printf.sprintf ", a%d" i) pts)))
      | _ -> ())
    calls;
  Buffer.contents b

(* batch：对照测试的函数名。给了的话不调用 u_main，而是每个函数按所有长度不超过 3 的选择序列各跑一遍，
   每次跑完检查有没有没释放的 Box 或区域内存块。 *)
let program ?batch (p : program) (fs : Mir.func list) : string =
  let b = Buffer.create 4096 in
  let add s = Buffer.add_string b s in
  add prelude;
  add "\n";
  add (type_defs p fs);
  List.iter (fun f -> add (signature f ^ ";\n")) fs;
  add (closures ());
  List.iter (func b) fs;
  (* 入口是 runtime/sinter.h 的 sn_main、sn_batch *)
  (match batch with
  | None -> add "int main(int argc, char **argv) { return sn_main(argc, argv, u_main); }\n"
  | Some names ->
      let rec seqs n = if n = 0 then [ "" ] else "" :: List.concat_map (fun s -> [ "1" ^ s; "0" ^ s ]) (seqs (n - 1)) in
      let seqs = List.sort_uniq compare (seqs 3) in
      let strs l = String.concat ", " (List.map (Printf.sprintf "\"%s\"") l) in
      add (Printf.sprintf "static const char *const sn_seqs[] = { %s };\n" (strs seqs));
      List.iter (fun n -> add (Printf.sprintf "static void t_%s(void) { (void)u_%s(); }\n" n n)) names;
      add (Printf.sprintf "static const sn_test sn_tests[] = { %s };\n" (String.concat ", " (List.map (( ^ ) "t_") names)));
      add (Printf.sprintf "static const char *const sn_names[] = { %s };\n" (strs names));
      add
        (Printf.sprintf "int main(void) { return sn_batch(sn_tests, sn_names, %d, sn_seqs, %d); }\n" (List.length names)
           (List.length seqs)));
  Buffer.contents b
