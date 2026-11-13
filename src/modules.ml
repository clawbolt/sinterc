(* 模块：一个文件一个模块。use m 引入和当前文件同目录的 m.sn，之后用 m.f(…)、m.T 访问它的 pub 条目；
   use m { a, B } 另外把这几个名字直接引入。所有模块读进来之后合成一个程序交给类型检查：
   模块 m 里的函数、结构体、枚举、trait 加上前缀，名字变成 m.f、m.T（主文件不加前缀）。
   trait 的方法名不加前缀（方法跟着 trait 走，和 impl 一样是全局的）；类型自己的方法（impl 类型 { … }）换成函数 头部.方法。
   改名只是名字的事，不改变任何检查规则，所以不需要新的 Lean 里程碑。
   consume 只能在定义这个 linear 类型的文件里用（见 typeck.ml 的 consume），linear 类型的封装由此成立。
   标准库：use std.m 引入编进编译器的 std/m.sn（见 stdlib_src.ml，由 std/*.sn 生成），之后照样写 m.f(…)。
   模块按最后一段名字区分：同一个程序里不能既有自己的 m.sn 又用 std.m。 *)
open Ast

let err pos fmt = Printf.ksprintf (fun m -> raise (Syntax (pos, m))) fmt

type modinfo = {
  modname : string;                        (* 主文件是 "" *)
  index : int;                             (* 第几个文件 *)
  src : source;
  pubs : (string, unit) Hashtbl.t;         (* 标了 pub 的名字（不带前缀） *)
  defs : (string, unit) Hashtbl.t;         (* 所有顶层名字（不带前缀） *)
}

let item_name = function
  | Struct { name; _ } | Enum { name; _ } | Fn { name; _ } | Trait { name; _ } -> Some name
  | Impl _ -> None

let item_pos = function
  | Struct { ipos; _ } | Enum { ipos; _ } | Fn { ipos; _ } | Trait { ipos; _ } | Impl { ipos; _ } -> ipos

let prefixed m x = if m = "" then x else m ^ "." ^ x

(* use 写的名字 → 模块名：std.math 就是 math。 *)
let alias m = match String.rindex_opt m '.' with Some i -> String.sub m (i + 1) (String.length m - i - 1) | None -> m

(* 预导入的核心部分：std/core/*.sn 每个程序都自动读进来，它们的条目加前缀 core.（几个文件共用这一个前缀），
   pub 的名字在每个文件里直接可见（本文件的定义和 use 引入的同名名字优先）。 *)
let core = "core"

let is_core_file name = String.length name > 5 && String.sub name 0 5 = "core/"

(* 这个位置在核心部分的文件里吗（用来去掉没有用到的核心函数）。 *)
let in_core (p : pos) =
  let i = file_of p in
  i > 0 && i < Array.length !files && is_core_file (String.sub !files.(i) 4 (max 0 (String.length !files.(i) - 4)))

(* 读一个文件（text 给出时读标准库里编进来的源码）：第 index 个文件的行号从 index * line_base + 1 开始。 *)
let parse_file ?text path index =
  Lexer.depth := 0;
  Lexer.saved := [];
  Lexer.pending := [];
  let ic =
    match text with
    | Some _ -> None
    | None -> ( try Some (open_in_bin path) with Sys_error _ -> raise (Sys_error path))
  in
  let close () = Option.iter close_in ic in
  let lexbuf =
    match ic, text with
    | Some ic, _ -> Lexing.from_channel ic
    | None, t -> Lexing.from_string (Option.value t ~default:"")
  in
  Lexing.set_filename lexbuf path;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = (index * line_base) + 1 };
  let src =
    try Parser.program Lexer.next lexbuf with
    | Parser.Error ->
        close ();
        let p = Lexing.lexeme_start_p lexbuf in
        raise
          (Syntax
             ( { line = p.pos_lnum; col = p.pos_cnum - p.pos_bol + 1 },
               Printf.sprintf "语法错误（在 '%s' 附近）" (Lexing.lexeme lexbuf) ))
    | e -> close (); raise e
  in
  close ();
  src

(* 改名：模块 me 里出现的名字换成合并后的名字。 *)
(* 内建函数分三层。预导入的几个（构造、所有权和区域的原语）哪里都能直接调用；读写外部的只有 std.io、std.fs、std.env、
   std.process 和核心部分的 print.sn 能调用；其余的只有标准库能调用，用户代码写成对应的方法或标准库函数（builtin_hint）。
   语法糖展开出的调用带 #（见 parser.mly），不检查；方法调用 s.len() 由类型检查换成 str_len，类型函数 Vec.new() 由改名换成 vec_new，
   也不经过这里。 *)
let prelude_builtins = [ "box"; "unbox"; "consume"; "alloc"; "panic"; "text"; "choose" ]

let io_builtins =
  [
    "print"; "print_str"; "print_byte"; "eprint"; "eprint_str";
    "print_bytes"; "read_line"; "args"; "read_file"; "write_file"; "read_bytes"; "write_bytes"; "read_file_in";
    "read_stdin_in"; "read_chunk"; "os_env"; "os_list_dir"; "os_exists"; "os_is_dir"; "os_make_dir"; "os_remove";
    "os_run"; "os_output"; "os_exit";
  ]

let is_builtin f = List.mem_assoc f Typeck_builtins.builtins || List.mem f Typeck_builtins.special

(* 去掉了的内建函数：旧程序调用时提示新的写法，而不是“未定义的函数” *)
let removed_builtins = [ "print_i64"; "print_bool"; "print_f64"; "print_char" ]

(* 直接调用内建函数 f 时给出的写法。核心部分里有对应的 intrinsic fn 时写成那个类型函数（Vec.new()）或方法（s.len()、n.to_str()），
   其余的是系统层的函数 *)
let builtin_hint f =
  let starts p = String.starts_with ~prefix:p f in
  let strip p = String.sub f (String.length p) (String.length f - String.length p) in
  let stat = Hashtbl.fold (fun k (b, _) acc -> if b = f then Some k else acc) statics None in
  let meths = Hashtbl.fold (fun g (b, ps, _, _, _) acc -> if b = f then (g, ps) :: acc else acc) prims [] in
  let meth =
    match List.partition (fun (g, _) -> not (String.ends_with ~suffix:".to_str" g)) meths with
    | m :: _, _ | [], m :: _ -> Some m
    | [], [] -> None
  in
  match f, meth with
  | "vec_get", _ -> "v[i]"
  | _, _ when stat <> None -> Option.get stat ^ "()"
  | _, Some (g, ps) ->
      let m = String.sub g (String.rindex g '.' + 1) (String.length g - String.rindex g '.' - 1) in
      let r =
        match ps with
        | (_, _, TStr) :: _ -> "s"
        | (_, _, (TBool | TBytes)) :: _ -> "b"
        | (_, _, TChar) :: _ -> "c"
        | (_, _, TI64) :: _ -> "n"
        | (_, _, TF64) :: _ -> "x"
        | _ -> "v"
      in
      let call = r ^ "." ^ m ^ (if List.length ps > 1 then "(…)" else "()") in
      if m = "to_str" then call ^ " 或 \"{" ^ r ^ "}\"" else call
  | "print_str", _ -> "print(s)"
  | "print", _ -> "println(s)"
  | "eprint_str", _ -> "eprint(s)"
  | "eprint", _ -> "eprintln(s)"
  | ("print_i64" | "print_bool" | "print_f64" | "print_char"), _ -> "println(\"{x}\")"   (* 已经去掉的内建函数，见 removed_builtins *)
  | "print_byte", _ -> "io.write_byte(b)"
  | "print_bytes", _ -> "io.write_bytes(b)"
  | "read_line", _ -> "io.read_line()"
  | "read_stdin_in", _ -> "io.read_all_in(a)"
  | "read_chunk", _ -> "io.read_chunk(a, n)"
  | "args", _ -> "env.args()"
  | "read_file", _ -> "fs.read(path)"
  | "write_file", _ -> "fs.write(path, s)"
  | "read_file_in", _ -> "fs.read_in(a, path)"
  | "read_bytes", _ -> "fs.read_bytes(path)"
  | "write_bytes", _ -> "fs.write_bytes(path, b)"
  | "os_env", _ -> "env.get(name)"
  | "os_exit", _ -> "env.exit(code)"
  | ("os_run" | "os_output"), _ -> "process." ^ strip "os_" ^ "(prog, args)"
  | _ when starts "os_" -> "fs." ^ strip "os_" ^ "(path)"
  | _ -> f ^ "(…)"

let rename ?(only_register = false) (mods : (string, modinfo) Hashtbl.t) (me : modinfo) : item list =
  let imported = List.map (fun (m, _, _) -> alias m) me.src.uses in
  (* 直接可见的名字 → 合并后的名字：先是预导入的（核心部分的 pub 名字），本文件的定义和 use 引入的名字盖过它们 *)
  let names : (string, string) Hashtbl.t = Hashtbl.create 16 in
  Hashtbl.iter
    (fun _ mi -> if mi.modname = core then Hashtbl.iter (fun x () -> Hashtbl.replace names x (prefixed core x)) mi.pubs)
    mods;
  Hashtbl.iter (fun x () -> Hashtbl.replace names x (prefixed me.modname x)) me.defs;
  let target pos m x =
    let mi = Hashtbl.find mods m in
    if not (Hashtbl.mem mi.defs x) then err pos "模块 %s 里没有 %s" m x;
    if not (Hashtbl.mem mi.pubs x) then err pos "%s.%s 不是 pub 的，不能在模块 %s 之外使用" m x m;
    prefixed m x
  in
  List.iter
    (fun (m, ns, pos) ->
      List.iter
        (fun x ->
          if Hashtbl.mem me.defs x then err pos "引入的 %s 和本文件里的定义重名" x;
          Hashtbl.replace names x (target pos (alias m) x))
        (Option.value ns ~default:[]))
    me.src.uses;
  (* 名字：写成 m.X 的按模块找，其他的按直接可见的名字找；都不是（内建、类型参数等）就不变 *)
  let qual pos s =
    match String.rindex_opt s '.' with
    | Some i ->
        let m = String.sub s 0 i and x = String.sub s (i + 1) (String.length s - i - 1) in
        if not (List.mem m imported) then err pos "没有引入模块 %s（在文件开头写 use %s）" m m;
        target pos m x
    | None -> Option.value (Hashtbl.find_opt names s) ~default:s
  in
  let rec ty pos = function
    | TStruct n -> TStruct (qual pos n)
    | TApp (n, ts) -> TApp (qual pos n, List.map (ty pos) ts)
    | TBox t -> TBox (ty pos t)
    | TVec t -> TVec (ty pos t)
    | TResult (a, b) -> TResult (ty pos a, ty pos b)
    | TRef (t, r) -> TRef (ty pos t, r)
    | TFn (ps, r) -> TFn (List.map (fun (m, t) -> (m, ty pos t)) ps, ty pos r)
    | TOFn (ps, r) -> TOFn (List.map (fun (m, t) -> (m, ty pos t)) ps, ty pos r)
    | (TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TArena _ | TParam _) as t -> t
  in
  let in_std =
    let p = !files.(me.index) in
    me.index > 0 && String.length p > 4 && String.sub p 0 4 = "std/"
  in
  let io_ok = in_std && (List.mem me.modname [ "io"; "fs"; "env"; "process" ] || !files.(me.index) = "std/core/print.sn") in
  let io_check pos f =
    if List.mem f io_builtins && not io_ok then
      err pos "%s 读写外部，标准库里只有 std.io、std.fs、std.env、std.process 和打印能调用它" f
  in
  (* 直接调用的 f 不是本文件、引入或预导入的函数，而是内建函数 *)
  let builtin_check pos f =
    if (not (Hashtbl.mem names f)) && List.mem f removed_builtins then
      err pos "%s 已经去掉了：写成 %s" f (builtin_hint f);
    if (not (Hashtbl.mem names f)) && is_builtin f && not (List.mem f prelude_builtins) then
      if not in_std then err pos "%s 是内建函数，标准库以外不能直接调用：写成 %s" f (builtin_hint f) else io_check pos f
  in
  let binder pos x =
    if List.mem x imported then err pos "变量 %s 和引入的模块同名，换一个名字" x
  in
  let rec ex (x : expr) =
    let pos = x.pos in
    let e =
      match x.e with
      | (Int _ | Float _ | Char _ | Bool _ | Unit | Str _ | Var _) as d -> d
      | Binop (o, a, b) -> Binop (o, ex a, ex b)
      | Unop (o, a) -> Unop (o, ex a)
      | Call (f, { e = Var m; _ } :: args) when f <> "" && f.[0] = '.' && List.mem m imported ->
          (* m.f(…)：模块 m 的函数 *)
          Call (target pos m (String.sub f 1 (String.length f - 1)), List.map ex args)
      | Call (f, { e = Var "intrinsic"; _ } :: args) when f <> "" && f.[0] = '.' ->
          (* intrinsic.f(…)：内建函数 f，即使这个模块里有同名的函数（例如 io.read_line 调用内建的 read_line）。只有标准库能用 *)
          if not in_std then err pos "intrinsic.%s：内建函数只能在标准库里这样调用" (String.sub f 1 (String.length f - 1));
          io_check pos (String.sub f 1 (String.length f - 1));
          Call (String.sub f 1 (String.length f - 1), List.map ex args)
      | Call (f, args) when f <> "" && f.[0] = '.' -> (
          (* 方法调用 r.f(…)：f 是本文件的函数或引入的名字时换成合并后的名字，其他的（trait 方法、内建）不变 *)
          let g = String.sub f 1 (String.length f - 1) in
          match Hashtbl.find_opt names g with
          | Some q -> Call ("." ^ q, List.map ex args)
          | None -> Call (f, List.map ex args))
      | Call (f, args) when f <> "" && f.[0] = '#' -> Call (String.sub f 1 (String.length f - 1), List.map ex args)
      | Call (f, args) when f <> "" && f.[0] = ':' -> (
          (* 类型函数 T.f(…)：T 按类型的名字改名，换成登记的函数（内建的就是内建函数，不检查） *)
          let s = String.sub f 1 (String.length f - 1) in
          let i = String.rindex s '.' in
          let t = String.sub s 0 i and g = String.sub s (i + 1) (String.length s - i - 1) in
          let h = qual pos t in
          match Hashtbl.find_opt statics (h ^ "." ^ g) with
          | Some (target, _) -> Call (target, List.map ex args)
          | None -> err pos "%s 没有类型函数 %s" t g)
      | Call (f, args) ->
          builtin_check pos f;
          Call (qual pos f, List.map ex args)
      | StructLit (n, fs) -> StructLit (qual pos n, List.map (fun (f, v) -> (f, ex v)) fs)
      | Field (a, f) -> Field (ex a, f)
      | Deref a -> Deref (ex a)
      | Try a -> Try (ex a)
      | Variant (v, a) -> Variant (v, Option.map ex a)
      | Match (b, s, arms) ->
          Match
            ( b,
              ex s,
              List.map
                (fun (v, y, a) ->
                  if y <> "" then binder pos y;
                  (v, y, ex a))
                arms )
      | PMatch (b, s, arms) ->
          let rec binders (q : pat) =
            match q.p with
            | PBind y -> binder pos y
            | PVariant (_, Some r) -> binders r
            | PTuple qs -> List.iter binders qs
            | PWild | PLit _ | PVariant (_, None) -> ()
          in
          PMatch (b, ex s, List.map (fun (q, a) -> binders q; (q, ex a)) arms)
      | Arena (r, b) -> Arena (r, blk b)
      | If (c, a, b) -> If (ex c, blk a, Option.map blk b)
      | Block b -> Block (blk b)
      | Lambda (ps, a) ->
          List.iter (binder pos) ps;
          Lambda (ps, ex a)
    in
    { x with e }
  and st (s : stmt) =
    let pos = s.spos in
    let d =
      match s.s with
      | Let (v, x, t, e) ->
          binder pos x;
          Let (v, x, Option.map (ty pos) t, ex e)
      | Decl (x, t) ->
          binder pos x;
          Decl (x, ty pos t)
      | Assign (x, e) -> Assign (x, ex e)
      | SetField (a, f, e) -> SetField (ex a, f, ex e)
      | SetPlace (a, e) -> SetPlace (ex a, ex e)
      | While (c, b) -> While (ex c, blk b)
      | For (a, s1, s2) -> For (ex a, st s1, st s2)
      | Return e -> Return (Option.map ex e)
      | Expr e -> Expr (ex e)
      | Errdefer e -> Errdefer (ex e)
      | (Break | Continue) as d -> d
      | Destruct (n, bs, e) ->
          List.iter (fun (_, y) -> binder pos y) bs;
          Destruct (qual pos n, bs, ex e)
    in
    { s with s = d }
  and blk (b : block) = { stmts = List.map st b.stmts; tail = Option.map ex b.tail } in
  let params pos ps =
    List.map
      (fun (m, x, t) ->
        binder pos x;
        (m, x, ty pos t))
      ps
  in
  let rec item ~top it =
    match it with
    | Struct s ->
        let name = prefixed me.modname s.name in
        Hashtbl.replace type_file name me.index;
        Struct { s with name; fields = List.map (fun f -> { f with fty = ty s.ipos f.fty }) s.fields }
    | Enum e ->
        let name = prefixed me.modname e.name in
        Hashtbl.replace type_file name me.index;
        Enum { e with name; variants = List.map (fun (v, t) -> (v, Option.map (ty e.ipos) t)) e.variants }
    | Fn f ->
        (* impl 里的方法（top = false）不改名 *)
        Fn
          {
            f with
            name = (if top then prefixed me.modname f.name else f.name);
            bounds = List.map (fun (t, tr) -> (t, qual f.ipos tr)) f.bounds;
            params = params f.ipos f.params;
            ret = ty f.ipos f.ret;
            body = blk f.body;
          }
    | Trait t ->
        Trait
          {
            t with
            name = prefixed me.modname t.name;
            methods =
              List.map
                (fun m ->
                  {
                    m with
                    mparams = params m.mpos m.mparams;
                    mret = ty m.mpos m.mret;
                    mbody = Option.map blk m.mbody;
                  })
                t.methods;
          }
    | Impl i ->
        Impl
          {
            i with
            trait = qual i.ipos i.trait;
            bounds = List.map (fun (t, tr) -> (t, qual i.ipos tr)) i.bounds;
            self = ty i.ipos i.self;
            fns = List.map (item ~top:false) i.fns;
          }
  in
  (* impl 类型 { … }：每个函数换成普通函数 头部.名字（例如 Vec<I64>.sum），类型参数和约束是 impl 的加上函数自己的；
     签名里的 Self 换成头部。第一个参数的类型是头部的是方法（接收者），登记到 Ast.inherent，类型检查按接收者的类型找到它；
     其余的是类型函数（Vec.new()、map.Map.new()），登记到 Ast.statics，调用 T.f(…) 按类型的名字找到它 *)
  let sub self =
    let rec sub = function
      | TStruct "Self" -> self
      | TBox t -> TBox (sub t)
      | TVec t -> TVec (sub t)
      | TResult (a, b) -> TResult (sub a, sub b)
      | TRef (t, r) -> TRef (sub t, r)
      | TApp (n, ts) -> TApp (n, List.map sub ts)
      | TFn (ps, r) -> TFn (List.map (fun (m, t) -> (m, sub t)) ps, sub r)
      | TOFn (ps, r) -> TOFn (List.map (fun (m, t) -> (m, sub t)) ps, sub r)
      | t -> t
    in
    sub
  in
  let is_method self params pos =
    match params with (_, _, t) :: _ -> sub self (ty pos t) = self | [] -> false
  in
  (* 先登记所有的类型函数（register），改写函数体时才找得到在后面、别的文件里定义的 *)
  let register () =
    List.iter
      (function
        | _, Impl { trait = ""; self; fns; ipos; _ } ->
            let self = ty ipos self in
            List.iter
              (function
                | Fn f when not (is_method self f.params f.ipos) ->
                    let key = type_head self ^ "." ^ f.name in
                    if Hashtbl.mem statics key then err f.ipos "%s 已经有类型函数 %s" (type_head self) f.name;
                    let target = match f.prim with Some b -> b | None -> string_of_ty self ^ "." ^ f.name in
                    Hashtbl.replace statics key (target, f.ipos)
                | _ -> ())
              fns
        | _ -> ())
      me.src.items
  in
  let expand (i : item) =
    match i with
    | Impl { trait = ""; tparams; bounds; self; fns; ipos } ->
        let self = ty ipos self in
        let bounds = List.map (fun (t, tr) -> (t, qual ipos tr)) bounds in
        let sub = sub self in
        List.filter_map
          (function
            | Fn f -> (
                let meth = is_method self f.params f.ipos in
                match item ~top:false (Fn f) with
                | Fn f ->
                    let params = List.map (fun (m, x, t) -> (m, x, sub t)) f.params in
                    let name = string_of_ty self ^ "." ^ f.name in
                    if meth then begin
                      if List.exists (fun (_, _, g, _) -> g = name) (Hashtbl.find_all inherent f.name) then
                        err f.ipos "%s 已经有方法 %s" (string_of_ty self) f.name;
                      if Hashtbl.mem statics (type_head self ^ "." ^ f.name) then
                        err f.ipos "%s 已经有类型函数 %s" (type_head self) f.name;
                      Hashtbl.add inherent f.name (self, tparams, name, f.ipos)
                    end;
                    if f.prim <> None && not in_std then err f.ipos "intrinsic fn 只能写在标准库里";
                    (match f.prim with
                    | Some b ->
                        (* intrinsic fn：只登记，不生成函数；方法调用、类型函数的调用直接换成内建函数 b *)
                        Hashtbl.replace Ast.prims name (b, params, sub f.ret, tparams @ f.tparams, f.ipos);
                        None
                    | None ->
                        Some
                          (Fn
                             {
                               f with
                               name;
                               tparams = tparams @ f.tparams;
                               bounds = bounds @ f.bounds;
                               params;
                               ret = sub f.ret;
                             }))
                | _ -> None)
            | _ -> None)
          fns
    | Impl { fns; _ } as it ->
        List.iter
          (function Fn { prim = Some _; ipos; _ } -> err ipos "intrinsic fn 只能写在 impl 类型 { … } 里，不能实现 trait" | _ -> ())
          fns;
        [ item ~top:true it ]
    | it -> [ item ~top:true it ]
  in
  if only_register then (register (); [])
  else List.concat_map (fun (_, it) -> expand it) me.src.items

(* 从主文件出发读进所有用到的模块，改名后合成一个程序。 *)
let load (main : string) : program =
  Hashtbl.reset type_file;
  Hashtbl.reset inherent;
  Hashtbl.reset Ast.prims;
  Hashtbl.reset statics;
  let mods : (string, modinfo) Hashtbl.t = Hashtbl.create 8 in
  let paths : (string, string) Hashtbl.t = Hashtbl.create 8 in   (* 模块名 → 从哪里读的 *)
  let order = ref [] in
  let file_list = ref [] in
  (* text：标准库的源码（编进编译器的） *)
  let rec visit name path text pos =
    match Hashtbl.find_opt paths name with
    | Some p ->
        if p <> path then err pos "模块 %s 既是 %s 又是 %s，同一个程序里只能用其中一个" name p path
    | None ->
        Hashtbl.replace paths name path;
        let index = List.length !file_list in
        file_list := !file_list @ [ path ];
        files := Array.of_list !file_list;
        let src =
          try parse_file ?text path index with Sys_error _ ->
            if index = 0 then raise (Sys_error path)
            else err pos "找不到模块 %s（应当是文件 %s）" name path
        in
        let pubs = Hashtbl.create 8 and defs = Hashtbl.create 8 in
        List.iter
          (fun (pub, it) ->
            match item_name it with
            | Some x ->
                Hashtbl.replace defs x ();
                if pub then Hashtbl.replace pubs x ()
            | None -> if pub then err (item_pos it) "impl 不用写 pub（实现跟着 trait 和类型，到处可见）")
          src.items;
        let mi = { modname = (if is_core_file name then core else name); index; src; pubs; defs } in
        Hashtbl.replace mods name mi;
        let dir = Filename.dirname path in
        List.iter
          (fun (m, _, upos) ->
            let a = alias m in
            if a = name then err upos "模块不能引入自己";
            if a <> m then begin
              (* use std.m：标准库 *)
              if String.sub m 0 (String.length m - String.length a - 1) <> "std" then
                err upos "只有标准库的模块写成 std.m；自己的模块直接写 use %s" a;
              match List.assoc_opt a Stdlib_src.files with
              | Some src -> visit a ("std/" ^ a ^ ".sn") (Some src) upos
              | None ->
                  err upos "标准库里没有模块 %s（有：%s）" a
                    (String.concat "、" (List.filter_map (fun (n, _) -> if is_core_file n then None else Some n) Stdlib_src.files))
            end
            else visit m (if dir = Filename.current_dir_name then m ^ ".sn" else Filename.concat dir (m ^ ".sn")) None upos)
          src.uses;
        order := mi :: !order
  in
  visit "" main None { line = 1; col = 1 };
  (* 核心部分：每个程序都读进来 *)
  List.iter
    (fun (n, src) -> if is_core_file n then visit n ("std/" ^ n ^ ".sn") (Some src) { line = 1; col = 1 })
    Stdlib_src.files;
  (* 核心部分在最前面，然后是被引入的模块，主文件最后；再往前是元组：内建的泛型结构体 Tuple#n<A0, …>，字段 0、1、… *)
  let tuple n =
    let ps = List.init n (fun i -> "A" ^ string_of_int i) in
    Struct
      {
        name = tuple_name n;
        tparams = List.map (fun p -> (p, false)) ps;
        fields = List.mapi (fun i p -> { fname = string_of_int i; fty = TStruct p }) ps;
        linear = false;
        ipos = { line = 1; col = 1 };
      }
  in
  let cores, others = List.partition (fun mi -> mi.modname = core) (List.rev !order) in
  List.iter (fun mi -> ignore (rename ~only_register:true mods mi)) (cores @ others);
  List.init (max_tuple - 1) (fun i -> tuple (i + 2)) @ List.concat_map (rename mods) (cores @ others)
