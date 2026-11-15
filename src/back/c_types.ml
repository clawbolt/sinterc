(* C 后端（二）：类型定义、析构函数、Vec 内建函数的宏，以及运行时库（runtime/sinter_lib.h）要的类型名。 *)
open Ast
open C_names

(* 析构一个值：先析构 Box 里面的值，再释放 Box；字符串释放它的字节；结构体、枚举（含 Result）调用这个类型的析构函数。
   析构函数每个类型一个（type_defs 生成），所以经过 Box 的递归类型（链表、树）的析构是递归调用（Lean `Datatypes.Recursive`）。 *)
let drop_fn d t = "sn_drop_" ^ cty d t

let rec drop_code d ty lv =
  match ty with
  | TBox t -> drop_code d t ("(*" ^ lv ^ ")") ^ Printf.sprintf "sn_free(%s); " lv
  | TStr | TBytes -> Printf.sprintf "if (!%s.lit) sn_free(%s.p); " lv lv
  | TOFn _ -> Printf.sprintf "%s.drop(%s.env); " lv lv
  | (TStruct _ | TResult _ | TVec _) when Defs.needs_drop d ty -> Printf.sprintf "%s(&%s); " (drop_fn d ty) lv
  | TI64 | TF64 | TChar | TBool | TView | TUnit | TRef _ | TArena _ | TStruct _ | TResult _ | TVec _ | TApp _ | TParam _ | TFn _ -> ""

(* 析构函数的函数体：结构体逐个析构字段；枚举按运行时标签析构对应变体的载荷（Lean `Datatypes.Enum` 的 dropOne）；
   Vec 逐个析构元素，再释放缓冲区（Lean `Datatypes.Vec`）。 *)
let drop_body d ty lv =
  match ty, Defs.variants d ty with
  | _, Some vs ->
      let cases =
        List.concat
          (List.mapi
             (fun i (_, t) ->
               match t with
               | Some t ->
                   let code = drop_code d t (Printf.sprintf "%s.u.v%d" lv i) in
                   if code = "" then [] else [ Printf.sprintf "case %d: %sbreak; " i code ]
               | None -> [])
             vs)
      in
      if cases = [] then "" else Printf.sprintf "switch (%s.tag) { %sdefault: break; } " lv (String.concat "" cases)
  | TVec t, _ ->
      Printf.sprintf "for (int64_t i_ = 0; i_ < %s.len; i_++) { %s} if (%s.data) sn_free(%s.data); " lv
        (drop_code d t (lv ^ ".data[i_]")) lv lv
  | TStruct n, None ->
      String.concat ""
        (List.map (fun (f : field) -> drop_code d f.fty (lv ^ ".f_" ^ f.fname)) (Hashtbl.find d.Defs.structs n))
  | _ -> ""

(* Vec 内建函数的辅助函数名。 *)
let vec_fn d b e = Printf.sprintf "sn_v%s_%s" b (cty d (TVec e))

(* Option<e>：vec_pop 和运行时的辅助函数用到的实例，代码生成之前已经建好（Check.generated）。 *)
let option (g : Tast.generated) e = List.assoc e g.options

(* 类型定义：先给所有结构体、枚举写前向声明，再按依赖顺序写定义（按值包含的类型先定义；
   Box 指向的类型只需要前向声明，放到后面再处理，所以经过 Box 的递归没有问题）；
   最后是需要析构的类型的析构函数（先写原型，析构函数之间可以互相调用）。
   types：程序里非泛型的结构体、枚举，按程序里的顺序（Tast.program 的 types）。 *)
let type_defs d (g : Tast.generated) (types : string list) (fs : Mir.func list) =
  let fwd = Buffer.create 256 and defs = Buffer.create 1024 in
  let protos = Buffer.create 256 and bodies = Buffer.create 1024 in
  let seen = Hashtbl.create 16 in
  let later = Queue.create () in
  let declare t =
    Hashtbl.replace seen (cty d t) ();
    Buffer.add_string fwd (Printf.sprintf "typedef struct %s %s;\n" (cty d t) (cty d t));
    if Defs.needs_drop d t then begin
      Buffer.add_string protos (Printf.sprintf "static void %s(%s *p);\n" (drop_fn d t) (cty d t));
      Buffer.add_string bodies
        (Printf.sprintf "static void %s(%s *p) { %s}\n" (drop_fn d t) (cty d t) (drop_body d t "(*p)"))
    end
  in
  let rec visit t =
    match t, Defs.variants d t with
    | (TBox t | TRef (t, _)), _ -> Queue.add t later
    | (TStruct _ | TResult _ | TVec _), _ when Hashtbl.mem seen (cty d t) -> ()
    | TVec e, _ ->
        (* 动态数组：缓冲区指针、长度、容量；元素类型只需要前向声明 *)
        declare t;
        Queue.add e later;
        Buffer.add_string defs
          (Printf.sprintf "struct %s { %s *data; int64_t len, cap; };\n" (cty d t) (cty d e))
    | _, Some vs ->
        (* 枚举（含 Result）：标签加上各个载荷的 union *)
        declare t;
        List.iter (fun (_, t) -> Option.iter visit t) vs;
        let pays =
          List.concat
            (List.mapi
               (fun i (_, t) ->
                 match t with Some t -> [ Printf.sprintf " %s v%d;" (cty d t) i ] | None -> [])
               vs)
        in
        Buffer.add_string defs
          (Printf.sprintf "struct %s { int64_t tag;%s };\n" (cty d t)
             (if pays = [] then "" else Printf.sprintf " union {%s } u;" (String.concat "" pays)))
    | TStruct n, _ ->
        declare t;
        let fields = Hashtbl.find d.Defs.structs n in
        List.iter (fun (f : field) -> visit f.fty) fields;
        Buffer.add_string defs (Printf.sprintf "struct %s {" (sname d n));
        List.iter
          (fun (f : field) -> Buffer.add_string defs (Printf.sprintf " %s f_%s;" (cty d f.fty) f.fname))
          fields;
        Buffer.add_string defs " };\n"
    | (TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TArena _ | TResult _ | TApp _ | TParam _ | TFn _ | TOFn _), _ -> ()
  in
  List.iter (fun n -> visit (TStruct n)) types;   (* 泛型定义本身没有 C 类型，用到的实例从函数的局部变量找到 *)
  List.iter
    (fun (f : Mir.func) ->
      visit f.ret;
      Array.iter (fun (l : Mir.local_info) -> visit l.lty) f.locals)
    fs;
  (* Vec 内建函数的辅助函数用到的类型（vec_pop 返回 Option<T>） *)
  List.iter
    (fun (_, (b, e)) ->
      visit (TVec e);
      if b = "pop" then visit (option g e))
    g.vec_calls;
  (* read_line、str_to_i64 返回的 Option<Str>、Option<I64>：辅助函数总是生成，类型也总要有 *)
  let opt_str = option g TStr and opt_i64 = option g TI64 in
  let opt_f64 = option g TF64 in
  visit opt_f64;
  (* 字符：char_from 返回的 Option<Char>，str_chars 返回的 Vec<Char>：辅助函数总是生成，类型也总要有 *)
  let opt_char = option g TChar in
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
  List.iter
    (fun (_, (b, e)) ->
      let fn = vec_fn d b e in
      if not (Hashtbl.mem emitted fn) then begin
        Hashtbl.add emitted fn ();
        let v = cty d (TVec e) and te = cty d e in
        Buffer.add_string helpers
          (match b with
          | "new" -> Printf.sprintf "SN_VEC_NEW(%s, %s)\n" fn v
          | "push" -> Printf.sprintf "SN_VEC_PUSH(%s, %s, %s)\n" fn v te
          | "pop" -> Printf.sprintf "SN_VEC_POP(%s, %s, %s)\n" fn v (cty d (option g e))
          | "swap" -> Printf.sprintf "SN_VEC_SWAP(%s, %s, %s)\n" fn v te
          | _ -> Printf.sprintf "SN_VEC_LEN(%s, %s)\n" fn v)
      end)
    g.vec_calls;
  (* runtime/sinter_lib.h（I/O、解析、std.os）用到的内建实例类型的 C 名字，然后原样放这一段 *)
  List.iter
    (fun (m, t) -> Buffer.add_string helpers (Printf.sprintf "#define %s %s\n" m (cty d t)))
    [
      ("SN_OPT_CHAR", opt_char); ("SN_VEC_CHAR", TVec TChar); ("SN_OPT_F64", opt_f64); ("SN_OPT_STR", opt_str);
      ("SN_OPT_I64", opt_i64); ("SN_VEC_STR", vstr); ("SN_RES_STR", rstr); ("SN_RES_UNIT", runit);
      ("SN_RES_BYTES", rbytes); ("SN_RES_VIEW", rview); ("SN_RES_VEC_STR", rvstr); ("SN_RES_I64", ri64);
    ];
  Buffer.add_string helpers Runtime_src.library;
  Buffer.contents fwd ^ Buffer.contents defs ^ Buffer.contents protos ^ Buffer.contents bodies
  ^ Buffer.contents helpers
