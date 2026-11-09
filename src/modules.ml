(* 模块（C34）：一个文件一个模块。use m 引入和当前文件同目录的 m.sn，之后用 m.f(…)、m.T 访问它的 pub 条目；
   use m { a, B } 另外把这几个名字直接引入。所有模块读进来之后合成一个程序交给类型检查：
   模块 m 里的函数、结构体、枚举、trait 加上前缀，名字变成 m.f、m.T（主文件不加前缀）。
   trait 的方法名不加前缀（方法跟着 trait 走，和 impl 一样是全局的）。
   改名只是名字的事，不改变任何检查规则，所以不需要新的 Lean 里程碑。
   consume 只能在定义这个 linear 类型的文件里用（见 typeck.ml 的 consume），linear 类型的封装由此成立。
   标准库（C35）：use std.m 引入编进编译器的 std/m.sn（见 stdlib_src.ml，由 std/*.sn 生成），之后照样写 m.f(…)。
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

(* 读一个文件（text 给出时读标准库里编进来的源码）：第 index 个文件的行号从 index * line_base + 1 开始。 *)
let parse_file ?text path index =
  Lexer.depth := 0;
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
    try Parser.program Lexer.token lexbuf with
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
let rename (mods : (string, modinfo) Hashtbl.t) (me : modinfo) : item list =
  let imported = List.map (fun (m, _, _) -> alias m) me.src.uses in
  (* 直接可见的名字 → 合并后的名字 *)
  let names : (string, string) Hashtbl.t = Hashtbl.create 16 in
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
    | (TI64 | TBool | TStr | TUnit | TArena _ | TParam _) as t -> t
  in
  let binder pos x =
    if List.mem x imported then err pos "变量 %s 和引入的模块同名，换一个名字" x
  in
  let rec ex (x : expr) =
    let pos = x.pos in
    let e =
      match x.e with
      | (Int _ | Bool _ | Unit | Str _ | Var _) as d -> d
      | Binop (o, a, b) -> Binop (o, ex a, ex b)
      | Unop (o, a) -> Unop (o, ex a)
      | Call (f, { e = Var m; _ } :: args) when f <> "" && f.[0] = '.' && List.mem m imported ->
          (* m.f(…)：模块 m 的函数 *)
          Call (target pos m (String.sub f 1 (String.length f - 1)), List.map ex args)
      | Call (f, args) when f <> "" && f.[0] = '.' -> (
          (* 方法调用 r.f(…)：f 是本文件的函数或引入的名字时换成合并后的名字，其他的（trait 方法、内建）不变 *)
          let g = String.sub f 1 (String.length f - 1) in
          match Hashtbl.find_opt names g with
          | Some q -> Call ("." ^ q, List.map ex args)
          | None -> Call (f, List.map ex args))
      | Call (f, args) -> Call (qual pos f, List.map ex args)
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
                  { m with mparams = List.map (fun (md, x, ty') -> (md, x, ty m.mpos ty')) m.mparams; mret = ty m.mpos m.mret })
                t.methods;
          }
    | Impl i ->
        Impl { i with trait = qual i.ipos i.trait; self = ty i.ipos i.self; fns = List.map (item ~top:false) i.fns }
  in
  List.map (fun (_, it) -> item ~top:true it) me.src.items

(* 从主文件出发读进所有用到的模块，改名后合成一个程序。 *)
let load (main : string) : program =
  Hashtbl.reset type_file;
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
        let mi = { modname = name; index; src; pubs; defs } in
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
                  err upos "标准库里没有模块 %s（有：%s）" a (String.concat "、" (List.map fst Stdlib_src.files))
            end
            else visit m (if dir = Filename.current_dir_name then m ^ ".sn" else Filename.concat dir (m ^ ".sn")) None upos)
          src.uses;
        order := mi :: !order
  in
  visit "" main None { line = 1; col = 1 };
  (* 被引入的模块排在前面，主文件最后 *)
  List.concat_map (rename mods) (List.rev !order)
