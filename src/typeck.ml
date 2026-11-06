(* C0 的类型检查：结构体按名字区分，没有泛型；`return` 之后的代码类型记为 Never。 *)
open Ast

exception Error of pos * string

let err pos fmt = Printf.ksprintf (fun m -> raise (Error (pos, m))) fmt

type t = Ty of ty | Never

type env = {
  structs : (string, field list) Hashtbl.t;
  fns : (string, ty list * ty) Hashtbl.t;
  ret : ty;                          (* 当前函数的返回类型 *)
  vars : (string * ty) list;         (* 局部变量，内层在前 *)
}

(* if-else 和块表达式的类型，按位置记录，给代码生成用。 *)
let types : (pos, t) Hashtbl.t = Hashtbl.create 64

let builtins = [ "print_i64", ([ TI64 ], TUnit); "print_bool", ([ TBool ], TUnit) ]

let show = function Ty t -> string_of_ty t | Never -> "!"

(* 两个分支的类型合并：Never 和任何类型相容。 *)
let join pos a b =
  match a, b with
  | Never, x | x, Never -> x
  | Ty x, Ty y when x = y -> Ty x
  | _ -> err pos "if 的两个分支类型不同：%s 和 %s" (show a) (show b)

let expect pos want got =
  match got with
  | Never -> ()
  | Ty t when t = want -> ()
  | Ty t -> err pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty want) (string_of_ty t)

let check_ty env pos = function
  | TStruct n when not (Hashtbl.mem env.structs n) -> err pos "未知类型 %s" n
  | _ -> ()

let rec expr env (x : expr) : t =
  let pos = x.pos in
  match x.e with
  | Int _ -> Ty TI64
  | Bool _ -> Ty TBool
  | Unit -> Ty TUnit
  | Var v -> (
      match List.assoc_opt v env.vars with
      | Some t -> Ty t
      | None -> err pos "未定义的变量 %s" v)
  | Binop (op, a, b) -> (
      let ta = expr env a and tb = expr env b in
      match op with
      | Add | Sub | Mul | Div | Rem ->
          expect a.pos TI64 ta; expect b.pos TI64 tb; Ty TI64
      | Lt | Le | Gt | Ge ->
          expect a.pos TI64 ta; expect b.pos TI64 tb; Ty TBool
      | Eq | Ne -> (
          match ta, tb with
          | Ty (TStruct _ | TUnit), _ | _, Ty (TStruct _ | TUnit) ->
              err pos "C0 只能比较 I64 和 Bool"
          | _ -> ignore (join pos ta tb); Ty TBool)
      | And | Or -> expect a.pos TBool ta; expect b.pos TBool tb; Ty TBool)
  | Unop (Neg, a) -> expect a.pos TI64 (expr env a); Ty TI64
  | Unop (Not, a) -> expect a.pos TBool (expr env a); Ty TBool
  | Call (f, args) ->
      let ps, r =
        match Hashtbl.find_opt env.fns f with
        | Some s -> s
        | None -> (
            match List.assoc_opt f builtins with
            | Some s -> s
            | None -> err pos "未定义的函数 %s" f)
      in
      if List.length ps <> List.length args then
        err pos "%s 需要 %d 个参数，实际给了 %d 个" f (List.length ps) (List.length args);
      List.iter2 (fun p (a : expr) -> expect a.pos p (expr env a)) ps args;
      Ty r
  | StructLit (n, inits) ->
      let fs =
        match Hashtbl.find_opt env.structs n with
        | Some fs -> fs
        | None -> err pos "未知结构体 %s" n
      in
      List.iter
        (fun (f : field) ->
          match List.filter (fun (g, _) -> g = f.fname) inits with
          | [ (_, (e : expr)) ] -> expect e.pos f.fty (expr env e)
          | [] -> err pos "%s 缺少字段 %s" n f.fname
          | _ -> err pos "字段 %s 重复" f.fname)
        fs;
      List.iter
        (fun (g, _) ->
          if not (List.exists (fun (f : field) -> f.fname = g) fs) then
            err pos "%s 没有字段 %s" n g)
        inits;
      Ty (TStruct n)
  | Field (a, f) -> (
      match expr env a with
      | Ty (TStruct n) -> (
          let fs = Hashtbl.find env.structs n in
          match List.find_opt (fun (g : field) -> g.fname = f) fs with
          | Some g -> Ty g.fty
          | None -> err pos "%s 没有字段 %s" n f)
      | Never -> Never
      | Ty t -> err pos "%s 不是结构体，不能取字段 %s" (string_of_ty t) f)
  | If (c, a, None) ->
      expect c.pos TBool (expr env c);
      expect pos TUnit (block env a);
      Ty TUnit
  | If (c, a, Some b) ->
      expect c.pos TBool (expr env c);
      let r = join pos (block env a) (block env b) in
      Hashtbl.replace types pos r;
      r
  | Block b ->
      let r = block env b in
      Hashtbl.replace types pos r;
      r

and block env (b : block) : t =
  let rec go env = function
    | [] -> (
        match b.tail with
        | Some e -> expr env e
        | None -> Ty TUnit)
    | (s : stmt) :: rest -> (
        let pos = s.spos in
        match s.s with
        | Let (v, ann, e) ->
            let t = expr env e in
            let ty =
              match ann, t with
              | Some a, _ -> check_ty env pos a; expect e.pos a t; a
              | None, Ty t -> t
              | None, Never -> err pos "%s 的初值不会返回，无法推断类型" v
            in
            if ty = TUnit then err pos "C0 不支持 () 类型的变量 %s" v;
            go { env with vars = (v, ty) :: env.vars } rest
        | Assign (v, e) ->
            (match List.assoc_opt v env.vars with
            | Some t -> expect e.pos t (expr env e)
            | None -> err pos "未定义的变量 %s" v);
            go env rest
        | While (c, body) ->
            expect c.pos TBool (expr env c);
            expect pos TUnit (block env body);
            go env rest
        | Return None ->
            expect pos env.ret (Ty TUnit);
            ignore (go env rest);
            Never
        | Return (Some e) ->
            expect e.pos env.ret (expr env e);
            ignore (go env rest);
            Never
        | Expr e ->
            let t = expr env e in
            let r = go env rest in
            if t = Never then Never else r)
  in
  go env b.stmts

let program (p : program) =
  let structs = Hashtbl.create 16 and fns = Hashtbl.create 16 in
  List.iter
    (function
      | Struct { name; fields; ipos } ->
          if Hashtbl.mem structs name then err ipos "结构体 %s 重复定义" name;
          Hashtbl.add structs name fields
      | Fn { name; params; ret; ipos; _ } ->
          if Hashtbl.mem fns name || List.mem_assoc name builtins then
            err ipos "函数 %s 重复定义" name;
          Hashtbl.add fns name (List.map snd params, ret))
    p;
  (* 结构体按值包含结构体，不能成环。 *)
  let rec visit path n =
    if List.mem n path then
      err { line = 1; col = 1 } "结构体 %s 按值包含自己" n;
    match Hashtbl.find_opt structs n with
    | Some fs ->
        List.iter (fun (f : field) ->
            match f.fty with TStruct m -> visit (n :: path) m | _ -> ()) fs
    | None -> ()
  in
  Hashtbl.iter (fun n _ -> visit [] n) structs;
  let env0 = { structs; fns; ret = TUnit; vars = [] } in
  List.iter
    (function
      | Struct { fields; ipos; _ } ->
          List.iter
            (fun (f : field) ->
              check_ty env0 ipos f.fty;
              if f.fty = TUnit then err ipos "C0 不支持 () 类型的字段 %s" f.fname)
            fields
      | Fn { params; ret; body; ipos; _ } ->
          List.iter
            (fun (x, t) ->
              check_ty env0 ipos t;
              if t = TUnit then err ipos "C0 不支持 () 类型的参数 %s" x)
            params;
          check_ty env0 ipos ret;
          let env = { env0 with ret; vars = List.rev params } in
          expect ipos ret (block env body))
    p;
  match Hashtbl.find_opt fns "main" with
  | Some ([], TUnit) -> ()
  | Some _ -> err { line = 1; col = 1 } "main 必须是 fn main()，没有参数、没有返回值"
  | None -> err { line = 1; col = 1 } "缺少 fn main()"
