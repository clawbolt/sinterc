(* 类型检查：结构体按名字区分，没有泛型；`return` 之后的代码类型记为 Never。
   `Ok(e)` / `Err(e)` 的类型要靠上下文确定（let 标注、返回值、实参、字段），
   所以有两种检查方式：expr 自己推出类型，check 按期望类型检查。 *)
open Ast

exception Error of pos * string

let err pos fmt = Printf.ksprintf (fun m -> raise (Error (pos, m))) fmt

type t = Ty of ty | Never

type env = {
  structs : (string, field list) Hashtbl.t;
  fns : (string, (mode * ty) list * ty) Hashtbl.t;
  ret : ty;                          (* 当前函数的返回类型 *)
  vars : (string * (ty * bool)) list;
      (* 局部变量和它是否可修改（var、inout / sink 参数），内层在前 *)
  guards : string list;              (* 当前生效的 errdefer 要消费的变量 *)
}

(* 每个表达式的类型，按表达式 id 记录。 *)
let types : (int, t) Hashtbl.t = Hashtbl.create 256

(* 每条 errdefer（按调用表达式的 id）要消费的变量。 *)
let errdefer_targets : (int, string) Hashtbl.t = Hashtbl.create 16

(* 函数签名，给降级用：每个参数的约定和类型、返回类型。 *)
let sigs : (string, (mode * ty) list * ty) Hashtbl.t = Hashtbl.create 16

(* 结构体定义和标了 linear 的结构体，给降级和代码生成用。 *)
let struct_tbl : (string, field list) Hashtbl.t = Hashtbl.create 16
let linear_tbl : (string, unit) Hashtbl.t = Hashtbl.create 16

(* linear 值必须被显式消费，不能在作用域结束时自动析构。 *)
let rec is_linear = function
  | TStruct n -> Hashtbl.mem linear_tbl n
  | TResult (a, b) -> is_linear a || is_linear b
  | TBox t -> is_linear t
  | TI64 | TBool | TUnit -> false

(* 需要析构（或者是 linear）的类型：这些类型的值只能移动，不能复制。 *)
let rec needs_drop = function
  | TBox _ -> true
  | TStruct n -> (
      Hashtbl.mem linear_tbl n
      ||
      match Hashtbl.find_opt struct_tbl n with
      | Some fs -> List.exists (fun (f : field) -> needs_drop f.fty) fs
      | None -> false)
  | TResult (a, b) -> needs_drop a || needs_drop b
  | TI64 | TBool | TUnit -> false

(* 内建函数按值接收可复制的参数，记作 sink。 *)
let builtins =
  [ "print_i64", ([ (MSink, TI64) ], TUnit); "print_bool", ([ (MSink, TBool) ], TUnit) ]

(* 特殊处理的内建函数：参数类型不固定。 *)
let special = [ "box"; "consume" ]

(* 位置表达式（变量、字段、解引用）所在的变量；其他表达式没有。 *)
let rec root (x : expr) =
  match x.e with
  | Var v -> Some v
  | Field (a, _) | Deref a -> root a
  | _ -> None

let show = function Ty t -> string_of_ty t | Never -> "!"

(* 两个分支的类型合并：Never 和任何类型相容。 *)
let join pos what a b =
  match a, b with
  | Never, x | x, Never -> x
  | Ty x, Ty y when x = y -> Ty x
  | _ -> err pos "%s的两个分支类型不同：%s 和 %s" what (show a) (show b)

let expect pos want got =
  match got with
  | Never -> ()
  | Ty t when t = want -> ()
  | Ty t -> err pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty want) (string_of_ty t)

let rec check_ty env pos = function
  | TStruct n when not (Hashtbl.mem env.structs n) -> err pos "未知类型 %s" n
  | TBox TUnit -> err pos "Box 里不能放 ()"
  | TBox t ->
      if is_linear t then err pos "Box 里不能放 linear 值（还不能从 Box 里移出）";
      check_ty env pos t
  | TResult (a, b) -> check_ty env pos a; check_ty env pos b
  | _ -> ()

(* 每个表达式的类型按 id 记下来，给降级用。 *)
let rec expr env (x : expr) : t =
  let t = expr1 env x in
  Hashtbl.replace types x.id t;
  t

(* 按期望类型检查。 *)
and check env want (x : expr) : t =
  let t = check1 env want x in
  Hashtbl.replace types x.id t;
  t

and check1 env want (x : expr) : t =
  match x.e with
  | ResLit (ok, a) -> (
      match want with
      | TResult (t, e) ->
          ignore (check env (if ok then t else e) a);
          Ty want
      | _ ->
          err x.pos "这里需要 %s，%s(…) 是 Result" (string_of_ty want) (if ok then "Ok" else "Err"))
  | If (c, a, Some b) ->
      expect c.pos TBool (expr env c);
      expect x.pos want (block env ~want a);
      expect x.pos want (block env ~want b);
      Ty want
  | Block b -> block env ~want b
  | Match (s, arms) -> match_ env (Some want) x.pos s arms
  | _ ->
      let t = expr1 env x in
      expect x.pos want t;
      t

and expr1 env (x : expr) : t =
  let pos = x.pos in
  match x.e with
  | Int _ -> Ty TI64
  | Bool _ -> Ty TBool
  | Unit -> Ty TUnit
  | Var v -> (
      match List.assoc_opt v env.vars with
      | Some (t, _) -> Ty t
      | None -> err pos "未定义的变量 %s" v)
  | Binop (op, a, b) -> (
      let ta = expr env a in
      let tb = expr env b in
      match op with
      | Add | Sub | Mul | Div | Rem ->
          expect a.pos TI64 ta; expect b.pos TI64 tb; Ty TI64
      | Lt | Le | Gt | Ge ->
          expect a.pos TI64 ta; expect b.pos TI64 tb; Ty TBool
      | Eq | Ne -> (
          match ta, tb with
          | Ty (TStruct _ | TUnit | TBox _ | TResult _), _
          | _, Ty (TStruct _ | TUnit | TBox _ | TResult _) ->
              err pos "只能比较 I64 和 Bool"
          | _ -> ignore (join pos "比较" ta tb); Ty TBool)
      | And | Or -> expect a.pos TBool ta; expect b.pos TBool tb; Ty TBool)
  | Unop (Neg, a) -> expect a.pos TI64 (expr env a); Ty TI64
  | Unop (Not, a) -> expect a.pos TBool (expr env a); Ty TBool
  | Call ("box", [ a ]) -> (
      match expr env a with
      | Ty TUnit -> err pos "Box 里不能放 ()"
      | Ty t when is_linear t -> err pos "Box 里不能放 linear 值（还不能从 Box 里移出）"
      | Ty t -> Ty (TBox t)
      | Never -> Never)
  | Call ("consume", [ a ]) -> (
      match expr env a with
      | Ty (TStruct n) when Hashtbl.mem linear_tbl n -> Ty TUnit
      | Never -> Never
      | Ty t -> err pos "consume 只能用于 linear 结构体，%s 不是" (string_of_ty t))
  | Call ((("box" | "consume") as f), _) -> err pos "%s 需要 1 个参数" f
  | Deref a -> (
      match expr env a with
      | Ty (TBox t) -> Ty t
      | Never -> Never
      | Ty t -> err pos "%s 不是 Box，不能解引用" (string_of_ty t))
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
      List.iter2
        (fun (m, p) (a : expr) ->
          ignore (check env p a);
          if m = MInout then
            match root a with
            | None -> err a.pos "inout 实参必须是变量、字段或解引用这样的位置"
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err a.pos "%s 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）" v))
        ps args;
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
          | [ (_, (e : expr)) ] -> ignore (check env f.fty e)
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
      join pos "if " (block env a) (block env b)
  | Block b -> block env b
  | ResLit (ok, _) ->
      err pos "无法确定 %s(…) 的类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方"
        (if ok then "Ok" else "Err")
  | Try a -> (
      match expr env a with
      | Never -> Never
      | Ty (TResult (t, e)) -> (
          match env.ret with
          | TResult (_, e') when e = e' -> Ty t
          | TResult (_, e') ->
              err pos "? 传出的错误类型是 %s，但函数返回的错误类型是 %s" (string_of_ty e)
                (string_of_ty e')
          | r -> err pos "? 只能用在返回 Result 的函数里（这个函数返回 %s）" (string_of_ty r))
      | Ty t -> err pos "%s 不是 Result，不能用 ?" (string_of_ty t))
  | Match (s, arms) -> match_ env None pos s arms

and match_ env want pos (s : expr) arms : t =
  let t, e =
    match expr env s with
    | Ty (TResult (t, e)) -> (t, e)
    | Never -> err s.pos "不能 match 一个不会返回的值"
    | Ty t -> err s.pos "%s 不是 Result，不能 match" (string_of_ty t)
  in
  List.iter
    (fun (n, _, _) -> if n <> "Ok" && n <> "Err" then err pos "match 的分支只能是 Ok 和 Err，不能是 %s" n)
    arms;
  let find n =
    match List.filter (fun (m, _, _) -> m = n) arms with
    | [ a ] -> a
    | [] -> err pos "match 缺少 %s 分支" n
    | _ -> err pos "match 有两个 %s 分支" n
  in
  let bind (_, x, (a : expr)) ty =
    if ty = TUnit then begin
      if x <> "_" then err a.pos "这个分支的值是 ()，绑定名请写 _（而不是 %s）" x;
      env
    end
    else { env with vars = (x, (ty, false)) :: env.vars }
  in
  let ((_, _, ao) as a) = find "Ok" and ((_, _, ae) as b) = find "Err" in
  let ea = bind a t and eb = bind b e in
  match want with
  | Some w ->
      ignore (check ea w ao);
      ignore (check eb w ae);
      Ty w
  | None -> join pos "match " (expr ea ao) (expr eb ae)

(* want：块的值的期望类型（块最后的表达式按它检查）。 *)
and block ?want env (b : block) : t =
  let rec go env = function
    | [] -> (
        match b.tail, want with
        | Some e, Some w -> check env w e
        | Some e, None -> expr env e
        | None, _ -> Ty TUnit)
    | (s : stmt) :: rest -> (
        let pos = s.spos in
        match s.s with
        | Let (mut, v, ann, e) ->
            let ty =
              match ann with
              | Some a ->
                  check_ty env pos a;
                  ignore (check env a e);
                  a
              | None -> (
                  match expr env e with
                  | Ty t -> t
                  | Never -> err pos "%s 的初值不会返回，无法推断类型" v)
            in
            if ty = TUnit then err pos "C0 不支持 () 类型的变量 %s" v;
            go { env with vars = (v, (ty, mut)) :: env.vars } rest
        | Assign (v, e) ->
            (match List.assoc_opt v env.vars with
            | Some (t, true) -> ignore (check env t e)
            | Some (_, false) -> err pos "%s 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）" v
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
            ignore (check env env.ret e);
            ignore (go env rest);
            Never
        | Errdefer e ->
            let v = errdefer env pos e in
            if List.mem v env.guards then err pos "%s 已经有一个生效的 errdefer，不能再登记" v;
            go { env with guards = v :: env.guards } rest
        | Expr e ->
            let t = expr env e in
            let r = go env rest in
            if t = Never then Never else r)
  in
  go env b.stmts

(* errdefer f(…)：实参只能是变量、字段或常量，并且恰好以 sink 传入一个 linear 变量，
   出错返回时由它消费。返回这个变量。 *)
and errdefer env pos (e : expr) =
  match e.e with
  | Call (f, args) when f <> "box" ->
      ignore (expr env e);
      List.iter
        (fun (a : expr) ->
          match a.e with
          | Var _ | Field _ | Deref _ | Int _ | Bool _ | Unit -> ()
          | _ -> err a.pos "errdefer 的实参只能是变量、字段或常量")
        args;
      let modes = if f = "consume" then [ MSink ] else List.map fst (fst (Hashtbl.find sigs f)) in
      let targets =
        List.filter_map
          (fun (m, (a : expr)) ->
            match m, a.e with
            | MSink, Var v when is_linear (fst (List.assoc v env.vars)) -> Some v
            | _ -> None)
          (List.combine modes args)
      in
      (match targets with
      | [ v ] ->
          Hashtbl.replace errdefer_targets e.id v;
          v
      | _ -> err pos "errdefer 的调用必须以 sink 方式传入恰好一个 linear 变量（出错时由它消费）")
  | _ -> err pos "errdefer 后面必须是一个函数调用"

let program (p : program) =
  let structs = Hashtbl.create 16 and fns = Hashtbl.create 16 in
  Hashtbl.reset linear_tbl;
  List.iter
    (function
      | Struct { name; fields; linear; ipos } ->
          if Hashtbl.mem structs name then err ipos "结构体 %s 重复定义" name;
          Hashtbl.add structs name fields;
          if linear then Hashtbl.replace linear_tbl name ()
      | Fn { name; params; ret; ipos; _ } ->
          if Hashtbl.mem fns name || List.mem_assoc name builtins || List.mem name special then
            err ipos "函数 %s 重复定义" name;
          Hashtbl.add fns name (List.map (fun (m, _, t) -> (m, t)) params, ret))
    p;
  Hashtbl.reset sigs;
  Hashtbl.iter (Hashtbl.replace sigs) fns;
  List.iter (fun (f, s) -> Hashtbl.replace sigs f s) builtins;
  (* 结构体不能包含自己，经过 Box 也不行（还没有 Option，这样的值造不出来）。 *)
  let rec visit path n =
    if List.mem n path then
      err { line = 1; col = 1 } "结构体 %s 包含自己（还不支持递归类型）" n;
    match Hashtbl.find_opt structs n with
    | Some fs ->
        let rec inner = function
          | TStruct m -> visit (n :: path) m
          | TBox t -> inner t
          | TResult (a, b) -> inner a; inner b
          | _ -> ()
        in
        List.iter (fun (f : field) -> inner f.fty) fs
    | None -> ()
  in
  Hashtbl.iter (fun n _ -> visit [] n) structs;
  Hashtbl.reset struct_tbl;
  Hashtbl.iter (Hashtbl.replace struct_tbl) structs;
  let env0 = { structs; fns; ret = TUnit; vars = []; guards = [] } in
  List.iter
    (function
      | Struct { name; fields; linear; ipos } ->
          List.iter
            (fun (f : field) ->
              check_ty env0 ipos f.fty;
              if f.fty = TUnit then err ipos "C0 不支持 () 类型的字段 %s" f.fname;
              if is_linear f.fty && not linear then
                err ipos "%s 的字段 %s 是 linear 的，%s 也必须标 linear" name f.fname name)
            fields
      | Fn { params; ret; body; ipos; _ } ->
          List.iter
            (fun (_, x, t) ->
              check_ty env0 ipos t;
              if t = TUnit then err ipos "C0 不支持 () 类型的参数 %s" x)
            params;
          check_ty env0 ipos ret;
          let vars = List.rev_map (fun (m, x, t) -> (x, (t, m <> MLet))) params in
          let env = { env0 with ret; vars } in
          expect ipos ret (block env ~want:ret body))
    p;
  match Hashtbl.find_opt fns "main" with
  | Some ([], TUnit) -> ()
  | Some _ -> err { line = 1; col = 1 } "main 必须是 fn main()，没有参数、没有返回值"
  | None -> err { line = 1; col = 1 } "缺少 fn main()"
