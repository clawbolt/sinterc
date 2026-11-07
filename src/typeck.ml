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

(* 枚举定义：变体按声明顺序，下标就是运行时的标签（Lean M8）。 *)
let enum_tbl : (string, (string * ty option) list) Hashtbl.t = Hashtbl.create 16

(* 变体的下标和载荷类型。 *)
let variant n v =
  let rec go i = function
    | [] -> None
    | (w, t) :: rest -> if w = v then Some (i, t) else go (i + 1) rest
  in
  go 0 (Hashtbl.find enum_tbl n)

(* linear 值必须被显式消费，不能在作用域结束时自动析构。 *)
let rec is_linear = function
  | TStruct n -> Hashtbl.mem linear_tbl n
  | TResult (a, b) -> is_linear a || is_linear b
  | TBox t -> is_linear t
  | TI64 | TBool | TUnit | TRef _ | TArena _ -> false

(* 需要析构（或者是 linear）的类型：这些类型的值只能移动，不能复制。 *)
let rec needs_drop = function
  | TBox _ -> true
  | TStruct n -> (
      Hashtbl.mem linear_tbl n
      ||
      match Hashtbl.find_opt struct_tbl n with
      | Some fs -> List.exists (fun (f : field) -> needs_drop f.fty) fs
      | None -> (
          (* 枚举：某个变体的载荷需要析构 *)
          match Hashtbl.find_opt enum_tbl n with
          | Some vs -> List.exists (function _, Some t -> needs_drop t | _, None -> false) vs
          | None -> false))
  | TResult (a, b) -> needs_drop a || needs_drop b
  | TI64 | TBool | TUnit | TRef _ | TArena _ -> false

(* 内建函数按值接收可复制的参数，记作 sink。 *)
let builtins =
  [
    "print_i64", ([ (MSink, TI64) ], TUnit);
    "print_bool", ([ (MSink, TBool) ], TUnit);
    "choose", ([], TBool);       (* 对照测试用：按运行时给定的选择序列返回真假 *)
  ]

(* 特殊处理的内建函数：参数类型不固定。 *)
let special = [ "box"; "consume"; "alloc" ]

(* 位置表达式（变量、字段、解引用）所在的变量；其他表达式没有。 *)
let rec root (x : expr) =
  match x.e with
  | Var v -> Some v
  | Field (a, _) | Deref a -> root a
  | _ -> None

let show = function Ty t -> string_of_ty t | Never -> "!"

(* ---------- 区域（Lean M5、M7（四）） ----------
   当前可见的区域，内层在前；每个区域记下比它活得久的区域。
   函数的区域参数（a: Arena，以及参数类型里出现的其他区域名）之间没有顺序（D3）；
   arena 块引入的区域比所有外层区域和区域参数都活得短。 *)
let regions : (string * string list) list ref = ref []

let outlives x y = x = y || match List.assoc_opt y !regions with Some a -> List.mem x a | None -> false

(* got 能不能当作 want 用：外层区域的引用可以当作内层区域的引用（D2 协变），其余要求相同。 *)
let rec sub got want =
  match got, want with
  | TRef (t, x), TRef (t', y) -> t = t' && outlives x y
  | TResult (a, b), TResult (c, d) -> sub a c && sub b d
  | _ -> got = want

let rec mentions r = function
  | TRef (t, x) -> x = r || mentions r t
  | TArena x -> x = r
  | TBox t -> mentions r t
  | TResult (a, b) -> mentions r a || mentions r b
  | TI64 | TBool | TUnit | TStruct _ -> false

let rec region_names = function
  | TRef (t, x) -> x :: region_names t
  | TArena x -> [ x ]
  | TBox t -> region_names t
  | TResult (a, b) -> region_names a @ region_names b
  | TI64 | TBool | TUnit | TStruct _ -> []

let rec rename s = function
  | TRef (t, x) -> TRef (rename s t, Option.value (List.assoc_opt x s) ~default:x)
  | TArena x -> TArena (Option.value (List.assoc_opt x s) ~default:x)
  | TBox t -> TBox (rename s t)
  | TResult (a, b) -> TResult (rename s a, rename s b)
  | t -> t

(* 两个分支的类型合并：Never 和任何类型相容；区域不同时取活得短的那个。 *)
let join pos what a b =
  match a, b with
  | Never, x | x, Never -> x
  | Ty x, Ty y when sub x y -> Ty y
  | Ty x, Ty y when sub y x -> Ty x
  | _ -> err pos "%s的两个分支类型不同：%s 和 %s" what (show a) (show b)

let expect pos want got =
  match got with
  | Never -> ()
  | Ty t when sub t want -> ()
  | Ty t -> err pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty want) (string_of_ty t)

(* param：函数参数的类型，可以是 Arena。 *)
let rec check_ty ?(param = false) env pos = function
  | TStruct n when not (Hashtbl.mem env.structs n || Hashtbl.mem enum_tbl n) -> err pos "未知类型 %s" n
  | TBox TUnit -> err pos "Box 里不能放 ()"
  | TBox t ->
      if is_linear t then err pos "Box 里不能放 linear 值（还不能从 Box 里移出）";
      check_ty env pos t
  | TResult (a, b) -> check_ty env pos a; check_ty env pos b
  | TArena _ when not param -> err pos "Arena 只能作为函数参数的类型"
  | TRef (t, r) ->
      if not (List.mem_assoc r !regions) then err pos "未知区域 %s" r;
      if region_names t <> [] then err pos "还不支持指向引用或区域的引用";
      if t = TUnit then err pos "不能引用 ()";
      check_ty env pos t
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
  | Variant (v, a) -> (
      match want with
      | TStruct n when Hashtbl.mem enum_tbl n -> (
          match variant n v, a with
          | None, _ -> err x.pos "枚举 %s 没有变体 %s" n v
          | Some (_, Some t), Some a ->
              ignore (check env t a);
              Ty want
          | Some (_, None), None -> Ty want
          | Some (_, Some t), None -> err x.pos ".%s 带一个 %s 类型的载荷，要写 .%s(…)" v (string_of_ty t) v
          | Some (_, None), Some _ -> err x.pos ".%s 没有载荷，不能写 .%s(…)" v v)
      | _ -> err x.pos "这里需要 %s，.%s 是枚举的变体" (string_of_ty want) v)
  | If (c, a, Some b) ->
      expect c.pos TBool (expr env c);
      expect x.pos want (block env ~want a);
      expect x.pos want (block env ~want b);
      Ty want
  | Block b -> block env ~want b
  | Match (b, s, arms) -> match_ env (Some want) x.pos b s arms
  | Arena (r, b) -> arena env (Some want) x.pos r b
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
          | Ty (TStruct _ | TUnit | TBox _ | TResult _ | TRef _ | TArena _), _
          | _, Ty (TStruct _ | TUnit | TBox _ | TResult _ | TRef _ | TArena _) ->
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
  | Call ("alloc", [ a; v ]) -> (
      match expr env a with
      | Ty (TArena r) -> (
          match expr env v with
          | Never -> Never
          | Ty TUnit -> err v.pos "区域里不能放 ()"
          | Ty t when needs_drop t ->
              err pos "区域里不能放需要析构或 linear 的值 %s：区域结束时不运行析构（D4）" (string_of_ty t)
          | Ty t when region_names t <> [] -> err v.pos "还不支持在区域里存放引用"
          | Ty t -> Ty (TRef (t, r)))
      | Never -> Never
      | Ty t -> err a.pos "alloc 的第一个参数必须是区域，%s 不是" (string_of_ty t))
  | Call ("alloc", _) -> err pos "alloc 需要 2 个参数：区域和值"
  | Deref a -> (
      match expr env a with
      | Ty (TBox t | TRef (t, _)) -> Ty t
      | Never -> Never
      | Ty t -> err pos "%s 不是 Box 或引用，不能解引用" (string_of_ty t))
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
      (* 区域参数按实参实例化（M5（四））：Arena 参数取实参的区域，
         参数类型里的其他区域名取实参类型里对应位置的区域 *)
      let s = ref [] in
      let rec bind p t =
        match p, t with
        | (TRef (p, x), TRef (t, y)) ->
            if not (List.mem_assoc x !s) then s := (x, y) :: !s;
            bind p t
        | TArena x, TArena y -> if not (List.mem_assoc x !s) then s := (x, y) :: !s
        | TBox p, TBox t -> bind p t
        | TResult (a, b), TResult (c, d) -> bind a c; bind b d
        | _ -> ()
      in
      List.iter2
        (fun (_, p) (a : expr) ->
          if region_names p <> [] then match expr env a with Ty t -> bind p t | Never -> ())
        ps args;
      let s = !s in
      List.iter2
        (fun (m, p) (a : expr) ->
          ignore (check env (rename s p) a);
          if m = MInout then
            match root a with
            | None -> err a.pos "inout 实参必须是变量、字段或解引用这样的位置"
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err a.pos "%s 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）" v))
        ps args;
      Ty (rename s r)
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
      | Ty (TStruct n) when Hashtbl.mem enum_tbl n -> err pos "%s 是枚举，不能取字段 %s（用 match）" n f
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
  | Variant (v, _) ->
      err pos "无法确定 .%s 的类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方" v
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
  | Match (b, s, arms) -> match_ env None pos b s arms
  | Arena (r, b) -> arena env None pos r b

(* arena |r| { … }：r 比所有外层区域都活得短。块的值的类型不能含 r（R3）。 *)
and arena env want pos r (b : block) : t =
  if List.mem_assoc r !regions then err pos "区域 %s 已经存在（内外层区域不能同名）" r;
  let saved = !regions in
  regions := (r, List.map fst saved) :: saved;
  let env' = { env with vars = (r, (TArena r, false)) :: env.vars } in
  let t = match want with Some w -> block env' ~want:w b | None -> block env' b in
  regions := saved;
  (match t with
  | Ty ty when mentions r ty ->
      let at = match b.tail with Some e -> e.pos | None -> pos in
      err at "块的值的类型 %s 含有区域 %s，不能带出这个区域（R3）" (string_of_ty ty) r
  | _ -> ());
  t

(* borrow：match let s { … }，只用于枚举，s 必须是位置（Lean M8（二））。 *)
and match_ env want pos borrow (s : expr) arms : t =
  match expr env s with
  | Ty (TStruct n) when Hashtbl.mem enum_tbl n ->
      if borrow && root s = None then
        err s.pos "match let 后面必须是变量、字段或解引用（要借用的位置）";
      enum_match env want pos n arms
  | Ty (TResult _) when borrow -> err pos "match let 只用于枚举"
  | Ty (TResult (t, e)) -> result_match env want pos t e arms
  | Never -> err s.pos "不能 match 一个不会返回的值"
  | Ty t -> err s.pos "%s 不是 Result 或枚举，不能 match" (string_of_ty t)

and result_match env want pos t e arms : t =
  List.iter
    (fun (n, _, _) -> if n <> "Ok" && n <> "Err" then err pos "match 的分支只能是 Ok 和 Err，不能是 %s" n)
    arms;
  let find n =
    match List.filter (fun (m, _, _) -> m = n) arms with
    | [ a ] -> a
    | [] -> err pos "match 缺少 %s 分支" n
    | _ -> err pos "match 有两个 %s 分支" n
  in
  let bind (n, x, (a : expr)) ty =
    if x = "" then err a.pos "%s 分支要写 %s(x)" n n;
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

(* 枚举的 match（Lean M8（一））：必须穷尽，每个变体恰好一个分支；带载荷的变体绑定载荷。 *)
and enum_match env want pos n arms : t =
  let vs = Hashtbl.find enum_tbl n in
  List.iter
    (fun (v, _, (a : expr)) -> if not (List.mem_assoc v vs) then err a.pos "枚举 %s 没有变体 %s" n v)
    arms;
  let bodies =
    List.map
      (fun (v, pty) ->
        match List.filter (fun (w, _, _) -> w = v) arms with
        | [ (_, x, (a : expr)) ] -> (
            match pty with
            | Some t ->
                if x = "" then err a.pos ".%s 带载荷，分支要写 .%s(x) 或 .%s(_)" v v v;
                ({ env with vars = (x, (t, false)) :: env.vars }, a)
            | None ->
                if x <> "" then err a.pos ".%s 没有载荷，分支要写 .%s => …" v v;
                (env, a))
        | [] -> err pos "match 没有覆盖 .%s（枚举的 match 必须穷尽）" v
        | _ -> err pos "match 有两个 .%s 分支" v)
      vs
  in
  match want, bodies with
  | Some w, _ ->
      List.iter (fun (env', a) -> ignore (check env' w a)) bodies;
      Ty w
  | None, (env', a) :: rest ->
      List.fold_left (fun acc (env', a) -> join pos "match " acc (expr env' a)) (expr env' a) rest
  | None, [] -> err pos "空枚举不能 match"

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
        | Decl (v, ty) ->
            check_ty env pos ty;
            (match ty with
            | TUnit -> err pos "C0 不支持 () 类型的变量 %s" v
            | TArena _ -> err pos "区域不能存进变量"
            | _ -> ());
            go { env with vars = (v, (ty, true)) :: env.vars } rest
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
            (match ty with TArena _ -> err pos "区域不能存进变量" | _ -> ());
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

(* 整个程序的准备：结构体、枚举、函数签名、字段和载荷。返回检查函数体用的环境。 *)
let prepare (p : program) =
  let structs = Hashtbl.create 16 and fns = Hashtbl.create 16 in
  Hashtbl.reset linear_tbl;
  Hashtbl.reset enum_tbl;
  List.iter
    (function
      | Struct { name; fields; linear; ipos } ->
          if Hashtbl.mem structs name || Hashtbl.mem enum_tbl name then err ipos "类型 %s 重复定义" name;
          Hashtbl.add structs name fields;
          if linear then Hashtbl.replace linear_tbl name ()
      | Enum { name; variants; ipos } ->
          if Hashtbl.mem structs name || Hashtbl.mem enum_tbl name then err ipos "类型 %s 重复定义" name;
          if variants = [] then err ipos "枚举 %s 至少要有一个变体" name;
          List.iteri
            (fun i (v, _) ->
              if List.exists (fun (w, _) -> w = v) (List.filteri (fun j _ -> j < i) variants) then
                err ipos "枚举 %s 的变体 %s 重复" name v)
            variants;
          Hashtbl.add enum_tbl name variants
      | Fn { name; params; ret; ipos; _ } ->
          if Hashtbl.mem fns name || List.mem_assoc name builtins || List.mem name special then
            err ipos "函数 %s 重复定义" name;
          Hashtbl.add fns name (List.map (fun (m, _, t) -> (m, t)) params, ret))
    p;
  Hashtbl.reset sigs;
  Hashtbl.iter (Hashtbl.replace sigs) fns;
  List.iter (fun (f, s) -> Hashtbl.replace sigs f s) builtins;
  (* 结构体、枚举不能包含自己，经过 Box 也不行（递归类型的析构还没有证明）。 *)
  let rec visit path n =
    if List.mem n path then
      err { line = 1; col = 1 } "类型 %s 包含自己（还不支持递归类型）" n;
    let rec inner = function
      | TStruct m -> visit (n :: path) m
      | TBox t -> inner t
      | TResult (a, b) -> inner a; inner b
      | _ -> ()
    in
    match Hashtbl.find_opt structs n with
    | Some fs -> List.iter (fun (f : field) -> inner f.fty) fs
    | None -> (
        match Hashtbl.find_opt enum_tbl n with
        | Some vs -> List.iter (fun (_, t) -> Option.iter inner t) vs
        | None -> ())
  in
  Hashtbl.iter (fun n _ -> visit [] n) structs;
  Hashtbl.iter (fun n _ -> visit [] n) enum_tbl;
  Hashtbl.reset struct_tbl;
  Hashtbl.iter (Hashtbl.replace struct_tbl) structs;
  let env0 = { structs; fns; ret = TUnit; vars = []; guards = [] } in
  List.iter
    (function
      | Struct { name; fields; linear; ipos } ->
          List.iter
            (fun (f : field) ->
              if region_names f.fty <> [] then
                err ipos "结构体字段 %s 不能含引用或区域（区域类型还没有实现）" f.fname;
              check_ty env0 ipos f.fty;
              if f.fty = TUnit then err ipos "C0 不支持 () 类型的字段 %s" f.fname;
              if is_linear f.fty && not linear then
                err ipos "%s 的字段 %s 是 linear 的，%s 也必须标 linear" name f.fname name)
            fields
      | Enum { name; variants; ipos } ->
          List.iter
            (fun (v, t) ->
              match t with
              | None -> ()
              | Some t ->
                  if region_names t <> [] then
                    err ipos "%s 的变体 %s 的载荷不能含引用或区域（区域类型还没有实现）" name v;
                  check_ty env0 ipos t;
                  if t = TUnit then err ipos "%s 的变体 %s：没有载荷请直接写 %s" name v v;
                  if is_linear t then err ipos "%s 的变体 %s：载荷暂不能是 linear 值" name v)
            variants
      | Fn _ -> ())
    p;
  env0

(* 检查一个函数。 *)
let check_fn env0 = function
  | Fn { params; ret; body; ipos; _ } ->
      (* 区域参数：Arena 参数和参数类型里出现的区域名，彼此之间没有顺序（D3） *)
      regions :=
        List.sort_uniq compare (List.concat_map (fun (_, _, t) -> region_names t) params)
        |> List.map (fun r -> (r, []));
      List.iter
        (fun (m, x, t) ->
          check_ty ~param:true env0 ipos t;
          (match t with
          | TArena _ when m <> MLet -> err ipos "区域参数 %s 只能按 let 传入" x
          | _ -> ());
          if t = TUnit then err ipos "C0 不支持 () 类型的参数 %s" x)
        params;
      check_ty env0 ipos ret;
      let vars = List.rev_map (fun (m, x, t) -> (x, (t, m <> MLet))) params in
      let env = { env0 with ret; vars } in
      expect ipos ret (block env ~want:ret body);
      regions := []
  | Struct _ | Enum _ -> ()

let check_main env0 =
  match Hashtbl.find_opt env0.fns "main" with
  | Some ([], TUnit) -> ()
  | Some _ -> err { line = 1; col = 1 } "main 必须是 fn main()，没有参数、没有返回值"
  | None -> err { line = 1; col = 1 } "缺少 fn main()"

let program (p : program) =
  let env0 = prepare p in
  List.iter (check_fn env0) p;
  check_main env0
