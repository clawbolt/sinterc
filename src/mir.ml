(* MIR：每个函数一张控制流图。基本块里是简单的赋值语句，块尾是跳转。
   降级时操作数一律从左到右求值；`&&`、`||`、`if`、`while` 都变成分支。
   C1b 会在这一层做初始化 / 移动 / drop flag 的数据流检查。 *)
open Ast

type local = int

(* 位置：一个局部变量加上字段路径。 *)
type place = { base : local; proj : string list }

type const = CInt of int64 | CBool of bool

type operand = Const of const | Copy of place

type rvalue =
  | Use of operand
  | Bin of binop * operand * operand      (* 不含 And / Or：它们降级成分支 *)
  | Un of unop * operand
  | Call of string * operand list
  | Agg of string * (string * operand) list   (* 结构体字面量 *)

type stmt =
  | Assign of place * rvalue
  | Eval of rvalue                        (* 只为副作用，例如返回 () 的调用 *)

type term =
  | Goto of int
  | Branch of operand * int * int
  | Return
  | Unterminated                          (* 降级过程中的临时状态 *)

type block = { mutable stmts : stmt list; mutable term : term }   (* stmts 倒序存放 *)

type local_info = { lname : string; lty : ty }

type func = {
  name : string;
  params : local list;
  ret : ty;
  ret_local : local option;      (* 返回值放在这里；返回 () 时没有 *)
  locals : local_info array;
  blocks : block array;          (* 0 号块是入口 *)
}

(* ---------- 降级 ---------- *)

type st = {
  mutable locals : local_info list;   (* 倒序 *)
  mutable nlocals : int;
  mutable blocks : block list;        (* 倒序 *)
  mutable nblocks : int;
  mutable cur : int;
  mutable fret : local option;
}

let new_local st lname lty =
  st.locals <- { lname; lty } :: st.locals;
  st.nlocals <- st.nlocals + 1;
  st.nlocals - 1

let new_block st =
  st.blocks <- { stmts = []; term = Unterminated } :: st.blocks;
  st.nblocks <- st.nblocks + 1;
  st.nblocks - 1

let block_of st i = List.nth st.blocks (st.nblocks - 1 - i)

let emit st s =
  let b = block_of st st.cur in
  b.stmts <- s :: b.stmts

(* 结束当前块。如果它已经结束（例如 return 之后的死代码），保持原样。 *)
let terminate st t =
  let b = block_of st st.cur in
  if b.term = Unterminated then b.term <- t

let ty_of (x : expr) =
  match Hashtbl.find_opt Typeck.types x.id with
  | Some (Typeck.Ty t) -> Some t
  | _ -> None          (* Never：这个值不会被用到 *)

let whole l = { base = l; proj = [] }

(* 不会被用到的值（Never 类型的表达式）用一个占位常量代替；它只出现在走不到的块里。 *)
let dummy = Some (Const (CInt 0L))

let temp st ty = new_local st "" ty

let rec expr st env (x : expr) : operand option =
  match x.e with
  | Int n -> Some (Const (CInt n))
  | Bool b -> Some (Const (CBool b))
  | Unit -> None
  | Var v -> Some (Copy (whole (List.assoc v env)))
  | Field (a, f) -> (
      match expr st env a with
      | Some (Copy p) -> Some (Copy { p with proj = p.proj @ [ f ] })
      | _ -> dummy)
  | Binop (((And | Or) as op), a, b) ->
      let t = temp st TBool in
      let oa = Option.get (expr st env a) in
      let rhs = new_block st and short = new_block st and join = new_block st in
      (match op with
      | And -> terminate st (Branch (oa, rhs, short))
      | _ -> terminate st (Branch (oa, short, rhs)));
      st.cur <- rhs;
      let ob = Option.get (expr st env b) in
      emit st (Assign (whole t, Use ob));
      terminate st (Goto join);
      st.cur <- short;
      emit st (Assign (whole t, Use (Const (CBool (op = Or)))));
      terminate st (Goto join);
      st.cur <- join;
      Some (Copy (whole t))
  | Binop (op, a, b) ->
      let oa = expr st env a in
      let ob = expr st env b in
      value st x (Bin (op, Option.get oa, Option.get ob))
  | Unop (op, a) ->
      let oa = expr st env a in
      value st x (Un (op, Option.get oa))
  | Call (f, args) ->
      let os = List.map (fun a -> Option.get (expr st env a)) args in
      value st x (Call (f, os))
  | StructLit (n, inits) ->
      let fs = List.map (fun (f, e) -> (f, Option.get (expr st env e))) inits in
      value st x (Agg (n, fs))
  | If (c, a, b) ->
      let oc = Option.get (expr st env c) in
      let t = Option.bind (ty_of x) (fun ty -> if ty = TUnit then None else Some (temp st ty)) in
      let bt = new_block st and be = new_block st and join = new_block st in
      terminate st (Branch (oc, bt, be));
      let arm blk =
        let v = block st env blk in
        (match t, v with
        | Some t, Some v -> emit st (Assign (whole t, Use v))
        | _ -> ());
        terminate st (Goto join)
      in
      st.cur <- bt;
      arm a;
      st.cur <- be;
      (match b with Some b -> arm b | None -> terminate st (Goto join));
      st.cur <- join;
      (match t with Some t -> Some (Copy (whole t)) | None -> if ty_of x = None then dummy else None)
  | Block b -> block st env b

(* 计算一个右值：结果是 () 时只求值，否则存进临时变量。 *)
and value st (x : expr) rv =
  match ty_of x with
  | Some TUnit -> emit st (Eval rv); None
  | Some ty ->
      let t = temp st ty in
      emit st (Assign (whole t, rv));
      Some (Copy (whole t))
  | None -> emit st (Eval rv); dummy

and block st env (b : Ast.block) : operand option =
  let rec go env = function
    | [] -> ( match b.tail with Some e -> expr st env e | None -> None)
    | (s : Ast.stmt) :: rest -> (
        match s.s with
        | Let (v, _, e) ->
            let o = expr st env e in
            let ty = match ty_of e with Some t -> t | None -> TI64 in
            let l = new_local st v ty in
            (match o with Some o -> emit st (Assign (whole l, Use o)) | None -> ());
            go ((v, l) :: env) rest
        | Assign (v, e) ->
            (match expr st env e with
            | Some o -> emit st (Assign (whole (List.assoc v env), Use o))
            | None -> ());
            go env rest
        | While (c, body) ->
            let head = new_block st and bbody = new_block st and exit = new_block st in
            terminate st (Goto head);
            st.cur <- head;
            let oc = Option.get (expr st env c) in
            terminate st (Branch (oc, bbody, exit));
            st.cur <- bbody;
            ignore (block st env body);
            terminate st (Goto head);
            st.cur <- exit;
            go env rest
        | Return e ->
            let o = Option.bind e (expr st env) in
            (match st.fret, o with
            | Some r, Some o -> emit st (Assign (whole r, Use o))
            | _ -> ());
            terminate st Return;
            st.cur <- new_block st;       (* 之后的代码走不到 *)
            ignore (go env rest);
            dummy
        | Expr e ->
            ignore (expr st env e);
            go env rest)
  in
  go env b.stmts

let lower_fn name params ret body =
  let st = { locals = []; nlocals = 0; blocks = []; nblocks = 0; cur = 0; fret = None } in
  let ps = List.map (fun (x, t) -> (x, new_local st x t)) params in
  st.fret <- (if ret = TUnit then None else Some (new_local st "ret" ret));
  st.cur <- new_block st;
  let v = block st ps body in
  (match st.fret, v with
  | Some r, Some v -> emit st (Assign (whole r, Use v))
  | _ -> ());
  terminate st Return;
  let blocks = Array.of_list (List.rev st.blocks) in
  Array.iter (fun b -> b.stmts <- List.rev b.stmts; if b.term = Unterminated then b.term <- Return) blocks;
  {
    name;
    params = List.map snd ps;
    ret;
    ret_local = st.fret;
    locals = Array.of_list (List.rev st.locals);
    blocks;
  }

let lower (p : program) : func list =
  List.filter_map
    (function
      | Fn { name; params; ret; body; _ } -> Some (lower_fn name params ret body)
      | Struct _ -> None)
    p
