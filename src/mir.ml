(* MIR：每个函数一张控制流图。基本块里是简单的赋值语句，块尾是跳转。
   降级时操作数一律从左到右求值；`&&`、`||`、`if`、`while`、`match`、`?` 都变成分支。
   需要析构的值（Box、含 Box 的结构体、linear 值）用 Move 读取；作用域结束、return 和出错返回之前插入 Drop，
   由 own.ml 的数据流检查决定每个 Drop 是直接析构、按 drop flag 析构，还是去掉（linear 值则报错）。
   let / inout 实参传地址（Ref / RefMut）；let 绑定一个不能整个移走的位置时建投影（Proj），
   由 borrow.ml 检查锁定和排他性。
   出错返回（`?` 的出错分支和 `return Err(…)`）之前，按登记的相反顺序执行生效的 errdefer（M7（三））。 *)
open Ast

type local = int

(* 位置：一个局部变量加上字段 / 解引用路径。at 是源代码位置，用来报错。 *)
type proj =
  | PField of string
  | PDeref
  | PVar of int     (* 枚举第 i 个变体的载荷 *)
  | PIndex of int   (* Vec 的元素，下标在这个局部变量里；越界时 panic *)

type place = { base : local; proj : proj list; at : pos }

type const = CInt of int64 | CBool of bool

type operand =
  | Const of const
  | Copy of place
  | Move of place          (* 读出后原位置变成未初始化；只用于整个局部变量 *)
  | Ref of place           (* let 实参：传地址，被调函数只读 *)
  | RefMut of place        (* inout 实参：传地址，被调函数可以修改 *)

type rvalue =
  | Use of operand
  | Bin of binop * operand * operand      (* 不含 And / Or：它们降级成分支 *)
  | Un of unop * operand
  | Call of string * operand list
  | Agg of string * (string * operand) list   (* 结构体字面量 *)
  | BoxNew of ty * operand                (* box(e)：分配堆内存并存入 e *)
  | Unbox of ty * operand                 (* unbox(b)：取出 Box 里的值，释放堆内存（Lean M11 的 unbox） *)
  | Consume of ty * operand               (* consume(x)：消费 linear 值，析构它的字段 *)
  | Alloc of ty * place * operand         (* alloc(a, e)：在区域 a 里分配并存入 e *)
  | MkEnum of ty * int * operand option   (* .A(e) / .B：第 i 个变体（Lean M8 的 mk） *)
  | Payload of operand * int              (* 取出第 i 个变体的载荷；没有载荷时只把枚举标记为已移走 *)
  | TagIs of operand * int                (* 枚举的标签是不是 i（只读） *)

(* Drop 的原因，用来报 linear 值没有被消费的错误。 *)
type why = End | Ret | Err

type stmt =
  | Assign of place * rvalue
  | Eval of rvalue                        (* 只为副作用，例如返回 () 的调用 *)
  | Proj of local * place                 (* let q = p.a：q 指向 p 内部，不拥有值 *)
  | Live of local * pos * string          (* 检查：这里 l 必须已初始化，否则报这条错误 *)
  | Enter of local                        (* 进入 arena 块：创建区域 *)
  | Leave of local                        (* 离开 arena 块（包括 return、出错返回穿过它）：整块释放 *)
  | Raise                                 (* 标记：这个块以出错返回结束（导出给 Lean 时用） *)
  | Drop of local * pos * why             (* 降级产生：如果还拥有这个值就析构 *)
  | DropNow of local                      (* 展开后：一定拥有，直接析构 *)
  | DropIf of local * local               (* 展开后：第二个局部（drop flag）为真时析构 *)
  | DropField of local * string           (* 展开后：部分移动过的结构体，只析构这个字段 *)
  | DropFieldIf of local * string * local (* 展开后：这个字段的 drop flag 为真时析构它 *)
  | DropAt of place                       (* 展开后：析构这个位置上的值（给字段赋值前析构旧值） *)

type term =
  | Goto of int
  | Branch of operand * int * int
  | Return
  | Unterminated                          (* 降级过程中的临时状态 *)

type block = { mutable stmts : stmt list; mutable term : term }   (* 降级时 stmts 倒序存放 *)

(* lref：这个局部是指针（let / inout 参数、投影），读写都要经过它，它本身不拥有值。
   lpos：声明位置，用来报“作用域结束时还没有被消费”。 *)
type local_info = { lname : string; lty : ty; lref : bool; lpos : pos }

type func = {
  name : string;
  params : local list;
  ret : ty;
  ret_local : local option;      (* 返回值放在这里；返回 () 时没有 *)
  locals : local_info array;
  blocks : block array;          (* 0 号块是入口 *)
  roots : local array;           (* 投影最终指向哪个局部的内部；其他局部是自己 *)
  arenas : (local * int) list;   (* arena 块的局部 → 嵌套层数（从 0 开始，不算区域参数） *)
  refarena : (local * local) list;   (* 引用类型的局部 → 它的区域所在的 arena 块的局部；区域参数的引用不在这里 *)
}

let nopos = { line = 0; col = 0 }

let whole ?(at = nopos) l = { base = l; proj = []; at }

(* 位置的类型：从局部的类型出发，沿着路径走。 *)
let rec proj_ty ty = function
  | [] -> ty
  | PField g :: rest -> (
      match ty with
      | TStruct n -> (
          match Hashtbl.find_opt Typeck.struct_tbl n with
          | Some fs -> proj_ty (List.find (fun (h : field) -> h.fname = g) fs).fty rest
          | None -> TUnit)
      | _ -> TUnit)
  | PDeref :: rest -> ( match ty with TBox t | TRef (t, _) -> proj_ty t rest | _ -> TUnit)
  | PIndex _ :: rest -> ( match ty with TVec t -> proj_ty t rest | _ -> TUnit)
  | PVar i :: rest -> (
      match Typeck.variants ty with
      | Some vs -> ( match snd (List.nth vs i) with Some t -> proj_ty t rest | None -> TUnit)
      | None -> TUnit)

let place_ty (f : func) (p : place) = proj_ty f.locals.(p.base).lty p.proj


(* ---------- 降级 ---------- *)

type st = {
  mutable locals : local_info list;   (* 倒序 *)
  mutable nlocals : int;
  mutable blocks : block list;        (* 倒序 *)
  mutable nblocks : int;
  mutable cur : int;
  mutable fret : local option;
  fret_ty : ty;
  mutable scopes : local list list;   (* 每层作用域拥有的、需要析构的局部，内层在前 *)
  mutable proots : (local * local) list;   (* 投影 → 它指向的局部 *)
  mutable regions : local list;        (* 当前所在的 arena 块，内层在前 *)
  mutable arenas : (local * int) list;
  mutable refarena : (local * local) list;
  mutable guards : (local * string * (string * local) list * expr) list;
      (* 生效的 errdefer：要消费的局部、它的名字、登记时的环境、调用表达式；内层在前 *)
}

let new_local ?(lref = false) ?(pos = nopos) st lname lty =
  st.locals <- { lname; lty; lref; lpos = pos } :: st.locals;
  (match lty with
  | TRef (_, r) -> (
      match List.find_opt (fun a -> (List.nth st.locals (st.nlocals - a)).lname = r) st.regions with
      | Some a -> st.refarena <- (st.nlocals, a) :: st.refarena
      | None -> ())
  | _ -> ());
  st.nlocals <- st.nlocals + 1;
  st.nlocals - 1

let info st l = List.nth st.locals (st.nlocals - 1 - l)

let root st l = match List.assoc_opt l st.proots with Some r -> r | None -> l

(* 需要析构的局部登记到当前作用域，作用域结束时插入 Drop。 *)
let own st l ty =
  if Typeck.needs_drop ty then
    match st.scopes with
    | s :: rest -> st.scopes <- (l :: s) :: rest
    | [] -> ()

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

(* 不会被用到的值（Never 类型的表达式）用一个占位常量代替；它只出现在走不到的块里。 *)
let dummy = Some (Const (CInt 0L))

let temp st ty pos =
  let t = new_local ~pos st "" ty in
  own st t ty;
  t

(* 读整个局部变量：需要析构的类型移动，其余复制。 *)
let use_local l ty at = if Typeck.needs_drop ty then Move (whole ~at l) else Copy (whole ~at l)

(* 一个位置是不是可以直接移走的局部（整个变量、拥有它的值）。 *)
let owned_whole st (p : place) = p.proj = [] && not (info st p.base).lref

let rec is_place (x : expr) =
  match x.e with
  | Var _ -> true
  | Field (a, _) | Deref a | Call ("vec_get", [ a; _ ]) -> is_place a
  | _ -> false

let place_of = function Copy p | Move p -> p | _ -> assert false

(* 作用域结束：在 l 的声明位置报错。 *)
let drop_end st locals = List.iter (fun l -> emit st (Drop (l, (info st l).lpos, End))) locals

let rec expr st env (x : expr) : operand option =
  match x.e with
  | Int n -> Some (Const (CInt n))
  | Bool b -> Some (Const (CBool b))
  | Unit -> None
  | Var _ | Field _ | Deref _ | Call ("vec_get", _) -> (
      match ty_of x with
      | None -> dummy
      | Some ty ->
          let p = place st env x in
          let lname = (info st p.base).lname in
          if not (Typeck.needs_drop ty) then Some (Copy p)
          else if owned_whole st p then Some (Move p)
          else if p.proj = [] then
            raise
              (Typeck.Error
                 (x.pos, Printf.sprintf "%s 不拥有值（是 let / inout 参数或投影），不能移动" lname))
          else
            match p.proj, (info st p.base).lty with
            | [ PField _ ], TStruct n when Hashtbl.mem Typeck.struct_tbl n ->
                (* 部分移动（Lean M13）：移走变量的一个字段，own.ml 按字段跟踪 *)
                if (info st p.base).lref then
                  raise
                    (Typeck.Error
                       (x.pos, Printf.sprintf "%s 不拥有值（是 let / inout 参数或投影），不能移走它的字段" lname))
                else if Typeck.is_linear (TStruct n) then
                  raise (Typeck.Error (x.pos, Printf.sprintf "linear 结构体 %s 不能部分移动（要用 consume）" n))
                else Some (Move p)
            | _ ->
                raise
                  (Typeck.Error
                     (x.pos, "只能移走变量的直接字段，不能从 Box、Vec 或多层字段里移出值（Box 用 unbox，Vec 的元素用 vec_pop）")))
  | Binop (((And | Or) as op), a, b) ->
      let t = temp st TBool x.pos in
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
  | Call ("box", [ a ]) ->
      let oa = Option.get (expr st env a) in
      let ty = match ty_of a with Some t -> t | None -> TI64 in
      value st x (BoxNew (ty, oa))
  | Call ("unbox", [ a ]) ->
      let oa = Option.get (expr st env a) in
      let ty = match ty_of x with Some t -> t | None -> TI64 in
      value st x (Unbox (ty, oa))
  | Call ("consume", [ a ]) ->
      let oa = Option.get (expr st env a) in
      (match ty_of a with Some ty -> emit st (Eval (Consume (ty, oa))) | None -> ());
      None
  | Call ("alloc", [ a; v ]) ->
      let pa = place st env a in
      let ov = Option.get (expr st env v) in
      let ty = match ty_of v with Some t -> t | None -> TI64 in
      value st x (Alloc (ty, pa, ov))
  | Call (f, args) ->
      let modes = List.map fst (fst (Hashtbl.find Typeck.sigs f)) in
      let os = List.map2 (arg st env) modes args in
      (* 泛型函数调用的是推出的类型实参对应的实例 *)
      let f = match Hashtbl.find_opt Typeck.call_inst x.id with Some ts -> Typeck.inst_name f ts | None -> f in
      value st x (Call (f, os))
  | StructLit (n, inits) ->
      let fs = List.map (fun (f, e) -> (f, Option.get (expr st env e))) inits in
      (* 泛型结构体：用实例的名字 *)
      let n = match ty_of x with Some (TStruct m) -> m | _ -> n in
      value st x (Agg (n, fs))
  | Variant (v, a) -> (
      let oa = Option.bind a (expr st env) in
      match ty_of x with
      | Some ty -> (
          match Typeck.variant ty v with Some (i, _) -> value st x (MkEnum (ty, i, oa)) | None -> dummy)
      | None -> dummy)
  | If (c, a, b) ->
      let oc = Option.get (expr st env c) in
      let ty = ty_of x in
      let t = Option.bind ty (fun ty -> if ty = TUnit then None else Some (temp st ty x.pos)) in
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
      result t ty x.pos
  | Block b -> block st env b
  | Arena (r, b) ->
      (* 块的值先存进块外的临时变量，再释放区域：值的类型不含这个区域（R3），可以安全带出 *)
      let l = new_local ~pos:x.pos st r (TArena r) in
      emit st (Enter l);
      st.arenas <- (l, List.length st.regions) :: st.arenas;
      st.regions <- l :: st.regions;
      let v = block st ((r, l) :: env) b in
      let v =
        match v, ty_of x with
        | Some ((Copy _ | Move _) as o), Some ty ->
            let t = temp st ty x.pos in
            emit st (Assign (whole t, Use o));
            Some (use_local t ty x.pos)
        | _ -> v
      in
      st.regions <- List.tl st.regions;
      emit st (Leave l);
      v
  | Try a -> (
      match ty_of a with
      | Some (TResult (t, e)) ->
          (* 出错：把错误值包成函数返回的 Err，执行 errdefer，出错返回；否则取出 Ok 的值。
             Result 是枚举，Ok 的标签是 0，Err 是 1 *)
          let o = Option.get (expr st env a) in
          let c = temp st TBool x.pos in
          emit st (Assign (whole c, TagIs (Copy (place_of o), 0)));
          let bok = new_block st and berr = new_block st in
          terminate st (Branch (Copy (whole c), bok, berr));
          st.cur <- berr;
          let ev = unwrap st o e 1 x.pos in
          (match st.fret with
          | Some r -> emit st (Assign (whole r, MkEnum (st.fret_ty, 1, ev)))
          | None -> ());
          exit_fn st x.pos Err;
          st.cur <- bok;
          unwrap st o t 0 x.pos
      | _ ->
          ignore (expr st env a);
          dummy)
  | Match (borrow, s, arms) -> (
      match ty_of s with
      | Some sty when Typeck.variants sty <> None -> enum_match st env x borrow s sty arms
      | _ ->
          ignore (expr st env s);
          dummy)

(* 枚举（含 Result）的 match（Lean M8（一）、M8（二））：按标签依次比较，跳到对应分支，最后一个变体不用比较。
   按移动：分支开始时把载荷移进分支变量（枚举整个变成已移走）；没有载荷的变体也把枚举标记为已移走。
   各分支结束时状态不同的变量由 own.ml 合并成“可能已初始化”，按 drop flag 析构。
   按借用（match let，或者被匹配的位置不能整个移走，和 let 绑定投影的规则相同）：
   需要析构的载荷绑定成投影，由 borrow.ml 锁住源对象；可复制的载荷直接复制。枚举本身不动。 *)
and enum_match st env (x : expr) explicit (s : expr) sty arms =
  let vs = Option.get (Typeck.variants sty) in
  let needs = Typeck.needs_drop sty in
  let borrowed, p, o =
    if is_place s then begin
      let p = place st env s in
      let b = needs && (explicit || not (owned_whole st p)) in
      (b, p, if needs && not b then Move p else Copy p)
    end
    else
      let o = Option.get (expr st env s) in
      (false, place_of o, o)
  in
  let ty = ty_of x in
  let t = Option.bind ty (fun ty -> if ty = TUnit then None else Some (temp st ty x.pos)) in
  let last = List.length vs - 1 in
  let arms_blocks = List.map (fun _ -> new_block st) vs and join = new_block st in
  List.iteri
    (fun i b ->
      if i = last then terminate st (Goto b)
      else begin
        let c = temp st TBool x.pos in
        emit st (Assign (whole c, TagIs (Copy p, i)));
        let next = new_block st in
        terminate st (Branch (Copy (whole c), b, next));
        st.cur <- next
      end)
    arms_blocks;
  List.iteri
    (fun i ((v, pty), b) ->
      st.cur <- b;
      let _, bind, (e : expr) = List.find (fun (w, _, _) -> w = v) arms in
      st.scopes <- [] :: st.scopes;
      let env' =
        let pay = { p with proj = p.proj @ [ PVar i ] } in
        match pty with
        | None ->
            (match o with Move _ -> emit st (Eval (Payload (o, i))) | _ -> ());
            env
        | Some pty when borrowed && Typeck.needs_drop pty ->
            let q = new_local ~lref:true ~pos:e.pos st bind pty in
            st.proots <- (q, root st p.base) :: st.proots;
            emit st (Proj (q, pay));
            (bind, q) :: env
        | Some pty when borrowed ->
            let l = new_local ~pos:e.pos st bind pty in
            emit st (Assign (whole l, Use (Copy pay)));
            (bind, l) :: env
        | Some pty ->
            let l = new_local ~pos:e.pos st bind pty in
            own st l pty;
            emit st (Assign (whole l, Payload (o, i)));
            (bind, l) :: env
      in
      let v = close_scope st (expr st env' e) (ty_of e) e.pos in
      (match t, v with Some t, Some v -> emit st (Assign (whole t, Use v)) | _ -> ());
      terminate st (Goto join))
    (List.combine vs arms_blocks);
  st.cur <- join;
  result t ty x.pos

(* if / match 的结果：放在临时变量 t 里；() 时没有；Never 时用占位值。 *)
and result t ty pos =
  match t, ty with
  | Some t, Some ty -> Some (use_local t ty pos)
  | _ -> if ty = None then dummy else None

(* 取出 Result 里一边的值，存进临时变量。值是 () 时没有临时变量，但仍要把 Result 标记为已移走。 *)
and unwrap st o ty i pos =
  if ty = TUnit then begin
    (match o with Move _ -> emit st (Eval (Payload (o, i))) | _ -> ());
    None
  end
  else begin
    let v = temp st ty pos in
    emit st (Assign (whole v, Payload (o, i)));
    Some (use_local v ty pos)
  end

(* 离开函数（返回值已经放好）。出错返回时先执行生效的 errdefer，内层的先执行；
   然后析构所有作用域里的局部，再释放穿过的区域（M7（四））。之后的代码走不到。 *)
and exit_fn st at why =
  if why = Err then begin
    emit st Raise;
    List.iter
      (fun (l, v, env, e) ->
        emit st (Live (l, at, Printf.sprintf "%s 在这里出错返回之前已经被消费，errdefer 会重复消费它" v));
        ignore (expr st env e))
      st.guards
  end;
  List.iter (List.iter (fun l -> emit st (Drop (l, at, why)))) st.scopes;
  List.iter (fun l -> emit st (Leave l)) st.regions;
  terminate st Return;
  st.cur <- new_block st

(* 按参数约定传实参：sink 移动或复制；let 传位置的地址，不是位置的先算进临时变量；
   inout 一定是位置（类型检查保证）。 *)
and arg st env m (a : expr) : operand =
  match m with
  | MSink -> Option.get (expr st env a)
  | MInout -> RefMut (place st env a)
  | MLet when is_place a -> Ref (place st env a)
  | MLet ->
      (* 例如 f({ x }) 会移走 x：值一律先存进本作用域的临时变量，调用后随作用域析构 *)
      let o = expr st env a in
      let ty = match ty_of a with Some t -> t | None -> TI64 in
      let t = temp st ty a.pos in
      emit st (Assign (whole t, Use (Option.value o ~default:(Const (CInt 0L)))));
      Ref (whole t)

(* 变量、字段、解引用组成的位置；其他表达式先算进临时变量。 *)
and place st env (x : expr) : place =
  match x.e with
  | Var v -> whole ~at:x.pos (List.assoc v env)
  | Field (a, f) ->
      let p = place st env a in
      { p with proj = p.proj @ [ PField f ]; at = x.pos }
  | Deref a ->
      let p = place st env a in
      { p with proj = p.proj @ [ PDeref ]; at = x.pos }
  | Call ("vec_get", [ a; i ]) ->
      (* 下标先算进临时变量，再取元素 *)
      let p = place st env a in
      let oi = Option.get (expr st env i) in
      let li = temp st TI64 i.pos in
      emit st (Assign (whole li, Use oi));
      { p with proj = p.proj @ [ PIndex li ]; at = x.pos }
  | _ -> (
      match expr st env x with
      | Some (Copy p | Move p) -> p
      | _ -> whole (new_local st "" TI64))     (* Never：只出现在走不到的块里 *)

(* 计算一个右值：结果是 () 时只求值，否则存进临时变量。 *)
and value st (x : expr) rv =
  match ty_of x with
  | Some TUnit -> emit st (Eval rv); None
  | Some ty ->
      let t = temp st ty x.pos in
      emit st (Assign (whole t, rv));
      Some (use_local t ty x.pos)
  | None -> emit st (Eval rv); dummy

(* 位置的值需要析构、又不能整个移走时，let 绑定它就是投影。只看类型和路径，不产生代码。 *)
and proj_init st env (e : expr) =
  let rec base (x : expr) =
    match x.e with
    | Var v -> (List.assoc v env, true)
    | Field (a, _) | Deref a | Call ("vec_get", [ a; _ ]) -> (fst (base a), false)
    | _ -> assert false
  in
  match ty_of e with
  | Some ty when Typeck.needs_drop ty ->
      let l, whole_var = base e in
      not (whole_var && not (info st l).lref)
  | _ -> false

(* 结束最内层作用域。值 v 如果放在本作用域的局部里，先移到外层的临时变量，再析构本作用域。 *)
and close_scope st v ty pos =
  let mine = List.hd st.scopes in
  st.scopes <- List.tl st.scopes;
  let v =
    match mine, v, ty with
    | _ :: _, Some ((Copy _ | Move _) as o), Some ty ->
        let t = temp st ty pos in
        emit st (Assign (whole t, Use o));
        Some (use_local t ty pos)
    | _ -> v
  in
  drop_end st mine;
  v

and block st env (b : Ast.block) : operand option =
  st.scopes <- [] :: st.scopes;
  let guards = st.guards in
  let rec go env = function
    | [] -> ( match b.tail with Some e -> expr st env e | None -> None)
    | (s : Ast.stmt) :: rest -> (
        match s.s with
        | Let (false, v, _, e) when is_place e && proj_init st env e ->
            (* let 绑定不拥有值的位置（字段、解引用、参数、投影）：建一个投影，不移动 *)
            let p = place st env e in
            let ty = Option.get (ty_of e) in
            let q = new_local ~lref:true ~pos:s.spos st v ty in
            st.proots <- (q, root st p.base) :: st.proots;
            emit st (Proj (q, p));
            go ((v, q) :: env) rest
        | Decl (v, ty) ->
            (* 一开始未初始化，由 own.ml 的数据流检查读之前有没有赋值 *)
            let l = new_local ~pos:s.spos st v ty in
            own st l ty;
            go ((v, l) :: env) rest
        | Let (_, v, ann, e) ->
            let o = expr st env e in
            let ty =
              match ann, ty_of e with Some t, _ | None, Some t -> t | None, None -> TI64
            in
            let l = new_local ~pos:s.spos st v ty in
            own st l ty;
            (match o with Some o -> emit st (Assign (whole l, Use o)) | None -> ());
            go ((v, l) :: env) rest
        | Assign (v, e) ->
            (* 旧值的析构由 own.ml 在赋值前插入 *)
            (match expr st env e with
            | Some o -> emit st (Assign (whole ~at:s.spos (List.assoc v env), Use o))
            | None -> ());
            go env rest
        | SetField (a, f, e) ->
            (* 先算右边，再算位置；旧值的析构由 own.ml 在赋值前插入 *)
            (match expr st env e with
            | Some o ->
                let p = place st env a in
                emit st (Assign ({ p with proj = p.proj @ [ PField f ]; at = s.spos }, Use o))
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
            let why =
              match e, st.fret_ty with Some { e = Variant ("Err", _); _ }, TResult _ -> Err | _ -> Ret
            in
            exit_fn st s.spos why;
            ignore (go env rest);
            dummy
        | Destruct (_, bs, e) ->
            (* 每个字段复制进各自的变量（所有权跟着转过去），然后把结构体标记为已移走、不析构。
               结构体不能整个移走（参数、投影）时，expr 报错 *)
            let o = Option.get (expr st env e) in
            let p = place_of o in
            let fs =
              match ty_of e with Some (TStruct m) -> Hashtbl.find Typeck.struct_tbl m | _ -> []
            in
            let env' =
              List.fold_left
                (fun env (f, x) ->
                  let ty = (List.find (fun (g : field) -> g.fname = f) fs).fty in
                  let l = new_local ~pos:s.spos st x ty in
                  own st l ty;
                  emit st (Assign (whole l, Use (Copy { p with proj = p.proj @ [ PField f ] })));
                  (x, l) :: env)
                env bs
            in
            (match o with Move _ -> emit st (Eval (Use o)) | _ -> ());
            go env' rest
        | Errdefer e ->
            (* 登记：要消费的值此刻必须还活着（M7（六）的登记规则） *)
            let v = Hashtbl.find Typeck.errdefer_targets e.id in
            let l = List.assoc v env in
            emit st (Live (l, s.spos, Printf.sprintf "errdefer 登记时 %s 必须是还没有被消费的 linear 值" v));
            st.guards <- (l, v, env, e) :: st.guards;
            go env rest
        | Expr e ->
            ignore (expr st env e);
            go env rest)
  in
  let v = go env b.stmts in
  st.guards <- guards;
  close_scope st v (Option.bind b.tail ty_of) (match b.tail with Some e -> e.pos | None -> nopos)

let lower_fn name params ret body ipos =
  let st =
    {
      locals = [];
      nlocals = 0;
      blocks = [];
      nblocks = 0;
      cur = 0;
      fret = None;
      fret_ty = ret;
      scopes = [ [] ];
      proots = [];
      regions = [];
      arenas = [];
      refarena = [];
      guards = [];
    }
  in
  let ps =
    List.map
      (fun (m, x, t) ->
        let l = new_local ~lref:(m <> MSink) ~pos:ipos st x t in
        if m = MSink then own st l t;
        (x, l))
      params
  in
  st.fret <- (if ret = TUnit then None else Some (new_local st "ret" ret));
  st.cur <- new_block st;
  let v = block st (List.rev ps) body in
  (match st.fret, v with
  | Some r, Some v -> emit st (Assign (whole r, Use v))
  | _ -> ());
  List.iter (drop_end st) st.scopes;
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
    roots = Array.init st.nlocals (root st);
    arenas = st.arenas;
    refarena = st.refarena;
  }

let lower (p : program) : func list =
  List.filter_map
    (function
      | Fn { name; params; ret; body; ipos; _ } -> Some (lower_fn name params ret body ipos)
      | Struct _ | Enum _ | Trait _ | Impl _ -> None)
    p
