(* 类型检查之后，把一个函数的 Ast 连同类型检查记在旁路表里的结果翻成 Tast（R4b.1）。
   降级只读 Tast 和 Defs；按表达式 id 查的表（types、call_inst、for_vec、scope_shares、errdefer_targets）
   和别名、签名、arm_for 都只在这里查。类型检查直接产出 Tast 之后（R4b–d）这个文件就去掉。

   这里只是换个形状，不做检查：类型检查已经保证查得到。查不到时和以前降级时一样抛 Not_found。 *)
open Ast
module T = Tast

let typ (x : expr) : T.typ =
  match Hashtbl.find_opt Typeck.types x.id with Some (Typeck.Ty t) -> T.Ty t | _ -> T.Never

let rec expr (x : expr) : T.expr =
  let ty = typ x in
  let mk e = { T.e; ty; pos = x.pos } in
  match x.e with
  | Int n -> mk (T.Lit (T.Int n))
  | Float f -> mk (T.Lit (T.Float f))
  | Char c -> mk (T.Lit (T.Char c))
  | Bool b -> mk (T.Lit (T.Bool b))
  | Unit -> mk (T.Lit T.Unit)
  | Str s -> mk (T.Lit (T.Str s))
  | Var v -> mk (T.Var v)
  | Field (a, f) -> mk (T.Field (expr a, f))
  | Deref a -> mk (T.Deref (expr a))
  | Binop (op, a, b) -> mk (T.Binop (op, expr a, expr b))
  | Unop (op, a) -> mk (T.Unop (op, expr a))
  (* 影响所有权或控制流的内建函数，各一个构造子 *)
  | Call ("vec_get", [ a; i ]) -> mk (T.Index (expr a, expr i))
  | Call ("ref_of", [ a ]) -> mk (T.Ref (expr a))
  | Call ("box", [ a ]) -> mk (T.Box (expr a))
  | Call ("unbox", [ a ]) -> mk (T.Unbox (expr a))
  | Call ("consume", [ a ]) -> mk (T.Consume (expr a))
  | Call ("alloc", [ a; v ]) -> mk (T.Alloc (expr a, expr v))
  | Call ("panic", [ a ]) -> mk (T.Panic (expr a))
  | Call ("os_exit", [ a ]) -> mk (T.Exit (expr a))
  | Call (f, args) -> mk (T.Call (call x f args))
  | StructLit (n, inits) ->
      (* 泛型结构体：用实例的名字 *)
      let n = match ty with T.Ty (TStruct m) -> m | _ -> n in
      mk (T.StructLit (n, List.map (fun (f, e) -> (f, expr e)) inits))
  | Variant (v, a) ->
      let payload = Option.map expr a in
      let tag, payload =
        match ty with
        | T.Ty t -> (
            match Typeck.variant t v with
            | Some (i, pty) ->
                (* 载荷是 () 时可以省略不写：.Some 就是 .Some(()) *)
                (i, if payload = None && pty = Some TUnit then Some { T.e = T.Lit T.Unit; ty = T.Ty TUnit; pos = x.pos } else payload)
            | None -> (-1, payload))
        | T.Never -> (-1, payload)
      in
      mk (T.Variant { variant = v; tag; payload })
  | Match (borrow, s, arms) ->
      let scrut = expr s in
      (* 每个变体一个分支，按变体的顺序；没写的用 _ 分支 *)
      let arms =
        match scrut.ty with
        | T.Ty sty -> (
            match Typeck.variants sty with
            | Some vs ->
                List.map
                  (fun (v, pty) ->
                    let bind, e = Option.get (Typeck.arm_for arms (v, pty)) in
                    { T.bind; body = expr e })
                  vs
            | None -> [])
        | T.Never -> []
      in
      mk (T.Match { borrow; scrut; arms })
  | Arena (r, b) -> mk (T.Arena (r, block b))
  | If (c, a, b) -> mk (T.If (expr c, block a, Option.map block b))
  | Block b -> mk (T.Block (block b))
  | Try a -> mk (T.Try (expr a))
  | Lambda _ -> failwith "闭包在类型检查之后已经换成提升出来的函数"
  | PMatch _ -> failwith "嵌套模式在类型检查时已经展开"

(* 调用：泛型函数调用的是推出的类型实参对应的实例；内建类型的 to_str 直接是内建函数（别名），
   这时实参按内建函数的签名传（I64 按值，不按 Self 的借用）。返回借用的泛型函数，实例也借自同样的参数。 *)
and call (x : expr) f args : T.call =
  let g = match Hashtbl.find_opt Typeck.call_inst x.id with Some ts -> Typeck.inst_name f ts | None -> f in
  let f, g = match Hashtbl.find_opt Typeck.aliases g with Some b -> (b, b) | None -> (f, g) in
  let lent = Hashtbl.find_opt Typeck.let_regions f in
  (* 泛型实例也登记进 Defs（borrow.ml 按调用的函数名查）；以前是降级时登记的 *)
  (match lent with Some l when g <> f -> Hashtbl.replace Typeck.let_regions g l | _ -> ());
  {
    callee = g;
    targs = [];
    modes = List.map fst (fst (Hashtbl.find Typeck.sigs f));
    borrows = Option.value lent ~default:[];
    args = List.map expr args;
  }

and stmt (s : stmt) : T.stmt =
  let mk d = { T.s = d; spos = s.spos } in
  match s.s with
  | Let (var, name, ann, e) ->
      let init = expr e in
      let ty = match ann, init.ty with Some t, _ | None, T.Ty t -> t | None, T.Never -> TI64 in
      mk (T.Let { var; name; ty; init })
  | Decl (v, t) -> mk (T.Decl (v, t))
  | Assign (v, e) -> mk (T.Assign (v, expr e))
  | SetField (a, f, e) -> mk (T.SetField (expr a, f, expr e))
  | SetPlace (a, e) -> mk (T.SetPlace (expr a, expr e))
  | While (c, b) -> mk (T.While (expr c, block b))
  | For (a, sv, si) ->
      (* 类型检查选的展开 *)
      stmt (if Hashtbl.find_opt Typeck.for_vec a.id = Some false then si else sv)
  | Scope (a, body) ->
      let handle = match a.e with Var v -> v | _ -> assert false in
      let shared = Option.value (Hashtbl.find_opt Typeck.scope_shares a.id) ~default:[] in
      mk (T.Scope { handle; shared; body = block body })
  | Return e -> mk (T.Return (Option.map expr e))
  | Expr e -> mk (T.Expr (expr e))
  | Errdefer e -> mk (T.Errdefer { call = expr e; target = Hashtbl.find Typeck.errdefer_targets e.id })
  | Break -> mk T.Break
  | Continue -> mk T.Continue
  | Destruct (n, bs, e) ->
      let value = expr e in
      let fs = match value.ty with T.Ty (TStruct m) -> Hashtbl.find Typeck.struct_tbl m | _ -> [] in
      let fields = List.map (fun (f, x) -> (f, x, (List.find (fun (g : field) -> g.fname = f) fs).fty)) bs in
      mk (T.Destruct { struct_name = n; fields; value })

and block (b : block) : T.block = { stmts = List.map stmt b.stmts; tail = Option.map expr b.tail }

(* 一个条目要降级的函数：函数是一个，其他条目没有。 *)
let item = function
  | Fn { name; params; ret; body; ipos; _ } ->
      [ { T.name; params; ret; body = block body; pos = ipos; from_core = Modules.in_core ipos } ]
  | Struct _ | Enum _ | Trait _ | Impl _ -> []

(* 代码生成要为它们写 C 辅助函数的东西：类型检查记在表里的闭包、Fn 调用、任务、Vec 内建函数的实例，
   列表的顺序和以前代码生成遍历这些表的顺序相同（生成的 C 不变）。
   vec_pop 返回的 Option<e> 和运行时总要用的 Option<Str>、Option<I64>、Option<F64>、Option<Char> 在这里实例化，
   代码生成就不再往 Defs 里加东西。所有函数降级、检查完之后调用。 *)
let generated () : T.generated =
  let list tbl = List.rev (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []) in
  let vec_calls = list Typeck.vec_calls in
  let elems = List.filter_map (fun (_, (b, e)) -> if b = "pop" then Some e else None) vec_calls in
  {
    packs = list Typeck.packs;
    fcalls = list Typeck.fcalls;
    spawns = list Typeck.spawns;
    vec_calls;
    options = List.map (fun e -> (e, Typeck.instantiate "Option" [ e ])) (elems @ [ TStr; TI64; TF64; TChar ]);
  }
