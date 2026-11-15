(* 单态化（Lean `Generics.Check`：泛型函数按抽象类型检查一次，实例就是把类型参数换成类型实参）。
   泛型函数检查完记在 generic_fns：带类型的语法树，和检查时按顺序发生的请求（调用了哪些泛型函数、Vec 内建函数，见 check_state.ml）。
   一个实例 = 语法树里的类型换掉、泛型调用按换过的类型实参重新算名字、按原来的顺序重放请求、
   展开嵌套模式时生成的变量 #pN 重新编号（编号会出现在生成的 C 里，要和重新检查时的一样）。
   实例本身不再检查：泛型函数按抽象类型检查通过，换成满足约束的具体类型也通过。
   只有任务、Fn(…) 闭包的捕获含不含引用要按实例的类型再看一次（重放 Lam、Pack 时检查，报的错和以前一样）。

   闭包：泛型函数里的闭包检查时不提升（不知道捕获的具体类型），记下 Lam；实例里按换过的类型提升（lift，和检查器共用），
   带 fn(…) 参数的调用（语法树里 callee 是 spec#编号）换成特化的调用，Fn#…、spawn#…、call#… 的名字跟着类型换。
   --batch 时泛型函数可能在还没检查的条目里：用到它的实例时先检查它（define）。
   SINTER_MONO=check 时所有实例都复制 Ast 重新检查，另外干跑单态化、逐个比较，最后在标准错误上报告（mono_check.sh 用）。 *)
open Ast
open Ctx
open Region
open Generics
module T = Tast

let checking = Sys.getenv_opt "SINTER_MONO" = Some "check"

(* ---------- 换掉语法树里的类型和变量名 ---------- *)

(* 按 ty（类型）、var（变量名）、call（调用节点，实参已经换过）改写一个函数。 *)
let map_fn ~(ty : ty -> ty) ~(var : string -> string) ~(call : T.call -> T.call) (fd : T.fn_def) : T.fn_def =
  let typ = function T.Ty t -> T.Ty (ty t) | T.Never -> T.Never in
  let sname m = match ty (TStruct m) with TStruct m' -> m' | _ -> m in
  (* 按语法树的顺序改写（ty 会登记泛型结构体的实例，顺序固定下来） *)
  let rec ex (x : T.expr) : T.expr =
    let e = desc x.e in
    { x with e; ty = typ x.ty }
  and desc : T.expr_desc -> T.expr_desc = function
    | T.Lit _ as d -> d
    | Var y -> Var (var y)
    | Binop (o, a, b) ->
        let a = ex a in
        Binop (o, a, ex b)
    | Unop (o, a) -> Unop (o, ex a)
    | Call c ->
        let args = List.map ex c.args in
        Call (call { c with args })
    | StructLit (m, fs) ->
        let fs = List.map (fun (f, e) -> (f, ex e)) fs in
        StructLit (sname m, fs)
    | Field (a, f) -> Field (ex a, f)
    | Deref a -> Deref (ex a)
    | Index (a, i) ->
        let a = ex a in
        Index (a, ex i)
    | Ref a -> Ref (ex a)
    | Box a -> Box (ex a)
    | Unbox a -> Unbox (ex a)
    | Consume a -> Consume (ex a)
    | Alloc (a, v) ->
        let a = ex a in
        Alloc (a, ex v)
    | Panic a -> Panic (ex a)
    | Exit a -> Exit (ex a)
    | Try a -> Try (ex a)
    | Variant v -> Variant { v with payload = Option.map ex v.payload }
    | Match { borrow; scrut; arms } ->
        let scrut = ex scrut in
        Match { borrow; scrut; arms = List.map (fun (a : T.arm) -> { T.bind = var a.bind; body = ex a.body }) arms }
    | Arena (r, b) -> Arena (r, block b)
    | If (c, a, b) ->
        let c = ex c in
        let a = block a in
        If (c, a, Option.map block b)
    | Block b -> Block (block b)
  and stmt (st : T.stmt) : T.stmt = { st with s = sdesc st.s }
  and sdesc : T.stmt_desc -> T.stmt_desc = function
    | Let { var = m; name; ty = t; init } ->
        let init = ex init in
        Let { var = m; name = var name; ty = ty t; init }
    | Decl (n, t) -> Decl (var n, ty t)
    | Assign (n, e) -> Assign (var n, ex e)
    | SetField (a, f, e) ->
        let a = ex a in
        SetField (a, f, ex e)
    | SetPlace (a, e) ->
        let a = ex a in
        SetPlace (a, ex e)
    | While (c, b) ->
        let c = ex c in
        While (c, block b)
    | Scope { handle; shared; body } -> Scope { handle; shared = List.map var shared; body = block body }
    | Return e -> Return (Option.map ex e)
    | Expr e -> Expr (ex e)
    | Errdefer { call; target } -> Errdefer { call = ex call; target = var target }
    | (Break | Continue) as d -> d
    | Destruct { struct_name; fields; value } ->
        let value = ex value in
        Destruct { struct_name; fields = List.map (fun (f, x, t) -> (f, var x, ty t)) fields; value }
  and block (b : T.block) : T.block =
    let stmts = List.map stmt b.stmts in
    { T.stmts; tail = Option.map ex b.tail }
  in
  let params = List.map (fun (m, x, t) -> (m, var x, ty t)) fd.params in
  let ret = ty fd.ret in
  { fd with params; ret; body = block fd.body }

(* 检查泛型函数时生成的 #pN（a < N）在实例里编号成从 c 往后数的。 *)
let renum a c v =
  let n = String.length v in
  if n > 2 && String.sub v 0 2 = "#p" then
    match int_of_string_opt (String.sub v 2 (n - 2)) with
    | Some k when k > a -> "#p" ^ string_of_int (k - a + c)
    | _ -> v
  else v

(* ---------- 实例 ---------- *)

(* 实例 (f, ts) 来自哪个检查过的泛型函数：和 Generics.instance 复制的是同一个（泛型 impl 的方法经过 minst）。
   查不到（--batch 时泛型函数在别的条目里还没检查）的照常重新检查。 *)
type source = {
  name : string;                  (* 实例名 *)
  fd : T.fn_def;                  (* 泛型函数的语法树 *)
  reqs : request list;
  pm0 : int;                      (* 检查泛型函数前后的 pm_count *)
  pm1 : int;
  s : (string * ty) list;         (* 类型参数 → 类型实参 *)
  regions : (string * string list) list;   (* 实例的区域参数（和检查实例时 check_fn 建的一样） *)
  nfn : int;                      (* 泛型函数的区域参数的个数（Lam 记下的区域里，后面这么多个是函数的） *)
}

(* define：泛型函数还没检查时检查它（--batch 时泛型函数可能在别的条目里；整个程序检查时它们总是先检查过）。 *)
let source ~define (f, ts) =
  let name = inst_name f ts in
  let g, ts = Option.value (Hashtbl.find_opt minst name) ~default:(f, ts) in
  (match Hashtbl.find_opt generic_fns g, Hashtbl.find_opt gfns g with
  | None, Some (Fn _ as it) -> define it
  | _ -> ());
  match Hashtbl.find_opt generic_fns g, Hashtbl.find_opt gfns g with
  | Some (fd, reqs, pm0, pm1), Some (Fn it) ->
      let s = List.combine (List.map fst it.tparams) ts in
      let sub (m, x, t) = (m, x, subst s t) in
      let regions = List.map (fun r -> (r, [])) (fn_regions (List.map sub it.params) (subst s it.ret)) in
      Some { name; fd; reqs; pm0; pm1; s; regions; nfn = List.length (fn_regions it.params it.ret) }
  | _ -> None

(* 闭包所在处可见的区域，换到实例里：前面是函数体里的 arena（内层在前，比外面的都活得短），后面是函数的区域参数。 *)
let lam_regions src (l : lam) =
  let arenas = List.filteri (fun i _ -> i < List.length l.lregions - src.nfn) l.lregions in
  List.fold_left (fun acc (r, _) -> (r, List.map fst acc) :: acc) src.regions (List.rev arenas)

(* 重放的结果：提升出的闭包（按 Lam 的编号）、特化（按 Spec 的编号：名字、参数、绑定的闭包）、换了名字的调用。 *)
type replayed = {
  lams : (int, closure * lam) Hashtbl.t;
  specs : (int, string * (mode * ty) list * (closure * lam) list) Hashtbl.t;
  renamed : (string, string) Hashtbl.t;
}

(* 按实例的类型实参、按原来的顺序重放请求。reg 为 Dry 时不登记，只记下会登记什么（和 trace 的写法一样），
   alias 收集会加上的内建函数别名。 *)
let replay ~reg ~alias src reqs =
  let s = src.s in
  let r = { lams = Hashtbl.create 4; specs = Hashtbl.create 4; renamed = Hashtbl.create 4 } in
  let spawned = List.filter_map (function Spawn (i, _) -> Some i | _ -> None) reqs in
  let dry = match reg with Dry _ -> true | Real -> false in
  List.iter
    (function
      | Inst (f, ts, has_fn) ->
          let ts = canon_regions (List.map (subst s) ts) in
          let n = inst_name f ts in
          if not dry then request_inst f ts ~has_fn
          else if List.for_all (fun t -> params_of t = []) ts && (not has_fn) && first reg n then begin
            (match ts with
            | [ t ] when Hashtbl.mem methods f && not (Hashtbl.mem sigs n) ->
                Option.iter (Hashtbl.replace alias n) (builtin_alias f t)
            | _ -> ());
            traced reg ("inst " ^ n)
          end
      | Vec (b, t) ->
          let t = subst s t in
          if not dry then vec_request b t
          else if params_of t = [] then traced reg ("vec " ^ inst_name ("vec_" ^ b) [ t ])
      | Fcall (fns, t, n) ->
          let t = subst s t in
          let n' =
            if not dry then fcall_register fns t
            else begin
              let n' = "call#" ^ string_of_ty t in
              if params_of t = [] && (not (Hashtbl.mem fcalls n')) && first reg ("fcall " ^ n') then
                traced reg ("fcall " ^ n');
              n'
            end
          in
          Hashtbl.replace r.renamed n n'
      | Lam l ->
          let caps0 = List.map (fun (m, v, t) -> (m, v, subst s t)) l.lcaps in
          (* 任务的捕获不能含引用（检查泛型函数时类型参数里没有引用，换成实例的类型后才知道） *)
          if List.mem l.lid spawned then
            List.iter
              (fun (_, v, t) -> if region_names t <> [] then err l.lexpr.pos "任务暂不能用含引用或区域的变量 %s" v)
              caps0;
          let c =
            lift ~reg ~ft:(subst s) ~concrete:true (lam_regions src l) l.lexpr.pos l.lys l.lbody
              (List.map (fun (m, t) -> (m, subst s t)) l.lpts)
              (Some (subst s l.lret)) caps0
          in
          Hashtbl.replace r.lams l.lid (c, l)
      | Spec (k, f, ts, ps, ids) ->
          let cls = List.map (Hashtbl.find r.lams) ids in
          let ts = Option.map (fun ts -> canon_regions (List.map (subst s) ts)) ts in
          let name = spec_register ~reg f ts (List.map fst cls) in
          Hashtbl.replace r.specs k (name, ps, cls)
      | Pack (i, want, pk) ->
          let c, l = Hashtbl.find r.lams i in
          pack_check l.lexpr.pos c.caps;
          let pk' = "Fn#" ^ c.cname in
          pack_register ~reg ~concrete:true pk' c (subst s want);
          Hashtbl.replace r.renamed pk pk'
      | Spawn (i, sp) ->
          let c, _ = Hashtbl.find r.lams i in
          let sp' = "spawn#" ^ c.cname in
          spawn_register ~reg ~concrete:true sp' c;
          Hashtbl.replace r.renamed sp sp')
    reqs;
  r

(* Spec 的 Lam 都在（不是传下去的 fn(…) 参数）：实例里总是这样，fn(…) 参数只出现在带 fn(…) 参数的函数里，它们不生成实例。 *)
let supported src = List.for_all (function Spec (_, _, _, _, ids) -> not (List.mem 0 ids) | _ -> true) src.reqs

(* 实例的语法树。alias：内建函数别名（干跑时加上 replay 算出的）。c：实例里 #pN 从哪里往后数。 *)
let build ~alias ~c src (r : replayed) =
  let s = src.s in
  let spec_of callee =
    let n = String.length callee in
    if n > 5 && String.sub callee 0 5 = "spec#" then int_of_string_opt (String.sub callee 5 (n - 5)) else None
  in
  let call (k : T.call) =
    match spec_of k.callee with
    | Some i ->
        (* 带 fn(…) 参数的函数：调用特化，去掉 fn(…) 参数的占位，后面接上每个闭包的捕获（外层变量本身）。
           约定就是特化的签名（spec_register 登记的）：f 的非 fn(…) 参数，然后是捕获；干跑时签名还没登记，所以直接算 *)
        let name, ps, cls = Hashtbl.find r.specs i in
        let fn_pos i = is_fn (snd (List.nth ps i)) in
        let keep = List.filteri (fun i _ -> not (fn_pos i)) k.args in
        let caps =
          List.concat_map
            (fun (_, (l : lam)) ->
              List.map (fun (_, v, t) -> { T.e = T.Var v; ty = T.Ty (subst s t); pos = l.lexpr.pos }) l.lcaps)
            cls
        in
        let modes =
          List.filter_map (fun (m, p) -> if is_fn p then None else Some m) ps
          @ List.concat_map (fun ((c : closure), _) -> List.map (fun (m, _, _) -> m) c.caps) cls
        in
        { T.callee = name; base = ""; targs = []; modes; borrows = []; args = keep @ caps }
    | None when Hashtbl.mem r.renamed k.callee -> { k with callee = Hashtbl.find r.renamed k.callee }
    | None when k.base = "" -> k
    | None -> (
        match call_node ~alias k.base (Some (canon_regions (List.map (subst s) k.targs))) k.args with
        | T.Call k -> k
        | _ -> assert false)
  in
  { (map_fn ~ty:(subst s) ~var:(renum src.pm0 c) ~call src.fd) with name = src.name }

(* 干跑：实例的语法树和会登记的东西，不改 pending、requested、pm_count 和别的表。 *)
let dry ~define r =
  match source ~define r with
  | Some src when supported src ->
      let out = ref [] and extra = Hashtbl.create 4 in
      let rp = replay ~reg:(Dry (Hashtbl.create 8, out)) ~alias:extra src src.reqs in
      let alias g = match Hashtbl.find_opt aliases g with Some b -> Some b | None -> Hashtbl.find_opt extra g in
      Some (build ~alias ~c:!pm_count src rp, List.rev !out)
  | _ -> None

(* 实例 r：先按顺序登记它用到的实例、Vec 内建函数、提升出的闭包、特化（内建类型的 to_str 的别名也在这时加上），
   pm_count 往前走泛型函数检查时用掉的那么多，再建语法树。 *)
let instantiate ~define r =
  match source ~define r with
  | Some src when supported src ->
      let rp = replay ~reg:Real ~alias:(Hashtbl.create 1) src src.reqs in
      let c = !pm_count in
      pm_count := c + (src.pm1 - src.pm0);
      build ~alias:(Hashtbl.find_opt aliases) ~c src rp
  | _ -> failwith ("单态化不了实例 " ^ inst_name (fst r) (snd r))

(* ---------- 对照 ---------- *)

let total = ref 0
let unsupported = ref 0
let same = ref 0
let same_erased = ref 0
let failed = ref []      (* 重新检查报错（单态化会接受）的实例 *)
let differ = ref []      (* (实例, 哪里不同) *)

(* 区域名都换成 _ 之后比较（C 里没有区域）。 *)
let erased fd =
  let e = Defs.erase defs in
  map_fn ~ty:e ~var:Fun.id ~call:(fun k -> { k with targs = List.map e k.targs }) fd

(* 第一个不同的语句的位置。 *)
let where (a : T.fn_def) (b : T.fn_def) =
  if a.params <> b.params then "参数"
  else if a.ret <> b.ret then "返回类型"
  else
    let rec go (xs : T.stmt list) (ys : T.stmt list) =
      match xs, ys with
      | x :: xs, y :: ys -> if x = y then go xs ys else Printf.sprintf "第 %d 行的语句" x.spos.line
      | [], [] -> "块的值"
      | _ -> "语句的条数"
    in
    go a.body.stmts b.body.stmts

(* 实例 r 的语法树（单态化）。
   对照（SINTER_MONO=check）时改成复制泛型函数的 Ast（Generics.instance）、用 check 重新检查，
   另外干跑单态化，和检查的结果比较。 *)
let instance_fn ~define check r =
  if not checking then Some (instantiate ~define r)
  else begin
    let it = instance r in
    incr total;
    (* 单态化在这里也可能报错（任务、Fn(…) 闭包的捕获换成实例的类型后含引用）：和重新检查报的错比较 *)
    let m = try Ok (dry ~define r) with Diag.Error e -> Error e in
    trace := [];
    match check it with
    | Some fd as res ->
        (match m with
        | Ok None -> incr unsupported
        | Ok (Some (mf, mt)) ->
            let tr = List.rev !trace in
            if mf = fd && mt = tr then incr same
            else if erased mf = erased fd && mt = tr then incr same_erased
            else differ := (fd.T.name, if mt <> tr then "登记的实例" else where mf fd) :: !differ
        | Error _ -> differ := (fd.T.name, "单态化报错，重新检查没有") :: !differ);
        res
    | None ->
        incr unsupported;
        None
    | exception (Diag.Error e as ex) ->
        (match m with
        | Ok None -> incr unsupported
        | Error e' when e' = e -> incr same
        | _ -> failed := inst_name (fst r) (snd r) :: !failed);
        raise ex
  end

let () =
  if checking then
    at_exit (fun () ->
        Printf.eprintf "mono 对照：实例 %d 个，不支持 %d，相同 %d，只差区域名 %d，不同 %d，重新检查报错 %d\n" !total
          !unsupported !same !same_erased (List.length !differ) (List.length !failed);
        List.iteri
          (fun i (n, w) -> if i < 10 then Printf.eprintf "mono 不同：%s（%s）\n" n w)
          (List.rev !differ);
        List.iteri (fun i n -> if i < 10 then Printf.eprintf "mono 重新检查报错：%s\n" n) (List.rev !failed))
