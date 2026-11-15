(* 类型检查：结构体按名字区分，泛型按实例化后的名字区分（见 generics.ml）；`return` 之后的代码类型记为 Never。
   `Ok(e)` / `Err(e)` 的类型要靠上下文确定（let 标注、返回值、实参、字段），
   所以有两种检查方式：expr 自己推出类型，check 按期望类型检查。

   拆成了几个文件，按依赖的顺序：ctx（环境；状态在 check_state.ml）、builtin、region、generics、
   pattern、check_expr，这里是整个程序的准备和检查。结果是带类型的语法树（Tast），另外 generated 交给代码生成。 *)
open Ast
include Ctx
include Builtin
include Region
include Generics
include Pattern
include Check_expr

(* 整个程序的准备：登记和检查结构体、枚举（含泛型定义）、函数签名、trait 和 impl、字段和载荷（见 collect.ml）。
   返回检查函数体用的环境。 *)
let prepare = Collect.run

(* 把函数里写出的类型换成解析后的：签名取 prepare 解析好的，函数体里的 let 标注、var 声明按类型参数解析。
   泛型函数另外记下来，实例化时复制。 *)
let resolve_item = function
  | Fn f ->
      let ps, ret = Hashtbl.find sigs f.name in
      let sc = List.map (fun (t, lin) -> (t, TParam (t, lin))) f.tparams in
      let it =
        Fn
          {
            f with
            params = List.map2 (fun (m, x, _) (_, t) -> (m, x, t)) f.params ps;
            ret;
            body = map_body ~fresh:false (resolve sc) f.body;
          }
      in
      if f.tparams <> [] then Hashtbl.replace gfns f.name it;
      it
  | it -> it

(* 整个程序的条目逐个解析：impl 换成它的方法（普通函数，名字是 trait.方法<类型>，Self 换成实现的类型）；
   泛型 impl 的方法是泛型函数，类型参数和约束是 impl 的。
   trait 的签名已经登记过，不再留在程序里；默认方法是泛型函数 trait.方法，类型参数 Self、约束 Self: trait。 *)
let resolve_items = function
  | Fn _ as it -> [ resolve_item it ]
  | Impl { trait = tr; tparams; bounds; self; fns; ipos } ->
      let tsc = List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams in
      let st = resolve tsc ipos self in
      List.filter_map
        (function
          | Fn f ->
              let name = inst_name (trait_key tr f.name) [ st ] in
              let ps, ret = Hashtbl.find sigs name in
              let it =
                Fn
                  {
                    f with
                    name;
                    tparams;
                    bounds;
                    params = List.map2 (fun (m, x, _) (_, t) -> (m, x, t)) f.params ps;
                    ret;
                    body = map_body ~fresh:false (resolve (("Self", st) :: tsc)) f.body;
                  }
              in
              if tparams <> [] then Hashtbl.replace gfns name it;
              Some it
          | Struct _ | Enum _ | Trait _ | Impl _ -> None)
        fns
  | Trait { name = tr; methods = ms; _ } ->
      List.filter_map
        (fun (m : msig) ->
          Option.map
            (fun body ->
              resolve_item
                (Fn
                   {
                     name = trait_key tr m.mname;
                     tparams = [ ("Self", true) ];
                     bounds = [ ("Self", tr) ];
                     params = m.mparams;
                     ret = m.mret;
                     body;
                     prim = None;
                     ipos = m.mpos;
                   }))
            m.mbody)
        ms
  | (Struct _ | Enum _) as it -> [ it ]

(* 检查一个函数，返回它的带类型的语法树。特化：clos 是 fn(…) 参数绑定的闭包，fvars 是这些参数（已经从参数里去掉，函数体里仍按名字用）。
   rord：闭包捕获的区域参数之间的先后关系（`Closures.CapRef`）；其余区域参数之间没有顺序（D3）。 *)
let check_fn ?(clos = []) ?(fvars = []) ?(rord = []) env0 = function
  | Fn { name; tparams; params; ret; body; ipos; bounds; _ } ->
      let regions = List.map (fun r -> (r, Option.value (List.assoc_opt r rord) ~default:[])) (fn_regions params ret) in
      let cur =
        {
          fn_name = name;
          concrete = tparams = [] && not (List.exists (fun (_, _, t) -> is_fn t) params);
          let_params = List.filter_map (fun (m, x, _) -> if m = MLet then Some x else None) params;
          bind_regions = [];
        }
      in
      let env1 = { env0 with cur; regions; loop_depth = 0; lambda_frame = None; scopes = [] } in
      List.iter
        (fun (m, x, t) ->
          check_ty ~param:true env1 ipos t;
          (match t with
          | TArena _ when m <> MLet -> err ipos "区域参数 %s 只能按 let 传入" x
          | TFn _ when m <> MLet -> err ipos "函数类型的参数 %s 只能按 let 传入（不写约定）" x
          | _ -> ()))
        params;
      check_ty env1 ipos ret;
      let vars = List.rev_map (fun (m, x, t) -> (x, (t, m <> MLet))) params in
      let vars = List.map (fun (y, t) -> (y, (t, false))) fvars @ vars in
      let env = { env1 with ret; vars; tbounds = bounds; clos } in
      let body, t = block env ~want:ret body in
      expect env ipos ret t;
      Some { Tast.name; params; ret; body; pos = ipos; from_core = Resolve.in_core ipos }
  | Struct _ | Enum _ | Trait _ | Impl _ -> None

(* 泛型函数检查完，连同检查时按顺序发生的请求和 pm_count 记下来（单态化用，见 elab/mono.ml）。 *)
let check_fn ?clos ?fvars ?rord env0 it =
  requests := [];
  let pm0 = !pm_count in
  let fd = check_fn ?clos ?fvars ?rord env0 it in
  (match it, fd with
  | Fn { name; tparams = _ :: _; _ }, Some fd -> Hashtbl.replace generic_fns name (fd, List.rev !requests, pm0, !pm_count)
  | _ -> ());
  fd

let check_main env0 =
  match Hashtbl.find_opt env0.fns "main" with
  | Some ([], TUnit) when not (is_generic "main") -> ()
  | Some _ -> err Source.start "main 必须是 fn main()，没有参数、没有返回值、没有类型参数"
  | None -> err Source.start "缺少 fn main()"

(* fn to_str(x: P) -> Str 是 impl ToStr for P { … } 的简写（可以给多种类型各写一个），
   fn to_str<T: ToStr>(v: Vec<T>) -> Str 是泛型 impl 的简写。
   模块里的 m.to_str(x: P) -> Str 照常是函数，另外为 P 实现 ToStr（调用它），"{x}" 也就能用了。 *)
let sugar_to_str (p : program) =
  let suffix = ".to_str" in
  let qualified n =
    let k = String.length n and l = String.length suffix in
    k > l && String.sub n (k - l) l = suffix
  in
  List.concat_map
    (function
      | Fn ({ name = "to_str"; tparams; bounds; params; ret; ipos; _ } as f) -> (
          match params, ret with
          | [ (MLet, _, self) ], TStr ->
              [ Impl { trait = "ToStr"; tparams; bounds; self; fns = [ Fn { f with tparams = []; bounds = [] } ]; ipos } ]
          | _ -> err ipos "to_str 是 trait ToStr 的方法，要写成 fn to_str(x: T) -> Str")
      | Fn ({ name; tparams = []; params = [ (MLet, x, self) ]; ret = TStr; ipos; _ } as f) as it when qualified name ->
          let call = { e = Call (name, [ { e = Var x; pos = ipos; id = fresh_id () } ]); pos = ipos; id = fresh_id () } in
          let body = { stmts = []; tail = Some call } in
          [ it; Impl { trait = "ToStr"; tparams = []; bounds = []; self; fns = [ Fn { f with name = "to_str"; body } ]; ipos } ]
      | it -> [ it ])
    p

(* 要降级的函数：检查过的函数里，带 fn(…) 参数的函数本身不降级（只降级它的特化），
   函数体里有闭包的泛型函数不在定义处降级。 *)
let lowered = function
  | Fn f, _ when List.exists (fun (_, _, t) -> is_fn t) f.params -> None
  | Fn f, _ when f.tparams <> [] && Hashtbl.mem skip_lower f.name -> None
  | _, fd -> fd

(* 非泛型的结构体、枚举，按程序里的顺序（代码生成先按这个顺序写类型定义）。 *)
let type_names (p : program) =
  List.filter_map
    (function
      | Struct { name; tparams = []; _ } | Enum { name; tparams = []; _ } -> Some name
      | Struct _ | Enum _ | Fn _ | Trait _ | Impl _ -> None)
    p

(* 检查整个程序。返回要降级的函数（impl 换成它的方法），后面接上用到的泛型函数实例：泛型函数只检查一次，
   实例由它的语法树单态化得到（elab/mono.ml），不再检查。
   闭包：提升出的闭包和特化也接在后面、照常检查（特化复制带 fn(…) 参数的函数的 Ast）。 *)
let program (res : Resolved.t) : Tast.program =
  let p = sugar_to_str res.Resolved.items in
  let env0 = prepare { res with Resolved.items = p } in
  let p = List.concat_map resolve_items p in
  List.iter (function Fn f as it -> Hashtbl.replace fn_items f.name it | _ -> ()) p;
  (* 函数之间互不影响，每个函数报它的第一个错误；有错误时不再生成泛型实例（免得同一个错误报好几次） *)
  let errs = ref [] in
  let guard g x = try g x with Diag.Error e -> errs := e :: !errs; None in
  let checked = List.map (fun it -> (it, guard (check_fn env0) it)) p in
  ignore (guard (fun env0 -> check_main env0; None) env0);
  if !errs <> [] then raise (Diag.Errors (List.rev !errs));
  (* 泛型函数的实例（单态化，elab/mono.ml），提升出的闭包和特化（照常检查）：都要降级，按生成的顺序 *)
  let rec drain acc =
    if List.length acc >= 1000 then
      err Source.start "泛型函数的实例超过 1000 个（可能是多态递归）";
    match Queue.take_opt pending with
    | Some r -> drain (guard (Mono.instance_fn ~define:(fun it -> ignore (check_fn env0 it)) (check_fn env0)) r :: acc)
    | None -> (
        match Queue.take_opt pending_items with
        | Some (it, clos, fvars, rord) -> drain (guard (check_fn ~clos ~fvars ~rord env0) it :: acc)
        | None -> List.rev acc)
  in
  let insts = drain [] in
  if !errs <> [] then raise (Diag.Errors (List.rev !errs));
  { Tast.types = type_names p; fns = List.filter_map lowered checked @ List.filter_map Fun.id insts }

(* 对照测试（--batch）：程序里的函数逐个检查，每个连同它引出的泛型实例、提升出的闭包和特化一起，
   返回要降级的函数（规则和 program 相同）。出错时清空队列再抛出，不影响下一个函数。
   调用前要用 batch_items 登记所有函数（特化时按名字找函数）。 *)
let batch_items (p : program) =
  let p = List.map resolve_item p in
  List.iter (function Fn f as it -> Hashtbl.replace fn_items f.name it | _ -> ()) p;
  p

let check_batch env0 it : Tast.fn_def list =
  try
    let first = (it, check_fn env0 it) in
    let rec drain acc =
      match Queue.take_opt pending with
      | Some r -> drain (Mono.instance_fn ~define:(fun it -> ignore (check_fn env0 it)) (check_fn env0) r :: acc)
      | None -> (
          match Queue.take_opt pending_items with
          | Some (i, clos, fvars, rord) -> drain (check_fn ~clos ~fvars ~rord env0 i :: acc)
          | None -> List.rev acc)
    in
    let insts = drain [] in
    List.filter_map lowered [ first ] @ List.filter_map Fun.id insts
  with e ->
    Queue.clear pending;
    Queue.clear pending_items;
    raise e

(* 代码生成要为它们写 C 辅助函数的东西：检查时记在表里的闭包、Fn 调用、任务、Vec 内建函数的实例，
   列表的顺序和以前代码生成遍历这些表的顺序相同（生成的 C 不变）。
   vec_pop 返回的 Option<e> 和运行时总要用的 Option<Str>、Option<I64>、Option<F64>、Option<Char> 在这里实例化，
   代码生成就不再往 Defs 里加东西。所有函数降级、检查完之后调用。 *)
let generated () : Tast.generated =
  let list tbl = List.rev (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []) in
  let vec_calls = list vec_calls in
  let elems = List.filter_map (fun (_, (b, e)) -> if b = "pop" then Some e else None) vec_calls in
  {
    Tast.packs = list packs;
    fcalls = list fcalls;
    spawns = list spawns;
    vec_calls;
    options = List.map (fun e -> (e, instantiate "Option" [ e ])) (elems @ [ TStr; TI64; TF64; TChar ]);
  }
