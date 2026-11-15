(* 泛型、trait 和 impl 的查找、协变、类型实参的推断和实例化。 *)
open Ast
open Ctx
open Builtin
open Region

(* ---------- 泛型（Lean `Generics.Check`） ----------
   泛型结构体和枚举按用到的类型实参实例化成具体的类型，名字就是写法本身（例如 "Option<I64>"），
   登记到 struct_tbl / enum_tbl，之后的检查、降级、代码生成都把它当普通类型。
   泛型函数里的类型参数记作 TParam，按抽象类型检查：值只能移动，普通的 T 可以自动析构，
   T: linear 必须交出去；不能对 T 做运算、取字段或 match。调用处推出类型实参并检查约束，
   用到的每个实例由泛型函数的带类型的语法树换成具体类型得到（单态化，elab/mono.ml）。 *)

(* trait（Lean `Traits.Static`）：每个方法当作带一个类型参数 Self 的泛型函数，Self 换成实现的类型。
   方法的函数名是 trait.方法（例如 Ord.cmp），不占用全局的名字：只能用方法调用 x.cmp(y) 调用，
   按接收者的类型在约束和 impl 里找（Lean `Traits.Inherent` 的 resolve_tra）。
   traits：trait → 方法名；methods：方法的函数名 → trait；tmethods：方法名 → (trait, 函数名)；impls：trait → 它的每个 impl；
   gbounds：泛型函数（和方法）的类型参数要满足的 trait 约束。
   impl 里的方法是普通的具体函数，名字记作 trait.方法<类型>（和泛型函数的实例同一种写法），单态化时直接调用它。
   泛型 impl（Lean `Traits.ImplSearch`）：impl 可以是泛型的（头部 Vec<T>，约束 T: ToStr），它的方法是以 impl 的类型参数为类型参数的泛型函数，
   名字是 trait.方法<头部>；trait 的方法可以有默认的函数体（defaults），它是泛型函数 trait.方法（类型参数 Self）。 *)

(* trait tr 的方法 m 的函数名；报错时只显示方法名。 *)
let trait_key tr m = tr ^ "." ^ m

let bare f =
  match Hashtbl.find_opt methods f with
  | Some tr -> String.sub f (String.length tr + 1) (String.length f - String.length tr - 1)
  | None -> f

let inst_name g ts = Ast.app_name g (List.map string_of_ty ts)

(* ---------- 可以存起来的闭包 Fn(…)（Lean `Closures.Stored`） ----------
   写在需要 Fn(pts) -> r 的地方（let 标注、返回值、实参、字段、Vec 元素）的闭包：和当实参传的闭包一样提升成普通函数
   （捕获在前，只读的按 let、修改的按 inout），闭包表达式换成调用内建的 Fn#提升出的函数名(捕获…)，
   捕获按 sink 传入（按值：会移动的值被移走，可复制的值复制），存进堆上的环境。调用 f(x) 换成 call#类型(f, x)，
   闭包按 inout：闭包体访问的环境和实参不重叠（`Closures.Stored` 的 env_disjoint），所以提升出的函数照常检查就够了。
   Fn 值需要析构（析构环境），只能移动；环境里不能有引用、区域和 fn(…) 参数（`Closures.Stored` 的 ok：存起来也不会悬空）。
   packs：Fn#… → (闭包, 它的类型)；fcalls：call#… → 闭包的类型。代码生成为它们写 C 函数。 *)

let inst_of n = Option.map fst (Hashtbl.find_opt inst_tbl n)

let is_generic f = Hashtbl.mem gparams f

let rec has_arena = function
  | TArena _ -> true
  | TBox t | TVec t | TRef (t, _) -> has_arena t
  | TResult (a, b) -> has_arena a || has_arena b
  | _ -> false

(* 类型实参的共同限制。泛型函数和 Vec 的类型实参可以含引用：实例的区域参数就是实参里的区域名（Lean `Regions.Generic`）；
   可以是 ()：() 是 copy 类别，换进任何类型参数都满足约束，实例照样安全（Lean `Generics.UnitArg`）。 *)
let type_arg pos t =
  if has_arena t then err pos "类型实参不能是 Arena"

let lin_arg pos t =
  if is_linear t then err pos "泛型结构体、枚举的类型实参暂不能是 linear 值（%s）" (string_of_ty t)

(* 泛型结构体、枚举的类型实参可以含引用（Lean `Regions.Data`），还不能是 linear 值（字段和载荷的 linear 规则还没有和泛型一起证明）。 *)
let data_arg pos t =
  type_arg pos t;
  lin_arg pos t

(* 解析类型：sc 把类型参数名映射到类型（泛型函数里是 TParam，实例化时是类型实参）；
   Option<I64> 这样的写法换成实例。 *)
let rec resolve sc pos = function
  | TStruct n when List.mem_assoc n sc -> List.assoc n sc
  | TStruct "Vec" when not (List.mem_assoc "Vec" sc) -> err pos "Vec 要写元素类型：Vec<…>"
  | TStruct n when Hashtbl.mem gtypes n -> err pos "%s 是泛型类型，要写类型实参：%s<…>" n n
  | TApp ("Vec", ts) -> (
      (* 内建的动态数组：元素不能是引用或 linear 值（pop 返回 Option<T>，Option 的实参还不能是 linear 的） *)
      match List.map (resolve sc pos) ts with
      | [ t ] ->
          type_arg pos t;
          lin_arg pos t;
          TVec t
      | ts -> err pos "Vec 需要 1 个类型实参，实际给了 %d 个" (List.length ts))
  | TApp (n, ts) -> (
      match Hashtbl.find_opt gtypes n with
      | None -> err pos "%s 不是泛型类型，不能写类型实参" n
      | Some (ps, _) ->
          if List.length ps <> List.length ts then
            err pos "%s 需要 %d 个类型实参，实际给了 %d 个" n (List.length ps) (List.length ts);
          let ts = List.map (resolve sc pos) ts in
          List.iter (data_arg pos) ts;
          instantiate n ts)
  | TBox t -> TBox (resolve sc pos t)
  | TVec t -> TVec (resolve sc pos t)
  | TResult (a, b) -> TResult (resolve sc pos a, resolve sc pos b)
  | TRef (t, r) -> TRef (resolve sc pos t, r)
  | TFn (ps, r) -> TFn (List.map (fun (m, t) -> (m, resolve sc pos t)) ps, resolve sc pos r)
  | TOFn (ps, r) -> TOFn (List.map (fun (m, t) -> (m, resolve sc pos t)) ps, resolve sc pos r)
  | t -> t

and instantiate g ts =
  let name = inst_name g ts in
  if not (Hashtbl.mem inst_tbl name) then begin
    Hashtbl.replace inst_tbl name (g, ts);
    if Hashtbl.length inst_tbl > 1000 then
      err Source.start "泛型类型的实例超过 1000 个（可能是类型实参越来越大的递归类型，例如 W<T> 里有 Box<W<Box<T>>>）";
    let ps, it = Hashtbl.find gtypes g in
    let sc = List.combine ps ts in
    match it with
    | Struct { fields; linear; ipos; _ } ->
        Hashtbl.replace struct_tbl name
          (List.map (fun (f : field) -> { f with fty = resolve sc ipos f.fty }) fields);
        if linear then Hashtbl.replace linear_tbl name ()
    | Enum { variants; ipos; _ } ->
        Hashtbl.replace enum_tbl name
          (List.map (fun (v, t) -> (v, Option.map (resolve sc ipos) t)) variants)
    | Fn _ | Trait _ | Impl _ -> ()
  end;
  TStruct name

let () = instantiate_fwd := instantiate

(* 把类型参数换成类型（一次换完，换进去的类型不再替换）；实例里的类型参数也一起换。 *)
let rec subst s = function
  | TParam (n, _) as t -> Option.value (List.assoc_opt n s) ~default:t
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) -> instantiate g (List.map (subst s) ts)
      | None -> t)
  | TBox t -> TBox (subst s t)
  | TVec t -> TVec (subst s t)
  | TResult (a, b) -> TResult (subst s a, subst s b)
  | TRef (t, r) -> TRef (subst s t, r)
  | TFn (ps, r) -> TFn (List.map (fun (m, t) -> (m, subst s t)) ps, subst s r)
  | TOFn (ps, r) -> TOFn (List.map (fun (m, t) -> (m, subst s t)) ps, subst s r)
  | t -> t

(* 类型里出现的类型参数。 *)
let rec params_of = function
  | TParam (n, _) -> [ n ]
  | TStruct n -> (
      match Hashtbl.find_opt inst_tbl n with Some (_, ts) -> List.concat_map params_of ts | None -> [])
  | TBox t | TVec t | TRef (t, _) -> params_of t
  | TResult (a, b) -> params_of a @ params_of b
  | TFn (ps, r) | TOFn (ps, r) -> List.concat_map (fun (_, t) -> params_of t) ps @ params_of r
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TArena _ | TApp _ -> []

(* ---- impl 的查找（Lean `Traits.ImplSearch`） ---- *)

(* 泛型类型的实例 Pair<I64> 记作名字，这里拆回 (Pair, [I64])；元组也一样。 *)
let app_of n = Hashtbl.find_opt inst_tbl n

(* 头部 p 对类型 t 的匹配（`Traits.ImplSearch` 的 mtch）：vs 是 impl 的类型参数，s 是已经匹配出的类型实参。
   t 里的类型参数（当前泛型函数的）当作普通的类型。loose：不比较区域（类型自己的方法的头部可以是 &str in a）。 *)
let rec tmatch ?(loose = false) vs p t s =
  match p, t with
  | TParam (n, _), _ when List.mem n vs -> (
      match List.assoc_opt n s with Some u -> if u = t then Some s else None | None -> Some ((n, t) :: s))
  | TRef (p, _), TRef (t, _) when loose -> tmatch ~loose vs p t s
  | TStruct a, TStruct b -> (
      match app_of a, app_of b with
      | Some (g, ps), Some (h, ts) when g = h && List.length ps = List.length ts -> tmatch_list ~loose vs ps ts s
      | None, None when a = b -> Some s
      | _ -> None)
  | TBox p, TBox t | TVec p, TVec t -> tmatch ~loose vs p t s
  | TResult (a, b), TResult (c, d) -> tmatch_list ~loose vs [ a; b ] [ c; d ] s
  | _ -> if p = t then Some s else None

and tmatch_list ~loose vs ps ts s =
  match ps, ts with
  | [], [] -> Some s
  | p :: ps, t :: ts -> Option.bind (tmatch ~loose vs p t s) (tmatch_list ~loose vs ps ts)
  | _ -> None

(* 两个头部可能有共同的实例（`Traits.ImplSearch` 的 compat，保守：类型参数和什么都相容）。loose 同上。 *)
let rec compat ?(loose = false) p q =
  match p, q with
  | TParam _, _ | _, TParam _ -> true
  | TRef (p, _), TRef (q, _) when loose -> compat ~loose p q
  | TStruct a, TStruct b -> (
      match app_of a, app_of b with
      | Some (g, ps), Some (h, qs) -> g = h && List.length ps = List.length qs && List.for_all2 (compat ~loose) ps qs
      | None, None -> a = b
      | _ -> false)
  | TBox p, TBox q | TVec p, TVec q -> compat ~loose p q
  | TResult (a, b), TResult (c, d) -> compat ~loose a c && compat ~loose b d
  | _ -> p = q

(* 接收者类型 t 的方法 m（`Traits.Inherent` 的 pickM）：CoherentM 保证至多一个头部能匹配，和登记的顺序无关。 *)
let find_inherent m t =
  List.find_map
    (fun (h, vs, g) -> Option.map (fun _ -> g) (tmatch ~loose:true vs h t []))
    (Hashtbl.find_all inherent_heads m)

(* 头部能匹配 t 的 tr 的 impl（Coherent 保证至多一个），和匹配出的类型实参。 *)
let find_impl tr t =
  List.find_map
    (fun i -> Option.map (fun s -> (i, s)) (tmatch i.itps i.head t []))
    (Hashtbl.find_all impls tr)

(* t: tr 成立吗（`Traits.ImplSearch` 的 solve）：类型参数查当前函数的约束 tb，否则找 impl，再查它的约束。
   impl 的头部不是单独的类型参数，约束里的类型都是 t 的真子项，所以递归一定会停。 *)
let rec has_impl tb tr t =
  match t with
  | TParam (m, _) -> List.mem (m, tr) tb
  | _ -> (
      match find_impl tr t with
      | Some (i, s) -> List.for_all (fun (n, tr') -> has_impl tb tr' (List.assoc n s)) i.ibounds
      | None -> false)

(* t: tr 不成立时，找出最里面不成立的那个约束（报错用）。 *)
let rec why_not tb tr t =
  match t with
  | TParam _ -> (tr, t)
  | _ -> (
      match find_impl tr t with
      | Some (i, s) -> (
          match List.find_opt (fun (n, tr') -> not (has_impl tb tr' (List.assoc n s))) i.ibounds with
          | Some (n, tr') -> why_not tb tr' (List.assoc n s)
          | None -> (tr, t))
      | None -> (tr, t))

(* 协变（Lean `Regions.DataVariance`）：泛型结构体、枚举 g 的第 i 个类型参数只出现在只读的位置时，
   g<…外层…> 可以当作 g<…内层…> 用。只读的位置：字段、载荷直接是这个参数，
   或者在 Result 里，或者是另一个实例里能协变的类型实参。
   Box、Vec 里的保守地算不变（实例之间不比较）；可以修改的 inout 实参本来就要求类型完全相同。 *)
let rec covariant g i =
  match Hashtbl.find_opt cov_memo (g, i) with
  | Some b -> b
  | None ->
      (* 先记一个结果，防止递归的类型（经过自己）死循环 *)
      Hashtbl.replace cov_memo (g, i) true;
      let ps, _ = Hashtbl.find gtypes g in
      let p = List.nth ps i in
      let abs = match instantiate g (List.map (fun q -> TParam (q, false)) ps) with TStruct n -> n | _ -> g in
      let tys =
        match Hashtbl.find_opt struct_tbl abs, Hashtbl.find_opt enum_tbl abs with
        | Some fs, _ -> List.map (fun (f : field) -> f.fty) fs
        | None, Some vs -> List.filter_map snd vs
        | None, None -> []
      in
      let b = List.for_all (only_cov p) tys in
      Hashtbl.replace cov_memo (g, i) b;
      b

(* 类型参数 p 在 t 里只出现在只读的位置（或者不出现）。 *)
and only_cov p t =
  match t with
  | TParam _ -> true
  | TResult (a, b) -> only_cov p a && only_cov p b
  | TStruct n -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) ->
          List.for_all Fun.id
            (List.mapi (fun i t -> (not (List.mem p (params_of t))) || (covariant g i && only_cov p t)) ts)
      | None -> true)
  | TBox _ | TVec _ | TRef _ | TFn _ | TOFn _ -> not (List.mem p (params_of t))
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TArena _ | TApp _ -> true

let () = covariant_fwd := covariant

(* 推断类型实参：模式 p 里的类型参数按 t 里对应位置的类型确定，先确定的为准。 *)
let rec unify s p t =
  match p, t with
  | TParam (n, _), t -> if not (List.mem_assoc n !s) then s := (n, t) :: !s
  | TBox p, TBox t | TVec p, TVec t | TRef (p, _), TRef (t, _) -> unify s p t
  | TResult (a, b), TResult (c, d) -> unify s a c; unify s b d
  | TFn (ps, r), (TFn (qs, u) | TOFn (qs, u)) | TOFn (ps, r), TOFn (qs, u) when List.length ps = List.length qs ->
      List.iter2 (fun (_, p) (_, q) -> unify s p q) ps qs;
      unify s r u
  | TStruct a, TStruct b -> (
      match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
      | Some (g, ps), Some (h, ts) when g = h -> List.iter2 (unify s) ps ts
      | _ -> ())
  | _ -> ()

(* 调用 Fn(…) 类型 t 的闭包的内建函数 call#t：闭包按 inout，其余参数按类型里写的约定。
   fcall_register 登记（第一次时），fcall_name 另外记下请求（单态化按实例的类型重新登记）。 *)
let fcall_register (fns : (string, (mode * ty) list * ty) Hashtbl.t) t =
  match t with
  | TOFn (pts, r) ->
      let n = "call#" ^ string_of_ty t in
      if not (Hashtbl.mem fns n) then begin
        let s = ((MInout, t) :: pts, r) in
        Hashtbl.replace sigs n s;
        Hashtbl.replace fns n s
      end;
      if params_of t = [] && not (Hashtbl.mem fcalls n) then begin
        trace := ("fcall " ^ n) :: !trace;
        Hashtbl.replace fcalls n t
      end;
      n
  | _ -> assert false

let fcall_name fns t =
  let n = fcall_register fns t in
  requests := Fcall (fns, t, n) :: !requests;
  n

(* 改写函数体里写出的类型（let 标注、var 声明），方法调用换成解析出的函数名（检查过的才有：复制泛型函数的实例时）。
   fresh：表达式换新的 id（复制出实例、特化、提升出的闭包时用）。 *)
let map_body ~fresh (ft : pos -> ty -> ty) (b : block) : block =
  let rec ex (x : expr) =
    let e =
      match x.e with
      | (Int _ | Float _ | Char _ | Bool _ | Unit | Str _ | Var _) as d -> d
      | Binop (o, a, b) -> Binop (o, ex a, ex b)
      | Unop (o, a) -> Unop (o, ex a)
      | Call (f, args) -> Call (callee x f, List.map ex args)
      | StructLit (n, fs) -> StructLit (n, List.map (fun (f, e) -> (f, ex e)) fs)
      | Field (a, f) -> Field (ex a, f)
      | Deref a -> Deref (ex a)
      | Try a -> Try (ex a)
      | Variant (v, a) -> Variant (v, Option.map ex a)
      | Match (bw, s, arms) -> Match (bw, ex s, List.map (fun (v, y, e) -> (v, y, ex e)) arms)
      | PMatch (bw, s, arms) -> PMatch (bw, ex s, List.map (fun (q, e) -> (q, ex e)) arms)
      | Arena (r, b) -> Arena (r, blk b)
      | If (c, a, b) -> If (ex c, blk a, Option.map blk b)
      | Block b -> Block (blk b)
      | Lambda (ps, a) -> Lambda (ps, ex a)
    in
    { x with e; id = (if fresh then fresh_id () else x.id) }
  and st (s : stmt) =
    let d =
      match s.s with
      | Let (m, v, t, e) -> Let (m, v, Option.map (ft s.spos) t, ex e)
      | Decl (v, t) -> Decl (v, ft s.spos t)
      | Assign (v, e) -> Assign (v, ex e)
      | SetField (a, f, e) -> SetField (ex a, f, ex e)
      | SetPlace (a, e) -> SetPlace (ex a, ex e)
      | While (c, b) -> While (ex c, blk b)
      | For (a, s1, s2) -> For (ex a, st s1, st s2)
      | Scope (a, b) -> Scope (ex a, blk b)
      | Return e -> Return (Option.map ex e)
      | Expr e -> Expr (ex e)
      | Errdefer e -> Errdefer (ex e)
      | (Break | Continue) as d -> d
      | Destruct (n, bs, e) -> Destruct (n, bs, ex e)
    in
    { s with s = d }
  and blk (b : block) = { stmts = List.map st b.stmts; tail = Option.map ex b.tail } in
  blk b

(* Vec 的内建函数按元素类型实例化：降级时调用名换成 vec_push<T> 这样的实例名；
   元素类型是具体类型时登记，代码生成为它写辅助函数。vec_inst 另外记下请求，返回调用节点的类型实参。 *)
let vec_request b t =
  if params_of t = [] then begin
    let n = inst_name ("vec_" ^ b) [ t ] in
    trace := ("vec " ^ n) :: !trace;
    Hashtbl.replace vec_calls n (b, t)
  end

let vec_inst b t =
  requests := Vec (b, t) :: !requests;
  vec_request b t;
  Some [ t ]

(* 泛型函数 f 按类型实参 ts 的 Ast 副本：复制函数体（表达式换新的 id），类型参数换成具体类型。
   给带 fn(…) 参数的泛型函数的特化用（特化照常检查），对照模式（SINTER_MONO=check）也用它重新检查实例。
   泛型 impl 的方法：方法<具体类型> 是 方法<头部> 按匹配出的类型实参的实例。 *)
let instance (f, ts) =
  let name = inst_name f ts in
  let f, ts = Option.value (Hashtbl.find_opt minst name) ~default:(f, ts) in
  match Hashtbl.find gfns f with
  | Fn g ->
      let s = List.combine (List.map fst g.tparams) ts in
      Fn
        {
          g with
          name;
          tparams = [];
          bounds = [];
          params = List.map (fun (m, x, t) -> (m, x, subst s t)) g.params;
          ret = subst s g.ret;
          body = map_body ~fresh:true (fun _ -> subst s) g.body;
        }
  | it -> it

(* 内建类型的 to_str 是内建函数（不生成实例）。 *)
let builtin_alias f t =
  match f, t with
  | "ToStr.to_str", TI64 -> Some "str_from_i64"
  | "ToStr.to_str", TF64 -> Some "str_from_f64"
  | "ToStr.to_str", TChar -> Some "str_from_char"
  | "ToStr.to_str", TBool -> Some "str_from_bool"
  | "ToStr.to_str", TStr -> Some "str_clone"
  | _ -> None

(* trait 的方法 f 对具体类型 t 第一次被调用：impl 里写了的就是函数 f<t>；
   内建类型的 to_str 换成内建函数；否则生成实例（泛型 impl 的方法，或者默认方法）。 *)
let method_inst f t =
  let name = inst_name f [ t ] in
  if not (Hashtbl.mem sigs name) then
    match builtin_alias f t with
    | Some b -> Hashtbl.replace aliases name b
    | None -> (
        match find_impl (Hashtbl.find methods f) t with
        | Some (i, s) when List.mem (bare f) i.defined ->
            Hashtbl.replace minst name (inst_name f [ i.head ], List.map (fun n -> List.assoc n s) i.itps);
            Queue.add (f, [ t ]) pending
        | Some _ -> Queue.add (f, [ t ]) pending
        | None -> ())

(* 泛型调用推出的类型实参里的区域按出现的顺序换成 '0、'1…：不同调用处的同一种实例只生成一份，
   也不会和函数体里 arena 的名字冲突。 *)
let canon_regions ts =
  let rs =
    List.fold_left (fun acc r -> if List.mem r acc then acc else acc @ [ r ]) [] (List.concat_map region_names ts)
  in
  List.map (rename (List.mapi (fun i r -> (r, "'" ^ string_of_int i)) rs)) ts

(* 调用泛型函数 f 推出了类型实参 ts：都是具体类型时登记这个实例（第一次时）。
   方法调用直接用 impl 里的函数；impl 没写、用默认方法的，或者 impl 是泛型的，生成实例；
   带 fn(…) 参数的函数生成的是特化（不在这里）。 *)
let request_inst f ts ~has_fn =
  let n = inst_name f ts in
  if List.for_all (fun t -> params_of t = []) ts && (not has_fn) && not (Hashtbl.mem requested n) then begin
    Hashtbl.replace requested n ();
    trace := ("inst " ^ n) :: !trace;
    if Hashtbl.mem methods f then method_inst f (List.hd ts) else Queue.add (f, ts) pending
  end

(* 调用 f(args) 的节点。inst：泛型调用推出的类型实参，调用的是它们对应的实例；内建类型的 to_str 直接是内建函数（别名，
   alias 查它），这时实参按内建函数的签名传（I64 按值，不按 Self 的借用）。返回借用的泛型函数，实例也借自同样的参数
   （登记进 Defs，borrow.ml 按调用的函数名查）。签名查不到的只会是不降级的函数里的调用（见 not_lowered），约定随便取。 *)
let call_node ?(alias = Hashtbl.find_opt aliases) f inst (args : Tast.expr list) : Tast.expr_desc =
  let g = match inst with Some ts -> inst_name f ts | None -> f in
  let f', g = match alias g with Some b -> (b, b) | None -> (f, g) in
  let lent = Hashtbl.find_opt let_regions f' in
  (match lent with Some l when g <> f' -> Hashtbl.replace let_regions g l | _ -> ());
  let modes =
    match Hashtbl.find_opt sigs f' with Some (ps, _) -> List.map fst ps | None -> List.map (fun _ -> MLet) args
  in
  Tast.Call
    {
      callee = g;
      base = (if inst = None then "" else f);
      targs = Option.value inst ~default:[];
      modes;
      borrows = Option.value lent ~default:[];
      args;
    }

(* 改写调用时新加的实参：变量 v（类型是 t），类型直接记下。 *)
let var_expr pos v t =
  let id = fresh_id () in
  Hashtbl.replace types id (Ty t);
  { e = Var v; pos; id }

(* 每个表达式的类型按 id 记下来，给降级用。 *)
(* 算术的类型：左边是 F64 时按 F64，否则按 I64（右边、报错都跟着它）。 *)
let num = function Ty TF64 -> TF64 | _ -> TI64


(* ---------- 闭包的提升和特化（检查器和单态化共用） ----------
   检查器在会生成代码的函数里直接调用；单态化（elab/mono.ml）按实例的类型重放泛型函数里记下的 Lam、Spec、Pack、Spawn。
   reg：Real 真的登记（排进 pending_items、写 sigs），Dry 只算名字、记下会登记什么（对照用，不改任何表）。 *)
type reg = Real | Dry of (string, unit) Hashtbl.t * string list ref

(* 名字 n 第一次出现：Real 时记进 requested。 *)
let first reg n =
  match reg with
  | Real -> if Hashtbl.mem requested n then false else (Hashtbl.replace requested n (); true)
  | Dry (seen, _) -> if Hashtbl.mem requested n || Hashtbl.mem seen n then false else (Hashtbl.replace seen n (); true)

let traced reg s = match reg with Real -> trace := s :: !trace | Dry (_, l) -> l := s :: !l

(* 闭包 |ys| body 提升成普通函数（检查完函数体、找出捕获 caps0 之后）。regions：闭包所在处可见的区域。
   捕获的类型里的区域换成区域参数 'c00、'c01…：按这里的嵌套顺序编号（外层在前），
   先后关系只取这里真实成立的（`Closures.CapRef` 的 capture_mono；调用时这些区域都活着是 capture_below）。
   r 为 Some 且 concrete（会生成代码）时登记：捕获在前、闭包的参数在后；名字按位置和类型确定，同一个闭包只提升一次。
   ft：函数体里写出的类型怎么换（单态化时是类型参数的替换）。 *)
let lift ?(reg = Real) ?(ft = Fun.id) ~concrete regions pos ys (body : expr) pts r caps0 : closure =
  let outer_first = List.rev_map fst regions in
  let idx r = Option.value (List.find_index (( = ) r) outer_first) ~default:max_int in
  let rs =
    List.sort_uniq compare (List.concat_map (fun (_, _, t) -> region_names t) caps0)
    |> List.stable_sort (fun a b -> compare (idx a) (idx b))
  in
  if List.length rs > 100 then err pos "闭包捕获的区域太多（最多 100 个）";
  let s = List.mapi (fun k r -> (r, Printf.sprintf "'c%02d" k)) rs in
  let rord =
    List.map (fun (r, c) -> (c, List.filter_map (fun (r', c') -> if r' <> r && outlives_in regions r' r then Some c' else None) s)) s
  in
  let caps = List.map (fun (m, v, t) -> (m, v, rename s t)) caps0 in
  let file, line = Source.locate (if Array.length !Source.files > 0 then !Source.files.(0) else "") pos in
  let tys = List.map (fun (_, _, t) -> string_of_ty t) caps @ [ string_of_ty (TFn (pts, Option.value r ~default:TUnit)) ] in
  let cname = Printf.sprintf "闭包<%s:%d:%d, %s>" (Filename.basename file) line pos.col (String.concat ", " tys) in
  (match r with
  | Some r when concrete && first reg cname -> (
      traced reg ("lift " ^ cname);
      match reg with
      | Real ->
          let params = caps @ List.map2 (fun y (m, t) -> (m, y, t)) ys pts in
          Hashtbl.replace sigs cname (List.map (fun (m, _, t) -> (m, t)) params, r);
          let b = match body.e with Block b -> b | _ -> { stmts = []; tail = Some body } in
          let body = map_body ~fresh:true (fun _ t -> rename s (ft t)) b in
          Queue.add
            (Fn { name = cname; tparams = []; bounds = []; params; ret = r; body; prim = None; ipos = pos }, [], [], rord)
            pending_items
      | Dry _ -> ())
  | _ -> ());
  { cname; caps; rord }

(* Fn(…) 闭包的捕获不能含引用、不能是 linear（闭包可以存起来）。 *)
let pack_check pos (caps : (mode * string * ty) list) =
  List.iter
    (fun (_, v, t) ->
      if region_names t <> [] then
        err pos "Fn(…) 闭包暂不能用含引用或区域的变量 %s：闭包可以存起来，引用可能活不过它的区域" v;
      if is_linear t then err pos "Fn(…) 闭包暂不能用 linear 值 %s" v)
    caps

(* Fn#… 的签名和代码生成要的东西（会生成代码时）。 *)
let pack_register ?(reg = Real) ~concrete pk (c : closure) want =
  match reg with
  | Real ->
      if not (Hashtbl.mem sigs pk) then Hashtbl.replace sigs pk (List.map (fun (_, _, t) -> (MSink, t)) c.caps, want);
      if concrete then Hashtbl.replace packs pk (c, want)
  | Dry _ -> ()

(* spawn#… 同上。 *)
let spawn_register ?(reg = Real) ~concrete sp (c : closure) =
  match reg with
  | Real ->
      if not (Hashtbl.mem sigs sp) then
        Hashtbl.replace sigs sp ((MInout, TI64) :: List.map (fun (m, _, t) -> (m, t)) c.caps, TUnit);
      if concrete then Hashtbl.replace spawns sp c
  | Dry _ -> ()

(* 带 fn(…) 参数的函数 f 对闭包 cls 的特化 f[闭包, …]（第一次用到时生成）：去掉 fn(…) 参数，后面接上每个闭包的捕获
   （`Closures.Param`）。ts：f 是泛型函数时推出的类型实参。返回特化的名字。 *)
let spec_register ?(reg = Real) f ts (cls : closure list) =
  let base = match ts with Some ts -> inst_name f ts | None -> f in
  let name = base ^ "[" ^ String.concat ", " (List.map (fun (c : closure) -> c.cname) cls) ^ "]" in
  if first reg name then begin
    traced reg ("spec " ^ name);
    match reg with
    | Dry _ -> ()
    | Real -> (
        match (match ts with Some ts -> instance (f, ts) | None -> Hashtbl.find fn_items f) with
        | Fn g ->
            (* 泛型实例已经复制过；非泛型函数在这里复制（表达式换新的 id） *)
            let body = if ts = None then map_body ~fresh:true (fun _ t -> t) g.body else g.body in
            let fparams = List.filter (fun (_, _, t) -> is_fn t) g.params in
            let binding = List.map2 (fun (_, y, t) c -> (y, t, c)) fparams cls in
            (* 每个闭包的区域参数加上 @y 区分（两个闭包都有 'c00 时是不同的区域，彼此之间没有顺序） *)
            let ry (c : closure) y = List.map (fun (r, _) -> (r, r ^ "@" ^ y)) c.rord in
            let capsof (y, _, (c : closure)) = List.map (fun (m, v, t) -> (m, v ^ "@" ^ y, rename (ry c y) t)) c.caps in
            let caps = List.concat_map capsof binding in
            let params = List.filter (fun (_, _, t) -> not (is_fn t)) g.params @ caps in
            Hashtbl.replace sigs name (List.map (fun (m, _, t) -> (m, t)) params, g.ret);
            let clos = List.map (fun ((y, _, c) as b) -> (y, (c, List.map (fun (_, v, t) -> (v, t)) (capsof b)))) binding in
            let fvars = List.map (fun (y, t, _) -> (y, t)) binding in
            let rord =
              List.concat_map
                (fun (y, _, (c : closure)) -> List.map (fun (r, os) -> (r ^ "@" ^ y, List.map (fun o -> o ^ "@" ^ y) os)) c.rord)
                binding
            in
            Queue.add (Fn { g with name; params; body; tparams = []; bounds = [] }, clos, fvars, rord) pending_items
        | _ -> assert false)
  end;
  name
