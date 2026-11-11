(* 泛型、trait 和 impl 的查找、协变、类型实参的推断和实例化。 *)
open Ast
open Typeck_env
open Typeck_builtins
open Typeck_regions

(* ---------- 泛型（Lean `Generics.Check`） ----------
   泛型结构体和枚举按用到的类型实参实例化成具体的类型，名字就是写法本身（例如 "Option<I64>"），
   登记到 struct_tbl / enum_tbl，之后的检查、降级、代码生成都把它当普通类型。
   泛型函数里的类型参数记作 TParam，按抽象类型检查：值只能移动，普通的 T 可以自动析构，
   T: linear 必须交出去；不能对 T 做运算、取字段或 match。调用处推出类型实参并检查约束，
   用到的每个实例复制一份函数体，换成具体类型后照常检查（单态化）。 *)

(* 泛型结构体、枚举的定义：类型参数名和原始定义（类型还没解析）。 *)
let gtypes : (string, string list * item) Hashtbl.t = Hashtbl.create 16

(* 泛型函数的类型参数；解析过类型的泛型函数本身，实例化时复制它。 *)
let gparams : (string, (string * bool) list) Hashtbl.t = Hashtbl.create 16
let gfns : (string, item) Hashtbl.t = Hashtbl.create 16

(* trait（Lean `Traits.Static`）：每个方法当作带一个类型参数 Self 的泛型函数，Self 换成实现的类型。
   方法的函数名是 trait.方法（例如 Ord.cmp），不占用全局的名字：只能用方法调用 x.cmp(y) 调用，
   按接收者的类型在约束和 impl 里找（Lean `Traits.Inherent` 的 resolve_tra）。
   traits：trait → 方法名；methods：方法的函数名 → trait；tmethods：方法名 → (trait, 函数名)；impls：trait → 它的每个 impl；
   gbounds：泛型函数（和方法）的类型参数要满足的 trait 约束。
   impl 里的方法是普通的具体函数，名字记作 trait.方法<类型>（和泛型函数的实例同一种写法），单态化时直接调用它。
   泛型 impl（Lean `Traits.ImplSearch`）：impl 可以是泛型的（头部 Vec<T>，约束 T: ToStr），它的方法是以 impl 的类型参数为类型参数的泛型函数，
   名字是 trait.方法<头部>；trait 的方法可以有默认的函数体（defaults），它是泛型函数 trait.方法（类型参数 Self）。 *)
type impl_info = {
  head : ty;   (* 实现的类型，impl 的类型参数是 TParam *)
  itps : string list;   (* impl 的类型参数 *)
  ibounds : (string * string) list;
  defined : string list;   (* impl 里写了的方法（方法名） *)
}

let traits : (string, string list) Hashtbl.t = Hashtbl.create 16
let methods : (string, string) Hashtbl.t = Hashtbl.create 16
let tmethods : (string, string * string) Hashtbl.t = Hashtbl.create 16
let impls : (string, impl_info) Hashtbl.t = Hashtbl.create 16
let gbounds : (string, (string * string) list) Hashtbl.t = Hashtbl.create 16
let defaults : (string, unit) Hashtbl.t = Hashtbl.create 16

(* trait tr 的方法 m 的函数名；报错时只显示方法名。 *)
let trait_key tr m = tr ^ "." ^ m

let bare f =
  match Hashtbl.find_opt methods f with
  | Some tr -> String.sub f (String.length tr + 1) (String.length f - String.length tr - 1)
  | None -> f

(* 泛型 impl 的方法的实例：实例名 方法<具体类型> → (泛型函数 方法<头部>, impl 的类型实参)。 *)
let minst : (string, string * ty list) Hashtbl.t = Hashtbl.create 16

(* 内建类型的 ToStr：实例名 to_str<I64> 等直接调用内建函数，降级时换名。 *)
let aliases : (string, string) Hashtbl.t = Hashtbl.create 16

(* 每个泛型调用（按调用表达式的 id）推出的类型实参，给降级时找实例的名字用。 *)
let call_inst : (int, ty list) Hashtbl.t = Hashtbl.create 16

(* 还没有生成的函数实例。 *)
let pending : (string * ty list) Queue.t = Queue.create ()
let requested : (string, unit) Hashtbl.t = Hashtbl.create 16

let inst_name g ts = Ast.app_name g (List.map string_of_ty ts)

(* 闭包：当前检查的函数会不会生成代码（不是泛型函数、没有 fn(…) 参数；泛型实例、特化、提升出的闭包都是）。
   只有这时才提升闭包、登记特化、改写调用；其他函数里的闭包只检查。 *)
let concrete = ref false
let cur_fn = ref ""

(* 函数体里有闭包、自己又不生成代码的泛型函数：不在定义处降级（它的实例照常降级、检查）。 *)
let skip_lower : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 还没检查的提升出的闭包和特化：条目、特化里 fn(…) 参数绑定的闭包、这些 fn(…) 参数的名字和类型（仍可在函数体里用）、
   区域参数的先后关系（`Closures.CapRef`）。 *)
let pending_items :
    (item * (string * (closure * (string * ty) list)) list * (string * ty) list * (string * string list) list) Queue.t =
  Queue.create ()

(* 调用的改写：调用表达式的 id → 新的函数名和实参，检查完之后换上（和方法调用的名字一样）。 *)
let rewrites : (int, string * expr list) Hashtbl.t = Hashtbl.create 16

(* ---------- 可以存起来的闭包 Fn(…)（Lean `Closures.Stored`） ----------
   写在需要 Fn(pts) -> r 的地方（let 标注、返回值、实参、字段、Vec 元素）的闭包：和当实参传的闭包一样提升成普通函数
   （捕获在前，只读的按 let、修改的按 inout），闭包表达式换成调用内建的 Fn#提升出的函数名(捕获…)，
   捕获按 sink 传入（按值：会移动的值被移走，可复制的值复制），存进堆上的环境。调用 f(x) 换成 call#类型(f, x)，
   闭包按 inout：闭包体访问的环境和实参不重叠（`Closures.Stored` 的 env_disjoint），所以提升出的函数照常检查就够了。
   Fn 值需要析构（析构环境），只能移动；环境里不能有引用、区域和 fn(…) 参数（`Closures.Stored` 的 ok：存起来也不会悬空）。
   packs：Fn#… → (闭包, 它的类型)；fcalls：call#… → 闭包的类型。代码生成为它们写 C 函数。 *)
let packs : (string, closure * ty) Hashtbl.t = Hashtbl.create 16
let fcalls : (string, ty) Hashtbl.t = Hashtbl.create 16


(* 带嵌套模式的 match 展开后的表达式（按 PMatch 的 id）：map_body 把 PMatch 换成它，之后的阶段只看到 Match、If、Destruct。 *)
let pdesugar : (int, expr) Hashtbl.t = Hashtbl.create 16

(* 当前函数按 let 传入的参数（借来的）：在它们上面的嵌套模式按借用拆开，和 match let 一样 *)
let let_params : string list ref = ref []

(* 解析过类型的非泛型函数，特化时复制。 *)
let fn_items : (string, item) Hashtbl.t = Hashtbl.create 16

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
      err { line = 1; col = 1 } "泛型类型的实例超过 1000 个（可能是类型实参越来越大的递归类型，例如 W<T> 里有 Box<W<Box<T>>>）";
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

(* 类型自己的方法（Lean `Traits.Inherent`）：方法名 → (解析过的头部, impl 的类型参数, 函数名)。
   prepare 从 Ast.inherent 登记，同名方法的头部两两不相容（CoherentM）。 *)
let inherent_heads : (string, ty * string list * string) Hashtbl.t = Hashtbl.create 16

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
let cov_memo : (string * int, bool) Hashtbl.t = Hashtbl.create 16

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
  | TBox p, TBox t | TVec p, TVec t -> unify s p t
  | TResult (a, b), TResult (c, d) -> unify s a c; unify s b d
  | TFn (ps, r), (TFn (qs, u) | TOFn (qs, u)) | TOFn (ps, r), TOFn (qs, u) when List.length ps = List.length qs ->
      List.iter2 (fun (_, p) (_, q) -> unify s p q) ps qs;
      unify s r u
  | TStruct a, TStruct b -> (
      match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
      | Some (g, ps), Some (h, ts) when g = h -> List.iter2 (unify s) ps ts
      | _ -> ())
  | _ -> ()

(* 调用 Fn(…) 类型 t 的闭包的内建函数 call#t：闭包按 inout，其余参数按类型里写的约定。 *)
let fcall_name (fns : (string, (mode * ty) list * ty) Hashtbl.t) t =
  match t with
  | TOFn (pts, r) ->
      let n = "call#" ^ string_of_ty t in
      if not (Hashtbl.mem fns n) then begin
        let s = ((MInout, t) :: pts, r) in
        Hashtbl.replace sigs n s;
        Hashtbl.replace fns n s
      end;
      if params_of t = [] then Hashtbl.replace fcalls n t;
      n
  | _ -> assert false

(* 改写函数体里写出的类型（let 标注、var 声明），方法调用换成解析出的函数名（检查过的才有）。
   fresh：表达式换新的 id（复制出实例时用）；不是 fresh 时还换上记下的调用改写。 *)
let map_body ~fresh (ft : pos -> ty -> ty) (b : block) : block =
  let rec ex (x : expr) =
    let e =
      match x.e with
      | (Int _ | Float _ | Char _ | Bool _ | Unit | Str _ | Var _) as d -> d
      | Binop (o, a, b) -> Binop (o, ex a, ex b)
      | Unop (o, a) -> Unop (o, ex a)
      | Call (f, args) -> (
          match if fresh then None else Hashtbl.find_opt rewrites x.id with
          | Some (g, args) -> Call (g, List.map ex args)
          | None -> Call (callee x f, List.map ex args))
      | StructLit (n, fs) -> StructLit (n, List.map (fun (f, e) -> (f, ex e)) fs)
      | Field (a, f) -> Field (ex a, f)
      | Deref a -> Deref (ex a)
      | Try a -> Try (ex a)
      | Variant (v, a) -> Variant (v, Option.map ex a)
      | Match (bw, s, arms) -> Match (bw, ex s, List.map (fun (v, y, e) -> (v, y, ex e)) arms)
      | PMatch (bw, s, arms) -> (
          match if fresh then None else Hashtbl.find_opt pdesugar x.id with
          | Some d -> (ex d).e
          | None -> PMatch (bw, ex s, List.map (fun (q, e) -> (q, ex e)) arms))
      | Arena (r, b) -> Arena (r, blk b)
      | If (c, a, b) -> If (ex c, blk a, Option.map blk b)
      | Block b -> Block (blk b)
      | Lambda (ps, a) -> (
          (* 需要 Fn(…) 的地方的闭包：换成 Fn#…(捕获…) *)
          match if fresh then None else Hashtbl.find_opt rewrites x.id with
          | Some (g, args) -> Call (g, List.map ex args)
          | None -> Lambda (ps, ex a))
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
   元素类型是具体类型时登记，代码生成为它写辅助函数。 *)
let vec_inst (x : expr) b t =
  Hashtbl.replace call_inst x.id [ t ];
  if params_of t = [] then Hashtbl.replace vec_calls (inst_name ("vec_" ^ b) [ t ]) (b, t)

(* 泛型函数 f 按类型实参 ts 的实例：复制函数体（表达式换新的 id），类型参数换成具体类型。
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

(* trait 的方法 f 对具体类型 t 第一次被调用：impl 里写了的就是函数 f<t>；
   内建类型的 to_str 换成内建函数；否则生成实例（泛型 impl 的方法，或者默认方法）。 *)
let method_inst f t =
  let name = inst_name f [ t ] in
  if not (Hashtbl.mem sigs name) then
    match f, t with
    | "ToStr.to_str", TI64 -> Hashtbl.replace aliases name "str_from_i64"
    | "ToStr.to_str", TF64 -> Hashtbl.replace aliases name "str_from_f64"
    | "ToStr.to_str", TChar -> Hashtbl.replace aliases name "str_from_char"
    | "ToStr.to_str", TBool -> Hashtbl.replace aliases name "str_from_bool"
    | "ToStr.to_str", TStr -> Hashtbl.replace aliases name "str_clone"
    | _ -> (
        match find_impl (Hashtbl.find methods f) t with
        | Some (i, s) when List.mem (bare f) i.defined ->
            Hashtbl.replace minst name (inst_name f [ i.head ], List.map (fun n -> List.assoc n s) i.itps);
            Queue.add (f, [ t ]) pending
        | Some _ -> Queue.add (f, [ t ]) pending
        | None -> ())

(* 改写调用时新加的实参：变量 v（类型是 t），类型直接记下。 *)
let var_expr pos v t =
  let id = fresh_id () in
  Hashtbl.replace types id (Ty t);
  { e = Var v; pos; id }

(* 每个表达式的类型按 id 记下来，给降级用。 *)
(* 算术的类型：左边是 F64 时按 F64，否则按 I64（右边、报错都跟着它）。 *)
let num = function Ty TF64 -> TF64 | _ -> TI64

