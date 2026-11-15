(* 区域、类型的相容和合并、解析写出的类型。 *)
open Ast
open Typeck_env
open Typeck_builtins

let outlives env x y =
  !lax_regions || x = y || match List.assoc_opt y env.regions with Some a -> List.mem x a | None -> false

(* 泛型结构体、枚举 g 的第 i 个类型参数能不能协变（在 typeck_generics.ml 里定义）。 *)
let covariant_fwd : (string -> int -> bool) ref = ref (fun _ _ -> false)

(* got 能不能当作 want 用：外层区域的引用可以当作内层区域的引用（D2 协变），其余要求相同。
   同一个泛型结构体、枚举的两个实例：能协变的类型参数逐个按 sub 比较，其他的要求相同（Lean `Regions.DataVariance`）。 *)
let rec sub env got want =
  match got, want with
  | TRef (t, x), TRef (t', y) -> t = t' && outlives env x y
  | TResult (a, b), TResult (c, d) -> sub env a c && sub env b d
  | TStruct a, TStruct b when a <> b -> (
      match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
      | Some (g, ts), Some (h, us) when g = h && List.length ts = List.length us ->
          List.for_all Fun.id
            (List.mapi (fun i (t, u) -> t = u || (!covariant_fwd g i && sub env t u)) (List.combine ts us))
      | _ -> false)
  | _ -> got = want

(* 泛型结构体、枚举的实例的类型实参（不是实例时为空），见 defs.ml。 *)
let inst_args = Defs.inst_args defs

(* 换名之后重新实例化（instantiate 在 typeck_generics.ml 里定义）。 *)
let instantiate_fwd : (string -> ty list -> ty) ref = ref (fun _ _ -> assert false)

let rec mentions r = function
  | TRef (t, x) -> x = r || mentions r t
  | TArena x -> x = r
  | TBox t | TVec t -> mentions r t
  | TResult (a, b) -> mentions r a || mentions r b
  | TStruct n -> List.exists (mentions r) (inst_args n)
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TApp _ | TParam _ | TFn _ | TOFn _ -> false   (* fn(…)、Fn(…) 里不能有区域（check_ty） *)

let region_names = Defs.region_names defs

let rec rename s = function
  | TRef (t, x) -> TRef (rename s t, Option.value (List.assoc_opt x s) ~default:x)
  | TArena x -> TArena (Option.value (List.assoc_opt x s) ~default:x)
  | TBox t -> TBox (rename s t)
  | TVec t -> TVec (rename s t)
  | TResult (a, b) -> TResult (rename s a, rename s b)
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) when List.exists (fun t -> region_names t <> []) ts -> !instantiate_fwd g (List.map (rename s) ts)
      | _ -> t)
  | t -> t

(* 两个分支的类型合并：Never 和任何类型相容；区域不同时取活得短的那个。 *)
let join env pos what a b =
  match a, b with
  | Never, x | x, Never -> x
  | Ty x, Ty y when sub env x y -> Ty y
  | Ty x, Ty y when sub env y x -> Ty x
  | _ -> err pos "%s的两个分支类型不同：%s 和 %s" what (show a) (show b)

let expect env pos want got =
  match got with
  | Never -> ()
  | Ty t when sub env t want -> ()
  | Ty t -> err pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty want) (string_of_ty t)

(* param：函数参数的类型，可以是 Arena。 *)
let rec check_ty ?(param = false) env pos = function
  | TStruct n when not (Hashtbl.mem env.structs n || Hashtbl.mem enum_tbl n) -> err pos "未知类型 %s" n
  | TStruct n -> List.iter (check_ty env pos) (inst_args n)
  | TBox t ->
      if is_linear t then err pos "Box 里不能放 linear 值（还不能从 Box 里移出）";
      check_ty env pos t
  | TVec t -> check_ty env pos t
  | TResult (a, b) -> check_ty env pos a; check_ty env pos b
  | TArena _ when not param -> err pos "Arena 只能作为函数参数的类型"
  | TFn _ when not param ->
      err pos "函数类型 fn(…) 只能作为函数参数的类型（闭包不能存进变量、字段、Vec，也不能返回）"
  | TFn (ps, r) ->
      if List.exists (fun (_, t) -> region_names t <> []) ps || region_names r <> [] then
        err pos "函数类型 fn(…) 里暂不能有引用或区域";
      List.iter (fun (_, t) -> check_ty env pos t) ps;
      check_ty env pos r
  | TOFn (ps, r) ->
      (* Fn(…)（Lean `Closures.Stored`）：可以存、可以返回，所以里面不能有引用或区域（环境里也不能有，见 pack） *)
      if List.exists (fun (_, t) -> region_names t <> []) ps || region_names r <> [] then
        err pos "函数类型 Fn(…) 里暂不能有引用或区域";
      List.iter (fun (_, t) -> check_ty env pos t) ps;
      check_ty env pos r
  | TRef (t, r) ->
      (* 区域可以是变量的名字：借自这个变量的引用（返回借用的调用的结果，Lean `Projection.Return`） *)
      if not (List.mem_assoc r env.regions || List.mem_assoc r env.vars) then err pos "未知区域 %s" r;
      if region_names t <> [] then err pos "还不支持指向引用或区域的引用";
      if t = TUnit then err pos "不能引用 ()";
      check_ty env pos t
  | _ -> ()

