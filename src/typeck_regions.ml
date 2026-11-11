(* 区域、类型的相容和合并、解析写出的类型。 *)
open Ast
open Typeck_env
open Typeck_builtins

(* ---------- 区域（Lean `Regions.Model`、`Unified.RegionDefs`） ----------
   当前可见的区域，内层在前；每个区域记下比它活得久的区域。
   函数的区域参数（a: Arena，以及参数类型里出现的其他区域名）之间没有顺序（D3）；
   arena 块引入的区域比所有外层区域和区域参数都活得短。 *)
let regions : (string * string list) list ref = ref []

(* --unchecked：不检查区域的先后和 R3，照样导出，看 Lean 是否独立地拒绝。 *)
let lax_regions = ref false

(* 当前所在的循环有几层：break / continue 只能用在循环里。 *)
let loop_depth = ref 0

let outlives x y =
  !lax_regions || x = y || match List.assoc_opt y !regions with Some a -> List.mem x a | None -> false

(* 实例名 → 泛型名和类型实参。 *)
let inst_tbl : (string, string * ty list) Hashtbl.t = Hashtbl.create 16

(* 泛型结构体、枚举 g 的第 i 个类型参数能不能协变（在 typeck_generics.ml 里定义）。 *)
let covariant_fwd : (string -> int -> bool) ref = ref (fun _ _ -> false)

(* got 能不能当作 want 用：外层区域的引用可以当作内层区域的引用（D2 协变），其余要求相同。
   同一个泛型结构体、枚举的两个实例：能协变的类型参数逐个按 sub 比较，其他的要求相同（Lean `Regions.DataVariance`）。 *)
let rec sub got want =
  match got, want with
  | TRef (t, x), TRef (t', y) -> t = t' && outlives x y
  | TResult (a, b), TResult (c, d) -> sub a c && sub b d
  | TStruct a, TStruct b when a <> b -> (
      match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
      | Some (g, ts), Some (h, us) when g = h && List.length ts = List.length us ->
          List.for_all Fun.id
            (List.mapi (fun i (t, u) -> t = u || (!covariant_fwd g i && sub t u)) (List.combine ts us))
      | _ -> false)
  | _ -> got = want

(* 泛型结构体、枚举的实例的类型实参（不是实例时为空）。区域名的收集、换名都要看进类型实参（Lean `Regions.Data`）。 *)
let inst_args n = match Hashtbl.find_opt inst_tbl n with Some (_, ts) -> ts | None -> []

(* 换名之后重新实例化（instantiate 在 typeck_generics.ml 里定义）。 *)
let instantiate_fwd : (string -> ty list -> ty) ref = ref (fun _ _ -> assert false)

let rec mentions r = function
  | TRef (t, x) -> x = r || mentions r t
  | TArena x -> x = r
  | TBox t | TVec t -> mentions r t
  | TResult (a, b) -> mentions r a || mentions r b
  | TStruct n -> List.exists (mentions r) (inst_args n)
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TApp _ | TParam _ | TFn _ | TOFn _ -> false   (* fn(…)、Fn(…) 里不能有区域（check_ty） *)

let rec region_names = function
  | TRef (t, x) -> x :: region_names t
  | TArena x -> [ x ]
  | TBox t | TVec t -> region_names t
  | TResult (a, b) -> region_names a @ region_names b
  | TStruct n -> List.concat_map region_names (inst_args n)
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TApp _ | TParam _ | TFn _ | TOFn _ -> []

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

(* 区域名都换成 _：C 后端里只差区域的类型是同一个 C 类型。 *)
let rec erase = function
  | TRef (t, _) -> TRef (erase t, "_")
  | TBox t -> TBox (erase t)
  | TVec t -> TVec (erase t)
  | TResult (a, b) -> TResult (erase a, erase b)
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) -> TStruct (Ast.app_name g (List.map (fun t -> string_of_ty (erase t)) ts))
      | None -> t)
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
      if not (List.mem_assoc r !regions) then err pos "未知区域 %s" r;
      if region_names t <> [] then err pos "还不支持指向引用或区域的引用";
      if t = TUnit then err pos "不能引用 ()";
      check_ty env pos t
  | _ -> ()

