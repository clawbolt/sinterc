(* 程序的定义（Defs）：结构体、枚举、函数签名、trait 和 impl。
   collect.ml 登记，之后类型检查、降级、所有权检查、代码生成都从这里查定义，不再通过 Typeck.xxx 读类型检查的内部。

   R4a.2：字段就是 typing.ml 第一组里的那些表（同一个 Hashtbl），Typing.defs 是指向它们的这个记录，
   所以类型检查写表的方式不用改。之后 collect 改成直接建一个新的 Defs.t、类型检查显式接收它，Typing 的第一组就可以去掉。

   建好之后会变的只有两处，都是检查时才知道的泛型实例：
   - structs、enums 里会加进泛型结构体、枚举的实例（Option<I64>），instances 记下它们来自哪个泛型；
   - borrowed_returns 会加进返回借用的泛型函数的实例（降级时登记，R6 换成 Tast 的调用节点后去掉）。 *)
open Ast

(* trait 的一个 impl（Lean `Traits.ImplSearch`）。impl 可以是泛型的（头部 Vec<T>，约束 T: ToStr），
   它的方法是以 impl 的类型参数为类型参数的泛型函数，名字是 trait.方法<头部>。 *)
type impl_info = {
  head : ty;   (* 实现的类型，impl 的类型参数是 TParam *)
  itps : string list;   (* impl 的类型参数 *)
  ibounds : (string * string) list;
  defined : string list;   (* impl 里写了的方法（方法名） *)
}

(* 泛型结构体、枚举的实例：检查时用到哪个登记哪个。 *)
type instances = {
  of_generic : (string, string * ty list) Hashtbl.t;   (* 实例名（Pair<I64, Str>）→ 泛型名和类型实参 *)
  covariant : (string * int, bool) Hashtbl.t;          (* 泛型类型 g 的第 i 个类型参数能不能协变（算过的） *)
}

type t = {
  (* 类型 *)
  structs : (string, field list) Hashtbl.t;   (* 结构体 → 字段，包括泛型结构体的实例 *)
  linear : (string, unit) Hashtbl.t;          (* 标了 linear 的结构体 *)
  enums : (string, (string * ty option) list) Hashtbl.t;   (* 枚举 → 变体，下标就是运行时的标签；包括实例 *)
  generic_types : (string, string list * item) Hashtbl.t;  (* 泛型结构体、枚举：类型参数名和原始定义 *)
  instances : instances;

  (* 函数 *)
  signatures : (string, (mode * ty) list * ty) Hashtbl.t;   (* 所有函数（包括内建函数）：每个参数的约定和类型、返回类型 *)
  generic_params : (string, (string * bool) list) Hashtbl.t;   (* 泛型函数和 trait 方法的类型参数；true：标了 linear *)
  bounds : (string, (string * string) list) Hashtbl.t;     (* 类型参数的 trait 约束，(T, Show) 表示 T: Show *)
  borrowed_returns : (string, (string * int) list) Hashtbl.t;
      (* 返回借用的函数（Lean `Projection.Return`）→ 结果借自的 let 参数（参数名, 第几个）；str_view 是 [("s", 0)] *)

  (* trait（Lean `Traits.Static`、`Traits.ImplSearch`） *)
  traits : (string, string list) Hashtbl.t;        (* trait → 方法名 *)
  trait_of_method : (string, string) Hashtbl.t;    (* 方法的函数名（Ord.cmp）→ trait *)
  methods_named : (string, string * string) Hashtbl.t;   (* 方法名（cmp）→ (trait, 函数名)，同名的可以有几条 *)
  impls : (string, impl_info) Hashtbl.t;           (* trait → 它的每个 impl *)
  default_methods : (string, unit) Hashtbl.t;      (* 有默认函数体的方法 *)

  (* 类型自己的方法（Lean `Traits.Inherent`）：方法名 → (解析过的头部, impl 的类型参数, 函数名)；同名方法的头部两两不相容 *)
  inherent : (string, ty * string list * string) Hashtbl.t;
}

(* ---------- 按定义回答的类型问题 ----------
   类型检查、降级、所有权检查、代码生成共用这一份（typeck_env、typeck_regions 里是对 Typing.defs 的部分应用）。 *)

(* 一个类型的变体（不是枚举时为 None）。Result<T, E> 就是变体 Ok(T)、Err(E) 的枚举。
   () 载荷是真的载荷（泛型代码里 T 可以是 ()），只是写的时候可以省略：Ok 就是 Ok(())，分支可以写 Ok => …。 *)
let variants (d : t) = function
  | TStruct n -> Hashtbl.find_opt d.enums n
  | TResult (t, e) -> Some [ ("Ok", Some t); ("Err", Some e) ]
  | _ -> None

(* 变体的下标和载荷类型。 *)
let variant (d : t) ty v =
  let rec go i = function
    | [] -> None
    | (w, t) :: rest -> if w = v then Some (i, t) else go (i + 1) rest
  in
  go 0 (Option.value (variants d ty) ~default:[])

(* linear 值必须被显式消费，不能在作用域结束时自动析构。 *)
let rec is_linear (d : t) = function
  | TStruct n -> Hashtbl.mem d.linear n
  | TResult (a, b) -> is_linear d a || is_linear d b
  | TBox t | TVec t -> is_linear d t
  | TParam (_, l) -> l
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TRef _ | TArena _ | TApp _ | TFn _ | TOFn _ -> false

(* 需要析构（或者是 linear）的类型：这些类型的值只能移动，不能复制。 *)
let rec needs_drop (d : t) = function
  | TBox _ | TVec _ | TStr | TBytes | TOFn _ -> true   (* Fn(…)拥有堆上的环境 *)
  | TStruct n -> (
      Hashtbl.mem d.linear n
      ||
      match Hashtbl.find_opt d.structs n with
      | Some fs -> List.exists (fun (f : field) -> needs_drop d f.fty) fs
      | None -> (
          (* 枚举：某个变体的载荷需要析构 *)
          match Hashtbl.find_opt d.enums n with
          | Some vs -> List.exists (function _, Some t -> needs_drop d t | _, None -> false) vs
          | None -> false))
  | TResult (a, b) -> needs_drop d a || needs_drop d b
  | TParam _ -> true            (* 类型参数按抽象类型检查：可能需要析构，只能移动（Lean `Generics.Check`） *)
  | TI64 | TF64 | TChar | TBool | TView | TUnit | TRef _ | TArena _ | TApp _ | TFn _ -> false

(* 泛型结构体、枚举的实例的类型实参（不是实例时为空）。区域名的收集、换名都要看进类型实参（Lean `Regions.Data`）。 *)
let inst_args (d : t) n = match Hashtbl.find_opt d.instances.of_generic n with Some (_, ts) -> ts | None -> []

(* 类型里出现的区域名（有重复）。 *)
let rec region_names (d : t) = function
  | TRef (t, x) -> x :: region_names d t
  | TArena x -> [ x ]
  | TBox t | TVec t -> region_names d t
  | TResult (a, b) -> region_names d a @ region_names d b
  | TStruct n -> List.concat_map (region_names d) (inst_args d n)
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TApp _ | TParam _ | TFn _ | TOFn _ -> []

(* 区域名都换成 _：C 后端里只差区域的类型是同一个 C 类型。 *)
let rec erase (d : t) = function
  | TRef (t, _) -> TRef (erase d t, "_")
  | TBox t -> TBox (erase d t)
  | TVec t -> TVec (erase d t)
  | TResult (a, b) -> TResult (erase d a, erase d b)
  | TStruct n as t -> (
      match Hashtbl.find_opt d.instances.of_generic n with
      | Some (g, ts) -> TStruct (app_name g (List.map (fun t -> string_of_ty (erase d t)) ts))
      | None -> t)
  | t -> t
