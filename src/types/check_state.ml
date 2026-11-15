(* 类型检查的状态：所有在函数之间、阶段之间传递的表都在这里，按用途分成五组。
   以前它们散在 ctx、builtin、region、generics、pattern、check_expr 六个文件里（当时叫 typeck_*），
   prepare 开头要一张张手工清空。现在 reset 一次清空前三组；第四组已经搬进环境（R4.0b）；第五组不清空。

   ctx.ml include 这个模块，其他文件按名字用。类型检查之外的阶段不再读这里：降级读 Tast（R4b），
   定义读 Defs（R4a，字段就是第一组的表），代码生成另外拿到 Check.generated。
   第二组剩下的按 id 记的表在 R5 去掉。 *)
open Ast

(* ---------- 类型 ---------- *)

(* 表达式的类型；Never 是 return、break、panic 之后的代码。和 Tast 的是同一个类型。 *)
type t = Tast.typ = Ty of ty | Never

(* 闭包（Lean `Closures.Param`）：提升出的函数名、捕获、区域参数的先后。定义和说明在 tast.ml。 *)
type closure = Tast.closure = {
  cname : string;
  caps : (mode * string * ty) list;
  rord : (string * string list) list;
}

(* 正在检查的闭包：env.vars 最后 outer 个是外层的变量；用到的外层变量（按第一次出现的顺序）；改过的外层变量。 *)
type frame = { outer : int; used : string list ref; written : (string, unit) Hashtbl.t }

(* ---------- 结构化并发（Lean `Concurrency.Scope`） ----------
   scope |s| { … } 里 s.spawn(|| …) 启动任务，块结束时等所有任务结束。任务的捕获只有两种：
   scope 外面的变量只读共享（let，整个 scope 里谁都不能修改、移动它），scope 里面的变量移进任务（sink，可复制的复制一份）。
   正在检查的一个 scope：s 的名字、s 在 env.vars 里的深度（从外往里数）、外面的变量个数、只读共享的变量、是否写在闭包里。 *)
type scope_frame = { sname : string; sdepth : int; souter : int; shared : string list ref; slam : bool }

(* trait 的一个 impl，定义在 defs.ml。 *)
type impl_info = Defs.impl_info = {
  head : ty;
  itps : string list;
  ibounds : (string * string) list;
  defined : string list;
}

(* ---------- 第一组：程序的定义 ----------
   prepare 登记，检查函数体时只查不改；例外是泛型类型的实例（resolve 时用到哪个登记哪个）和提升出的闭包的签名。 *)

(* resolve.ml 登记的表：类型自己的方法、内建方法、类型函数、类型定义在哪个文件。 *)
let resolved = ref (Resolved.empty ())

(* 结构体定义和标了 linear 的结构体，给降级和代码生成用。 *)
let struct_tbl : (string, field list) Hashtbl.t = Hashtbl.create 16
let linear_tbl : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 枚举定义：变体按声明顺序，下标就是运行时的标签（Lean `Datatypes.Enum`）。 *)
let enum_tbl : (string, (string * ty option) list) Hashtbl.t = Hashtbl.create 16

(* 泛型结构体、枚举的定义：类型参数名和原始定义（类型还没解析）。 *)
let gtypes : (string, string list * item) Hashtbl.t = Hashtbl.create 16

(* 泛型结构体、枚举的实例：实例名 → 泛型名和类型实参。 *)
let inst_tbl : (string, string * ty list) Hashtbl.t = Hashtbl.create 16

(* 泛型结构体、枚举 g 的第 i 个类型参数能不能协变（generics.ml 的 covariant 算出后记下）。 *)
let cov_memo : (string * int, bool) Hashtbl.t = Hashtbl.create 16

(* 泛型函数的类型参数；解析过类型的泛型函数本身，实例化时复制它。 *)
let gparams : (string, (string * bool) list) Hashtbl.t = Hashtbl.create 16
let gfns : (string, item) Hashtbl.t = Hashtbl.create 16

(* trait（Lean `Traits.Static`）：traits：trait → 方法名；methods：方法的函数名 → trait；
   tmethods：方法名 → (trait, 函数名)；impls：trait → 它的每个 impl；
   gbounds：泛型函数（和方法）的类型参数要满足的 trait 约束；defaults：有默认函数体的方法（泛型函数 trait.方法）。 *)
let traits : (string, string list) Hashtbl.t = Hashtbl.create 16
let methods : (string, string) Hashtbl.t = Hashtbl.create 16
let tmethods : (string, string * string) Hashtbl.t = Hashtbl.create 16
let impls : (string, impl_info) Hashtbl.t = Hashtbl.create 16
let gbounds : (string, (string * string) list) Hashtbl.t = Hashtbl.create 16
let defaults : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 泛型 impl 的方法的实例：实例名 方法<具体类型> → (泛型函数 方法<头部>, impl 的类型实参)。 *)
let minst : (string, string * ty list) Hashtbl.t = Hashtbl.create 16

(* 内建类型的 ToStr：实例名 to_str<I64> 等直接调用内建函数，降级时换名。 *)
let aliases : (string, string) Hashtbl.t = Hashtbl.create 16

(* 类型自己的方法（Lean `Traits.Inherent`）：方法名 → (解析过的头部, impl 的类型参数, 函数名)。
   prepare 从 Resolved.inherent 登记，同名方法的头部两两不相容（CoherentM）。 *)
let inherent_heads : (string, ty * string list * string) Hashtbl.t = Hashtbl.create 16

(* 返回借用的函数（Lean `Projection.Return`）：返回类型 &T in m 里的 m 是 let 参数的名字。
   函数名 → (参数名, 第几个参数)。调用处的结果借自对应的实参，锁住它（borrow.ml）。
   内建的 str_view(s) 是 [("s", 0)]。 *)
let let_regions : (string, (string * int) list) Hashtbl.t = Hashtbl.create 16

(* 函数签名，给降级用：每个参数的约定和类型、返回类型。prepare 最后从检查用的签名表重建。 *)
let sigs : (string, (mode * ty) list * ty) Hashtbl.t = Hashtbl.create 16

(* 解析过类型的非泛型函数，特化时复制。 *)
let fn_items : (string, item) Hashtbl.t = Hashtbl.create 16

(* 上面的定义合成一个 Defs.t（同一批表，不复制），给降级、所有权检查、代码生成用。
   reset 只清空这些表，不换掉它们，所以 defs 一直有效。 *)
let defs : Defs.t =
  {
    Defs.structs = struct_tbl;
    linear = linear_tbl;
    enums = enum_tbl;
    generic_types = gtypes;
    instances = { Defs.of_generic = inst_tbl; covariant = cov_memo };
    signatures = sigs;
    generic_params = gparams;
    bounds = gbounds;
    borrowed_returns = let_regions;
    traits;
    trait_of_method = methods;
    methods_named = tmethods;
    impls;
    default_methods = defaults;
    inherent = inherent_heads;
  }

(* ---------- 第二组：检查中间的结果和给代码生成的东西 ----------
   检查的结果是带类型的语法树（Tast），降级只读它。这里剩下两类：
   按表达式 id 记的两张表，只在类型检查里面用；代码生成要为它们写 C 辅助函数的东西（Check.generated 交出去）。
   按 id 记的表要求实例化时复制出的函数体分配新的 id。 *)

(* 每个表达式的类型：有几处要看之前检查过的子表达式的类型（借自哪里、给字段赋值的路径、inout 实参）。 *)
let types : (int, t) Hashtbl.t = Hashtbl.create 256

(* 方法调用 e.f(…)（UFCS）解析成的函数名，按表达式 id 记下：复制 Ast 时沿用（map_body 换上；特化、对照模式）。 *)
let method_res : (int, string) Hashtbl.t = Hashtbl.create 16

(* spawn#… → 提升出的任务函数和它的捕获，代码生成为它写启动线程的 C 函数。 *)
let spawns : (string, closure) Hashtbl.t = Hashtbl.create 16

(* 可以存起来的闭包（Lean `Closures.Stored`）：packs：Fn#… → (闭包, 它的类型)；fcalls：call#… → 闭包的类型。
   代码生成为它们写 C 函数。 *)
let packs : (string, closure * ty) Hashtbl.t = Hashtbl.create 16
let fcalls : (string, ty) Hashtbl.t = Hashtbl.create 16

(* Vec 内建函数的实例（降级后的调用名，例如 vec_push<I64>）→ 函数（push 等）和元素类型，给代码生成找辅助函数用。 *)
let vec_calls : (string, string * ty) Hashtbl.t = Hashtbl.create 16

(* 函数体里有闭包、自己又不生成代码的泛型函数：不在定义处降级（它的实例照常降级、检查）。 *)
let skip_lower : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 单态化（elab/mono.ml）用的：检查一个函数时，按发生的顺序记下和类型实参有关的请求。
   泛型函数检查完，连同它的语法树记进 generic_fns；实例就是把语法树里的类型参数换掉，再按实例的类型实参重放这些请求。 *)
type request =
  | Inst of string * ty list * bool  (* 调用泛型函数 f，推出的类型实参（区域已经换名）；f 带不带 fn(…) 参数 *)
  | Vec of string * ty               (* Vec 的内建函数 vec_b 用在元素类型 t 上 *)
  | Fcall of (string, (mode * ty) list * ty) Hashtbl.t * ty * string
      (* 调用 Fn(…) 类型 t 的值：登记 call#t 的表，和这里用的名字 *)
  | Lam of lam                       (* 闭包（检查完函数体、找出捕获之后）：会生成代码的函数里在这时提升 *)
  | Spec of int * string * ty list option * (mode * ty) list * int list
      (* 调用带 fn(…) 参数的函数：语法树里调用节点的编号（callee 是 spec#编号）、函数、推出的类型实参、参数、
         每个 fn(…) 参数绑定的闭包（Lam 的编号） *)
  | Pack of int * ty * string        (* 需要 Fn(…) 的地方的闭包：Lam 的编号、Fn 类型、这里的 Fn#… 名字 *)
  | Spawn of int * string            (* s.spawn(|| …)：Lam 的编号、这里的 spawn#… 名字 *)

(* 闭包提升要的：检查时可见的区域（env.regions）、闭包本身、参数的类型、返回类型、捕获（约定、变量、外层的类型）。 *)
and lam = {
  lid : int;
  lregions : (string * string list) list;
  lexpr : expr;
  lys : string list;
  lbody : expr;
  lpts : (mode * ty) list;
  lret : ty;
  lcaps : (mode * string * ty) list;
}

(* Lam、Spec 的编号（整个程序里不重复） *)
let lam_count = ref 0
let spec_count = ref 0

let requests : request list ref = ref []   (* 正在检查的函数的请求，倒序 *)

(* 检查过的泛型函数：带类型的语法树、检查时的请求（按顺序）、检查前后的 pm_count（实例里的 #pN 要重新编号）。 *)
let generic_fns : (string, Tast.fn_def * request list * int * int) Hashtbl.t = Hashtbl.create 16

(* 实际登记了的实例、Vec 内建函数、提升出的闭包、特化，按顺序（倒序存）：对照单态化和重新检查的结果用。 *)
let trace : string list ref = ref []

(* ---------- 第三组：还没检查的函数 ----------
   检查时遇到新的泛型实例、闭包、特化就排进来，program 和 check_batch 依次取出来检查。 *)

(* 还没有生成的泛型函数实例；requested 防止同一个实例排两次。 *)
let pending : (string * ty list) Queue.t = Queue.create ()
let requested : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 还没检查的提升出的闭包和特化：条目、特化里 fn(…) 参数绑定的闭包、这些 fn(…) 参数的名字和类型（仍可在函数体里用）、
   区域参数的先后关系（`Closures.CapRef`）。 *)
let pending_items :
    (item * (string * (closure * (string * ty) list)) list * (string * ty) list * (string * string list) list) Queue.t =
  Queue.create ()

(* ---------- 第四组：正在检查的函数 ----------
   不再是模块级的状态：当前函数、区域、循环层数、闭包、scope 都在检查函数时传递的环境里（ctx.ml 的 env）。 *)

(* ---------- 第五组：不随程序清空的 ---------- *)

(* --unchecked：不检查区域的先后和 R3，照样导出，看 Lean 是否独立地拒绝。 *)
let lax_regions = ref false

(* 展开嵌套模式时生成的变量名 #p1、#p2…：和表达式的 id 一样一直往上数，名字会出现在生成的 C 里。 *)
let pm_count = ref 0

(* 开始检查一个程序：清空前三组。 *)
let reset () =
  Hashtbl.reset struct_tbl;
  Hashtbl.reset linear_tbl;
  Hashtbl.reset enum_tbl;
  Hashtbl.reset gtypes;
  Hashtbl.reset inst_tbl;
  Hashtbl.reset cov_memo;
  Hashtbl.reset gparams;
  Hashtbl.reset gfns;
  Hashtbl.reset traits;
  Hashtbl.reset methods;
  Hashtbl.reset tmethods;
  Hashtbl.reset impls;
  Hashtbl.reset gbounds;
  Hashtbl.reset defaults;
  Hashtbl.reset minst;
  Hashtbl.reset aliases;
  Hashtbl.reset inherent_heads;
  Hashtbl.reset let_regions;
  Hashtbl.reset sigs;
  Hashtbl.reset fn_items;
  Hashtbl.reset types;
  Hashtbl.reset method_res;
  Hashtbl.reset spawns;
  Hashtbl.reset packs;
  Hashtbl.reset fcalls;
  Hashtbl.reset vec_calls;
  Hashtbl.reset skip_lower;
  requests := [];
  Hashtbl.reset generic_fns;
  trace := [];
  Queue.clear pending;
  Hashtbl.reset requested;
  Queue.clear pending_items
