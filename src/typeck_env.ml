(* 类型检查共用的状态：错误、类型 t、环境、闭包的捕获，以及类型检查结果的表（给降级和代码生成用）。 *)
open Ast

exception Error of pos * string

(* 一次报多个错误：每个函数各自的第一个错误，按出现顺序。 *)
exception Errors of (pos * string) list
let err pos fmt = Printf.ksprintf (fun m -> raise (Error (pos, m))) fmt

type t = Ty of ty | Never

type env = {
  structs : (string, field list) Hashtbl.t;
  fns : (string, (mode * ty) list * ty) Hashtbl.t;
  ret : ty;                          (* 当前函数的返回类型 *)
  vars : (string * (ty * bool)) list;
      (* 局部变量和它是否可修改（var、inout / sink 参数），内层在前 *)
  guards : string list;              (* 当前生效的 errdefer 要消费的变量 *)
  tbounds : (string * string) list;  (* 当前泛型函数的类型参数的 trait 约束，(T, Show) 表示 T: Show *)
  clos : (string * (closure * (string * ty) list)) list;
      (* 特化里 fn(…) 参数绑定的闭包，和这个特化里传它的捕获的参数（名字和类型） *)
}

(* ---------- 闭包（Lean `Closures.Param`） ----------
   闭包 |x| e 只能写在实参的位置，传给类型是 fn(…) 的参数；fn(…) 类型的值不能存、不能返回（second-class）。
   闭包用到的外层变量是它的捕获：只读的按 let，赋值、作 inout 实参或改字段的按 inout。
   实现是按闭包特化：闭包提升成一个普通函数（捕获在前，闭包的参数在后）；调用 h(a, |x| …) 换成调用
   h 的特化 h[闭包]，它没有 fn(…) 参数，多出闭包的捕获作参数，函数体里的 f(y) 换成调用提升出的函数。
   于是捕获就是一层层传下去的额外实参：调用处按 `Calls.NArgs` 的规则检查一次（`Closures.Param` 的 inner_ok 说明这就够了），
   之后的所有权、排他性检查和翻译验证看到的都是普通的函数，不用改。闭包不能逃逸，引用的栈帧总还活着（`Closures.Param` 的 clo_live）。 *)
and closure = {
  cname : string;                      (* 提升出来的函数名，按闭包的位置和类型确定 *)
  caps : (mode * string * ty) list;    (* 捕获的外层变量（按第一次用到的顺序）：只读的 let，修改的 inout *)
  rord : (string * string list) list;
      (* 捕获的类型里的区域换成的区域参数 'c00、'c01…（按闭包所在位置的嵌套顺序，外层在前），
         和比它活得久的区域参数（只取那里真实成立的先后关系，`Closures.CapRef`） *)
}

(* 正在检查的闭包：env.vars 最后 outer 个是外层的变量；用到的外层变量（按第一次出现的顺序）；改过的外层变量。 *)
type frame = { outer : int; used : string list ref; written : (string, unit) Hashtbl.t }

let lambda_frame : frame option ref = ref None

(* 在闭包里用到变量 v（write：修改它）：v 是外层的变量时记成捕获。 *)
let note ?(write = false) (vars : (string * (ty * bool)) list) v =
  match !lambda_frame with
  | None -> ()
  | Some fr -> (
      let rec find i = function [] -> None | (y, _) :: r -> if y = v then Some i else find (i + 1) r in
      match find 0 vars with
      | Some i when i >= List.length vars - fr.outer ->
          if not (List.mem v !(fr.used)) then fr.used := !(fr.used) @ [ v ];
          if write then Hashtbl.replace fr.written v ()
      | _ -> ())

let is_fn = function TFn _ -> true | _ -> false

(* 每个表达式的类型，按表达式 id 记录。 *)
let types : (int, t) Hashtbl.t = Hashtbl.create 256

(* 每条 errdefer（按调用表达式的 id）要消费的变量。 *)
let errdefer_targets : (int, string) Hashtbl.t = Hashtbl.create 16

(* 函数签名，给降级用：每个参数的约定和类型、返回类型。 *)
let sigs : (string, (mode * ty) list * ty) Hashtbl.t = Hashtbl.create 16

(* 返回借用的函数（Lean `Projection.Return`）：返回类型 &T in m 里的 m 是 let 参数的名字。
   函数名 → (参数名, 第几个参数)。调用处的结果借自对应的实参，锁住它（borrow.ml）。
   内建的 str_view(s) 是 [("s", 0)]。 *)
let let_regions : (string, (string * int) list) Hashtbl.t = Hashtbl.create 16

(* 结构体定义和标了 linear 的结构体，给降级和代码生成用。 *)
let struct_tbl : (string, field list) Hashtbl.t = Hashtbl.create 16
let linear_tbl : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 枚举定义：变体按声明顺序，下标就是运行时的标签（Lean `Datatypes.Enum`）。 *)
let enum_tbl : (string, (string * ty option) list) Hashtbl.t = Hashtbl.create 16

(* 一个类型的变体（不是枚举时为 None）。Result<T, E> 就是变体 Ok(T)、Err(E) 的枚举。
   () 载荷是真的载荷（泛型代码里 T 可以是 ()），只是写的时候可以省略：Ok 就是 Ok(())，分支可以写 Ok => …。 *)
let variants = function
  | TStruct n -> Hashtbl.find_opt enum_tbl n
  | TResult (t, e) -> Some [ ("Ok", Some t); ("Err", Some e) ]
  | _ -> None

(* 变体的下标和载荷类型。 *)
let variant ty v =
  let rec go i = function
    | [] -> None
    | (w, t) :: rest -> if w = v then Some (i, t) else go (i + 1) rest
  in
  go 0 (Option.value (variants ty) ~default:[])

(* 报错时变体的写法：Ok / Err 不带点。 *)
let vname v = if v = "Ok" || v = "Err" then v else "." ^ v

(* match 的分支：第 v 个变体用写了它的分支，没写的用 _ 分支（Lean `Matching.Wildcard` 的 expand）。
   返回 (载荷绑定的变量, 分支体)；_ 分支在带载荷（不是 ()）的变体上把载荷绑到 _#（用户写不出这个名字），
   分支结束时随作用域析构。 *)
let wild_var = "_#"

let arm_for arms (v, pty) =
  match List.find_opt (fun (w, _, _) -> w = v) arms with
  | Some (_, x, e) -> Some (x, e)
  | None -> (
      match List.find_opt (fun (w, _, _) -> w = "_") arms with
      | Some (_, _, e) -> Some ((match pty with Some t when t <> TUnit -> wild_var | _ -> ""), e)
      | None -> None)


(* linear 值必须被显式消费，不能在作用域结束时自动析构。 *)
let rec is_linear = function
  | TStruct n -> Hashtbl.mem linear_tbl n
  | TResult (a, b) -> is_linear a || is_linear b
  | TBox t | TVec t -> is_linear t
  | TParam (_, l) -> l
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TRef _ | TArena _ | TApp _ | TFn _ | TOFn _ -> false

(* 需要析构（或者是 linear）的类型：这些类型的值只能移动，不能复制。 *)
let rec needs_drop = function
  | TBox _ | TVec _ | TStr | TBytes | TOFn _ -> true   (* Fn(…)拥有堆上的环境 *)
  | TStruct n -> (
      Hashtbl.mem linear_tbl n
      ||
      match Hashtbl.find_opt struct_tbl n with
      | Some fs -> List.exists (fun (f : field) -> needs_drop f.fty) fs
      | None -> (
          (* 枚举：某个变体的载荷需要析构 *)
          match Hashtbl.find_opt enum_tbl n with
          | Some vs -> List.exists (function _, Some t -> needs_drop t | _, None -> false) vs
          | None -> false))
  | TResult (a, b) -> needs_drop a || needs_drop b
  | TParam _ -> true            (* 类型参数按抽象类型检查：可能需要析构，只能移动（Lean `Generics.Check`） *)
  | TI64 | TF64 | TChar | TBool | TView | TUnit | TRef _ | TArena _ | TApp _ | TFn _ -> false

