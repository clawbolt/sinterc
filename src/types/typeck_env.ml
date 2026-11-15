(* 类型检查共用的：环境 env 和在它上面的小工具。状态（各种表）见 typing.ml，这里 include 它；错误见 diag.ml。 *)
open Ast
include Typing

let err pos fmt = Diag.error pos fmt

(* 正在检查的函数：check_fn 开始时建好，函数体里所有的环境共用这一份。 *)
type current_fn = {
  fn_name : string;
  concrete : bool;
      (* 会不会生成代码（不是泛型函数、没有 fn(…) 参数；泛型实例、特化、提升出的闭包都是）。
         只有这时才提升闭包、登记特化、改写调用；其他函数里的闭包只检查。 *)
  let_params : string list;
      (* 按 let 传入的参数（借来的）：在它们上面的嵌套模式按借用拆开，和 match let 一样 *)
  mutable bind_regions : (string * string) list;
      (* 不拥有值的变量（let 绑定的位置、match 按借用绑定的载荷、for 的元素）借自哪个变量：变量名 → 区域名，后定义的在前。
         只用来给引用的类型取名字（遮蔽时可能取错，那样只会多拒绝：真正锁住哪个局部由 borrow.ml 决定）。
         整个函数一直往前加，不随块结束去掉，所以是可变的 *)
}

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
  cur : current_fn;
  regions : (string * string list) list;
      (* 区域（Lean `Regions.Model`、`Unified.RegionDefs`）：当前可见的区域，内层在前；每个区域记下比它活得久的区域。
         函数的区域参数（a: Arena，以及参数类型里出现的其他区域名）之间没有顺序（D3）；
         arena 块引入的区域比所有外层区域和区域参数都活得短 *)
  loop_depth : int;                  (* 所在的循环有几层：break / continue 只能用在循环里；闭包体、scope 里从 0 数 *)
  lambda_frame : frame option;       (* 正在检查的闭包（不在闭包里时为 None） *)
  scopes : scope_frame list;         (* 正在检查的 scope，内层在前 *)
}

(* 整个程序的登记（collect.ml）用的环境：不在任何函数里。 *)
let no_fn = { fn_name = ""; concrete = false; let_params = []; bind_regions = [] }

(* 在闭包里用到变量 v（write：修改它）：v 是外层的变量时记成捕获。 *)
let note ?(write = false) env v =
  match env.lambda_frame with
  | None -> ()
  | Some fr -> (
      let vars = env.vars in
      let rec find i = function [] -> None | (y, _) :: r -> if y = v then Some i else find (i + 1) r in
      match find 0 vars with
      | Some i when i >= List.length vars - fr.outer ->
          if not (List.mem v !(fr.used)) then fr.used := !(fr.used) @ [ v ];
          if write then Hashtbl.replace fr.written v ()
      | _ -> ())

let is_fn = function TFn _ -> true | _ -> false

(* s 是不是正在检查的某个 scope 的 s（按深度认，被遮住的不算） *)
let scope_of env v =
  let rec depth = function [] -> None | (y, _) :: r -> if y = v then Some (List.length r) else depth r in
  match depth env.vars with
  | Some d -> List.find_opt (fun f -> f.sname = v && f.sdepth = d) env.scopes
  | None -> None

(* return、? 不能离开 scope（同一个函数体里的；任务的闭包体里 return 只是结束这个任务） *)
let no_exit env pos what =
  let here = env.lambda_frame <> None in
  if List.exists (fun f -> f.slam = here) env.scopes then
    err pos "scope 里不能写 %s：离开 scope 之前要等所有任务结束（可以先把结果存进 scope 外面的 var 变量）" what

(* 变体、linear、析构这些按定义回答的问题在 defs.ml，这里查 Typing.defs。 *)
let variants = Defs.variants defs
let variant = Defs.variant defs

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

let is_linear = Defs.is_linear defs
let needs_drop = Defs.needs_drop defs

