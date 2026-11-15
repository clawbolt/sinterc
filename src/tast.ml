(* 带类型的语法树（Tast）。现在由 to_tast.ml 从 Ast 和类型检查的旁路表翻译出来（R4b.1），降级（mir.ml）只读它和 Defs。
   之后类型检查（R4b–d）直接产出它；去糖（R5，elab/）和单态化（mono）在它上面做。
   和 Ast 相比，降级以前要去旁路表里查的东西，都直接写在节点上：
   - 每个表达式带类型 ty：不再查 Typeck.types；
   - 调用带最终的函数名、每个实参的约定、结果借自哪些实参：不再查 call_inst、aliases、sigs、let_regions
     （泛型实例的返回借用以前由 mir.ml 降级时补登记，现在在 to_tast.ml，R5 的 mono 实例化时登记）；
   - match 的分支补全成每个变体一个（_ 分支已经展开）：不再调用 arm_for、variant；
   - for 只留类型检查选中的那个展开：不再查 for_vec；scope 带只读共享的变量：不再查 scope_shares；
     errdefer 带要消费的变量：不再查 errdefer_targets；解构带字段的类型：不再查 struct_tbl；
   - 没有 PMatch（嵌套模式已展开）、没有 Lambda（闭包已提升、调用已改写）、没有方法调用（名字已解析）、
     没有字符串插值（已经是 to_str 的调用和拼接）。
   所以表达式不需要 id：以前 id 只是旁路表的键。 *)
open Ast

(* 表达式的类型；Never 是 return、break、panic 之后的代码，它的值不会被用到。 *)
type typ = Ty of ty | Never

type lit = Int of int64 | Float of float | Char of int | Bool of bool | Unit | Str of string

type expr = { e : expr_desc; ty : typ; pos : pos }

and expr_desc =
  | Lit of lit
  | Var of string
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Call of call
  | StructLit of string * (string * expr) list   (* 结构体的名字：泛型结构体是实例的名字，例如 Pair<I64, Str> *)
  | Field of expr * string
  | Deref of expr        (* *b：读出 Box 里的值，不移动 *)
  | Index of expr * expr (* v[i]：Vec 的元素，是一个位置（Ast 里是 vec_get 调用） *)
  | Ref of expr          (* &p：p 的地址，锁住 p 所在的局部（Ast 里是 ref_of 调用） *)
  | Box of expr          (* 下面五个是影响所有权或控制流的内建函数，降级时各有各的规则 *)
  | Unbox of expr
  | Consume of expr      (* linear 结构体的显式消费，只能在定义它的文件里 *)
  | Alloc of expr * expr (* alloc(a, v)：把 v 放进区域 a *)
  | Panic of expr
  | Exit of expr         (* env.exit(code) *)
  | Try of expr          (* e? *)
  | Variant of variant
  | Match of { borrow : bool; scrut : expr; arms : arm list }
  | Arena of string * block
  | If of expr * block * block option
  | Block of block

and call = {
  callee : string;
      (* 最终调用的函数：内建函数、提升出的闭包、特化；mono 之前泛型函数是它的名字，之后是实例的名字
         （内建类型的 to_str 已经换成 str_from_i64 这样的内建函数） *)
  targs : ty list;       (* 泛型调用的类型实参；mono 之后为空（现在实例在类型检查里生成，总是空） *)
  modes : mode list;     (* 每个实参的约定，和 args 一样长，按 callee 的签名 *)
  borrows : (string * int) list;
      (* 返回借用（Lean `Projection.Return`）：结果借自哪些 let 参数（参数名, 第几个参数）；不返回借用时为空 *)
  args : expr list;
}

(* 枚举（含 Result）的构造。payload 写的时候省略的 ()（.Some 就是 .Some(())）已经补上。
   tag 是 -1：表达式的类型是 Never（不会被用到），降级时用占位值。 *)
and variant = { variant : string; tag : int; payload : expr option }

(* match 的分支：arms 的第 i 个是第 i 个变体的分支（按 Defs 里变体的顺序），没写的变体用 _ 分支的体。
   bind：载荷绑定的变量；"" 不绑定；_ 分支在带载荷的变体上绑到 "_#"（用户写不出这个名字），随作用域析构。 *)
and arm = { bind : string; body : expr }

and stmt = { s : stmt_desc; spos : pos }

and stmt_desc =
  | Let of { var : bool; name : string; ty : ty; init : expr }
      (* var：可以重新赋值。ty 是标注的类型，没标注时是初值的类型；初值是 Never 时随便取（这个变量不会被用到） *)
  | Decl of string * ty  (* var x: T，先不给初值 *)
  | Assign of string * expr
  | SetField of expr * string * expr
  | SetPlace of expr * expr
  | While of expr * block
  | Scope of { handle : string; shared : string list; body : block }
      (* scope |s| { … }（Lean `Concurrency.Scope`）：shared 是任务只读共享的外层变量，整个 scope 里锁住它们 *)
  | Return of expr option
  | Expr of expr
  | Errdefer of { call : expr; target : string }   (* target：出错返回时这个调用要消费的变量 *)
  | Break
  | Continue
  | Destruct of { struct_name : string; fields : (string * string * ty) list; value : expr }
      (* let Node { v, next: n } = e：(字段名, 变量名, 字段的类型)，结构体本身被移走 *)

and block = { stmts : stmt list; tail : expr option }

(* 一个要降级的函数。泛型函数本身也降级并检查（定义处检查，Lean `Generics.Check`），但只输出它的实例（pipeline 按名字筛掉）。 *)
type fn_def = {
  name : string;
  params : (mode * string * ty) list;
  ret : ty;
  body : block;
  pos : pos;
  from_core : bool;  (* 预导入的核心部分里的函数：只输出用到的 *)
}

(* ---------- 闭包（Lean `Closures.Param`） ----------
   闭包 |x| e 只能写在实参的位置，传给类型是 fn(…) 的参数；fn(…) 类型的值不能存、不能返回（second-class）。
   闭包用到的外层变量是它的捕获：只读的按 let，赋值、作 inout 实参或改字段的按 inout。
   实现是按闭包特化：闭包提升成一个普通函数（捕获在前，闭包的参数在后）；调用 h(a, |x| …) 换成调用
   h 的特化 h[闭包]，它没有 fn(…) 参数，多出闭包的捕获作参数，函数体里的 f(y) 换成调用提升出的函数。
   于是捕获就是一层层传下去的额外实参：调用处按 `Calls.NArgs` 的规则检查一次（`Closures.Param` 的 inner_ok 说明这就够了），
   之后的所有权、排他性检查和翻译验证看到的都是普通的函数，不用改。闭包不能逃逸，引用的栈帧总还活着（`Closures.Param` 的 clo_live）。 *)
type closure = {
  cname : string;                      (* 提升出来的函数名，按闭包的位置和类型确定 *)
  caps : (mode * string * ty) list;    (* 捕获的外层变量（按第一次用到的顺序）：只读的 let，修改的 inout *)
  rord : (string * string list) list;
      (* 捕获的类型里的区域换成的区域参数 'c00、'c01…（按闭包所在位置的嵌套顺序，外层在前），
         和比它活得久的区域参数（只取那里真实成立的先后关系，`Closures.CapRef`） *)
}

(* 类型检查和去糖之后的整个程序。generated 是代码生成要为它们写 C 辅助函数的东西。 *)
type program = { fns : fn_def list; generated : generated }

and generated = {
  packs : (string * (closure * ty)) list;         (* Fn#…：存进 Fn(…) 值的闭包和它的类型（Lean `Closures.Stored`） *)
  fcalls : (string * ty) list;                    (* call#…：调用 Fn(…) 值 *)
  spawns : (string * closure) list;               (* spawn#…：scope 里启动的任务（Lean `Concurrency.Scope`） *)
  vec_calls : (string * (string * ty)) list;      (* vec_push<I64> 这样的 Vec 内建函数的实例 → (函数, 元素类型) *)
  options : (ty * ty) list;                       (* 元素类型 e → Option<e> 的实例：vec_pop 和运行时的辅助函数要用 *)
}
