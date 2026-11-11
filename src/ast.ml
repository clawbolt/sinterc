(* 语法树。 *)

type pos = { line : int; col : int }

(* 参数约定：let 只读借用（默认），inout 可变借用，sink 转移所有权。 *)
type mode = MLet | MInout | MSink

type ty =
  | TI64
  | TF64                (* 64 位浮点数：可以复制，不涉及所有权 *)
  | TChar               (* 一个 Unicode 标量值（Lean `Library.Utf8`）：可以复制 *)
  | TBool
  | TStr                (* 字符串：拥有一块堆内存（字节），需要析构，只能移动（Lean `Datatypes.Str`） *)
  | TBytes              (* 字节串：任意字节，和 Str 一样拥有一块堆内存、需要析构、只能移动 *)
  | TView               (* str（Lean `Strings.Slice`）：字符串切片指向的内容，只出现在 &str in a 里（TRef (TView, a)）：
                           切片不拥有内存，缓冲区归区域 a 所有，所以切片就是区域 a 里的一种引用，可以复制 *)
  | TUnit
  | TStruct of string   (* 用户定义的结构体或枚举，按名字区分 *)
  | TBox of ty          (* 拥有一块堆内存 *)
  | TVec of ty          (* 内建的动态数组 Vec<T>：拥有一块连续的堆内存（Lean `Datatypes.Vec`） *)
  | TResult of ty * ty  (* Result<T, E>：变体为 Ok(T)、Err(E) 的枚举（还没有泛型，单独记） *)
  | TRef of ty * string (* &T in a：区域 a 里的只读引用 *)
  | TArena of string    (* 区域本身，只能作为函数参数类型或 arena 块引入的名字 *)
  | TApp of string * ty list   (* Option<I64>：泛型结构体或枚举加类型实参；类型检查时换成具体的 TStruct *)
  | TParam of string * bool    (* 泛型函数里的类型参数 T；true：T: linear（Lean `Generics.Check`） *)
  | TFn of (mode * ty) list * ty   (* fn(T) -> U：只能作为函数参数的类型，实参是闭包（Lean `Closures.Param`） *)
  | TOFn of (mode * ty) list * ty  (* Fn(T) -> U：可以存、可以返回的闭包（Lean `Closures.Stored`）：拥有堆上的环境，需要析构、只能移动 *)

type binop =
  | Add | Sub | Mul | Div | Rem
  | Eq | Ne | Lt | Le | Gt | Ge
  | And | Or

type unop = Neg | Not

type expr = { e : expr_desc; pos : pos; id : int }   (* id：每个表达式唯一 *)

and expr_desc =
  | Int of int64
  | Float of float      (* 浮点数字面量 1.5、2e-3 *)
  | Char of int         (* 字符字面量 'a'、'中'：码点 *)
  | Bool of bool
  | Unit
  | Str of string       (* 字符串字面量：每次求值分配一个新的 Str *)
  | Var of string
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Call of string * expr list
  | StructLit of string * (string * expr) list
  | Field of expr * string
  | Deref of expr       (* *b：读出 Box 里的值，不移动 *)
  | Try of expr             (* e? *)
  | Variant of string * expr option   (* .A(e) / .B / Ok(e) / Err(e)：枚举的构造，类型由上下文确定 *)
  | Match of bool * expr * (string * string * expr) list
      (* match r { Ok(x) => a, Err(y) => b }，或 match s { .A(x) => a, .B => b }；没写绑定时绑定名是 ""。
         true：match let s { … }，按借用匹配 *)
  | Arena of string * block   (* arena |a| { … } *)
  | If of expr * block * block option
  | Block of block
  | Lambda of string list * expr   (* |x, y| e：闭包，写在类型是 fn(…) 的参数的实参位置，或者需要 Fn(…) 的地方 *)
  | PMatch of bool * expr * (pat * expr) list
      (* 带嵌套模式的 match（Lean `Matching.Nested`、`Matching.Coverage`）：类型检查时检查覆盖性，再展开成 Match、If、Destruct，
         之后的阶段看不到它。true：match let *)

(* 模式：_、变量、字面量、变体 .V / .V(p)、元组 (p, q, …) *)
and pat = { p : pat_desc; ppos : pos }

and pat_desc =
  | PWild
  | PBind of string
  | PLit of expr
  | PVariant of string * pat option
  | PTuple of pat list

and stmt = { s : stmt_desc; spos : pos }

and stmt_desc =
  | Let of bool * string * ty option * expr   (* true：var，可以重新赋值 *)
  | Decl of string * ty     (* var x: T，先不给初值；读之前必须赋值 *)
  | Assign of string * expr
  | SetField of expr * string * expr   (* a.f = e：给变量里的一个字段赋值（a 是位置） *)
  | SetPlace of expr * expr   (* p = e：给 Vec 的一个元素整体赋值；for x in inout v 的展开和 v[i] = e *)
  | While of expr * block
  | Return of expr option
  | Expr of expr
  | Errdefer of expr        (* 出错返回时执行的调用 *)
  | Break                   (* 跳出最内层的循环 *)
  | Continue                (* 进入最内层循环的下一轮 *)
  | Destruct of string * (string * string) list * expr
      (* let Node { v, next: n } = e：把结构体的每个字段移进各自的变量（字段名, 变量名），结构体本身被移走 *)

(* 块：若干语句，最后可以有一个作为值的表达式。 *)
and block = { stmts : stmt list; tail : expr option }

type field = { fname : string; fty : ty }

(* tparams：类型参数，true 表示标了 linear（只有函数可以标）。
   bounds：函数类型参数的 trait 约束，(T, Show) 表示 T: Show。 *)
type item =
  | Struct of { name : string; tparams : (string * bool) list; fields : field list; linear : bool; ipos : pos }
  | Enum of { name : string; tparams : (string * bool) list; variants : (string * ty option) list; ipos : pos }
      (* 每个变体至多一个载荷 *)
  | Fn of {
      name : string;
      tparams : (string * bool) list;
      bounds : (string * string) list;
      params : (mode * string * ty) list;
      ret : ty;
      body : block;
      ipos : pos;
    }
  | Trait of { name : string; methods : msig list; ipos : pos }   (* 方法签名里可以用 Self *)
  | Impl of {
      trait : string;
      tparams : (string * bool) list;   (* 泛型 impl：impl<T: ToStr> ToStr for Vec<T> *)
      bounds : (string * string) list;
      self : ty;
      fns : item list;
      ipos : pos;
    }   (* fns 都是 Fn。trait 是 "" 时是类型自己的方法 impl 类型 { … }，modules.ml 把它换成普通函数 *)

(* mbody：默认方法的函数体，impl 没写这个方法时用它 *)
and msig = { mname : string; mparams : (mode * string * ty) list; mret : ty; mbody : block option; mpos : pos }

type program = item list

(* 一个源文件解析的结果：开头的 use，和每个条目（true：标了 pub）。
   use m 引入同目录的 m.sn；use m { a, B } 另外把这几个名字直接引入。 *)
type source = { uses : (string * string list option * pos) list; items : (bool * item) list }

(* 多个文件的位置：第 i 个文件（0 是主文件）的行号从 i * line_base + 1 开始，
   位置照常是一个 pos，报错时再换回文件名和文件里的行号。 *)
let line_base = 1_000_000
let files : string array ref = ref [||]
let file_of (p : pos) = p.line / line_base

let locate default (p : pos) =
  let i = file_of p in
  if i > 0 && i < Array.length !files then (!files.(i), p.line mod line_base) else (default, p.line)

(* 结构体、枚举（加上模块前缀的名字）定义在第几个文件里：consume 只能在定义 linear 类型的文件里用。 *)
let type_file : (string, int) Hashtbl.t = Hashtbl.create 16

(* 类型自己的方法（Lean `Traits.Inherent`）：方法名 → (头部, impl 的类型参数, 编译成的函数名, 位置)。
   modules.ml 登记，类型检查按接收者的类型查找。 *)
let inherent : (string, ty * (string * bool) list * string * pos) Hashtbl.t = Hashtbl.create 16

(* 解析时发现的、语法规则表达不了的错误。 *)
exception Syntax of pos * string

(* 表达式的 id：解析时依次分配，实例化泛型函数时复制出的表达式也从这里取新的。 *)
let next_id = ref 0
let fresh_id () = incr next_id; !next_id

(* 元组：(A, B) 是内建的泛型结构体 Tuple#2<A, B>（名字里有 #，用户写不出来），字段名是 0、1、…；
   实例的名字直接写成 (A, B)。2 到 max_tuple 个元素。 *)
let max_tuple = 6
let tuple_name n = "Tuple#" ^ string_of_int n
let is_tuple g = String.length g > 6 && String.sub g 0 6 = "Tuple#"
let app_name g args = if is_tuple g then "(" ^ String.concat ", " args ^ ")" else g ^ "<" ^ String.concat ", " args ^ ">"

let rec string_of_ty = function
  | TI64 -> "I64"
  | TF64 -> "F64"
  | TChar -> "Char"
  | TBool -> "Bool"
  | TStr -> "Str"
  | TBytes -> "Bytes"
  | TView -> "str"
  | TUnit -> "()"
  | TStruct n -> n
  | TBox t -> "Box<" ^ string_of_ty t ^ ">"
  | TVec t -> "Vec<" ^ string_of_ty t ^ ">"
  | TResult (a, b) -> "Result<" ^ string_of_ty a ^ ", " ^ string_of_ty b ^ ">"
  | TRef (t, r) -> "&" ^ string_of_ty t ^ " in " ^ r
  | TArena _ -> "Arena"
  | TApp (n, ts) -> app_name n (List.map string_of_ty ts)
  | TParam (n, _) -> n
  | TFn (ps, r) ->
      let m = function MLet -> "" | MInout -> "inout " | MSink -> "sink " in
      "fn(" ^ String.concat ", " (List.map (fun (md, t) -> m md ^ string_of_ty t) ps) ^ ") -> " ^ string_of_ty r
  | TOFn (ps, r) ->
      let m = function MLet -> "" | MInout -> "inout " | MSink -> "sink " in
      "Fn(" ^ String.concat ", " (List.map (fun (md, t) -> m md ^ string_of_ty t) ps) ^ ") -> " ^ string_of_ty r
