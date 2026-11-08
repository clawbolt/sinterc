(* C0 子集的语法树：整数、布尔、结构体、函数。 *)

type pos = { line : int; col : int }

type ty =
  | TI64
  | TBool
  | TUnit
  | TStruct of string   (* 用户定义的结构体或枚举，按名字区分 *)
  | TBox of ty          (* 拥有一块堆内存 *)
  | TVec of ty          (* 内建的动态数组 Vec<T>：拥有一块连续的堆内存（Lean M12） *)
  | TResult of ty * ty  (* Result<T, E>：变体为 Ok(T)、Err(E) 的枚举（还没有泛型，单独记） *)
  | TRef of ty * string (* &T in a：区域 a 里的只读引用 *)
  | TArena of string    (* 区域本身，只能作为函数参数类型或 arena 块引入的名字 *)
  | TApp of string * ty list   (* Option<I64>：泛型结构体或枚举加类型实参；类型检查时换成具体的 TStruct *)
  | TParam of string * bool    (* 泛型函数里的类型参数 T；true：T: linear（Lean M9） *)

type binop =
  | Add | Sub | Mul | Div | Rem
  | Eq | Ne | Lt | Le | Gt | Ge
  | And | Or

type unop = Neg | Not

type expr = { e : expr_desc; pos : pos; id : int }   (* id：每个表达式唯一 *)

and expr_desc =
  | Int of int64
  | Bool of bool
  | Unit
  | Str of string       (* 字符串字面量：暂时只能直接作为 print 的实参（C26），不是一个类型 *)
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

and stmt = { s : stmt_desc; spos : pos }

and stmt_desc =
  | Let of bool * string * ty option * expr   (* true：var，可以重新赋值 *)
  | Decl of string * ty     (* var x: T，先不给初值；读之前必须赋值 *)
  | Assign of string * expr
  | SetField of expr * string * expr   (* a.f = e：给变量里的一个字段赋值（a 是位置） *)
  | While of expr * block
  | Return of expr option
  | Expr of expr
  | Errdefer of expr        (* 出错返回时执行的调用 *)
  | Destruct of string * (string * string) list * expr
      (* let Node { v, next: n } = e：把结构体的每个字段移进各自的变量（字段名, 变量名），结构体本身被移走 *)

(* 块：若干语句，最后可以有一个作为值的表达式。 *)
and block = { stmts : stmt list; tail : expr option }

type field = { fname : string; fty : ty }

(* 参数约定：let 只读借用（默认），inout 可变借用，sink 转移所有权。 *)
type mode = MLet | MInout | MSink

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
  | Impl of { trait : string; self : ty; fns : item list; ipos : pos }   (* fns 都是 Fn *)

and msig = { mname : string; mparams : (mode * string * ty) list; mret : ty; mpos : pos }

type program = item list

(* 解析时发现的、语法规则表达不了的错误。 *)
exception Syntax of pos * string

(* 表达式的 id：解析时依次分配，实例化泛型函数时复制出的表达式也从这里取新的。 *)
let next_id = ref 0
let fresh_id () = incr next_id; !next_id

let rec string_of_ty = function
  | TI64 -> "I64"
  | TBool -> "Bool"
  | TUnit -> "()"
  | TStruct n -> n
  | TBox t -> "Box<" ^ string_of_ty t ^ ">"
  | TVec t -> "Vec<" ^ string_of_ty t ^ ">"
  | TResult (a, b) -> "Result<" ^ string_of_ty a ^ ", " ^ string_of_ty b ^ ">"
  | TRef (t, r) -> "&" ^ string_of_ty t ^ " in " ^ r
  | TArena _ -> "Arena"
  | TApp (n, ts) -> n ^ "<" ^ String.concat ", " (List.map string_of_ty ts) ^ ">"
  | TParam (n, _) -> n
