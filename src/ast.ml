(* C0 子集的语法树：整数、布尔、结构体、函数。 *)

type pos = { line : int; col : int }

type ty =
  | TI64
  | TBool
  | TUnit
  | TStruct of string
  | TBox of ty          (* 拥有一块堆内存 *)

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
  | Var of string
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Call of string * expr list
  | StructLit of string * (string * expr) list
  | Field of expr * string
  | Deref of expr       (* *b：读出 Box 里的值，不移动 *)
  | If of expr * block * block option
  | Block of block

and stmt = { s : stmt_desc; spos : pos }

and stmt_desc =
  | Let of bool * string * ty option * expr   (* true：var，可以重新赋值 *)
  | Assign of string * expr
  | While of expr * block
  | Return of expr option
  | Expr of expr

(* 块：若干语句，最后可以有一个作为值的表达式。 *)
and block = { stmts : stmt list; tail : expr option }

type field = { fname : string; fty : ty }

(* 参数约定：let 只读借用（默认），inout 可变借用，sink 转移所有权。 *)
type mode = MLet | MInout | MSink

type item =
  | Struct of { name : string; fields : field list; ipos : pos }
  | Fn of { name : string; params : (mode * string * ty) list; ret : ty; body : block; ipos : pos }

type program = item list

let rec string_of_ty = function
  | TI64 -> "I64"
  | TBool -> "Bool"
  | TUnit -> "()"
  | TStruct n -> n
  | TBox t -> "Box<" ^ string_of_ty t ^ ">"
