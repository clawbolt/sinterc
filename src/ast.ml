(* C0 子集的语法树：整数、布尔、结构体、函数。 *)

type pos = { line : int; col : int }

type ty =
  | TI64
  | TBool
  | TUnit
  | TStruct of string

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
  | If of expr * block * block option
  | Block of block

and stmt = { s : stmt_desc; spos : pos }

and stmt_desc =
  | Let of string * ty option * expr
  | Assign of string * expr
  | While of expr * block
  | Return of expr option
  | Expr of expr

(* 块：若干语句，最后可以有一个作为值的表达式。 *)
and block = { stmts : stmt list; tail : expr option }

type field = { fname : string; fty : ty }

type item =
  | Struct of { name : string; fields : field list; ipos : pos }
  | Fn of { name : string; params : (string * ty) list; ret : ty; body : block; ipos : pos }

type program = item list

let string_of_ty = function
  | TI64 -> "I64"
  | TBool -> "Bool"
  | TUnit -> "()"
  | TStruct n -> n
