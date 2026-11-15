(* C 后端（一）：类型和函数的 C 名字、C 类型。
   gen_names 是整个进程的表：一个名字第一次用到时编号，所以生成 C 时调用这些函数的顺序决定编号（生成的 C 要和以前逐字节相同）。 *)
open Ast

(* 泛型实例（名字里有 <）的 C 名字：编号加上去掉符号的写法，和用户写的名字（S_、u_ 开头）不会重名。 *)
let gen_names : (string, string) Hashtbl.t = Hashtbl.create 16

(* 名字本身就是 C 的标识符（只有字母、数字、下划线）时直接用；元组的实例 (I64, Bool) 等另起名字 *)
let is_ident n = String.for_all (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false) n

let cname prefix n =
  (* 泛型实例（Option<I64>）和别的模块里的名字（geom.area）不是 C 的标识符，另起一个名字 *)
  if is_ident n then prefix ^ n
  else
    match Hashtbl.find_opt gen_names n with
    | Some c -> c
    | None ->
        let clean = String.map (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '_') n in
        let c = Printf.sprintf "g%d_%s" (Hashtbl.length gen_names) clean in
        Hashtbl.add gen_names n c;
        c

(* 结构体、枚举。只差区域的实例（Option<&I64 in a> 和 Option<&I64 in b>）是同一个 C 类型。 *)
let sname d n = cname "S_" (match Defs.erase d (TStruct n) with TStruct m -> m | _ -> n)
let fname n = cname "u_" n   (* 函数 *)

(* Result<T, E> 的 C 名字由两个类型拼出来（每个用到的 Result 是一个具体的枚举）。 *)
let rec mangle d = function
  | TI64 -> "I64"
  | TF64 -> "F64"
  | TChar -> "Char"
  | TBool -> "Bool"
  | TStr -> "Str"
  | TBytes -> "Bytes"
  | TView -> "View"
  | TUnit -> "U"
  | TStruct n -> if is_ident n then n else sname d n
  | TBox t -> "B" ^ mangle d t
  | TVec t -> "V" ^ mangle d t ^ "_"
  | TResult (a, b) -> "R" ^ mangle d a ^ "_" ^ mangle d b ^ "_"
  | TRef (t, _) -> "P" ^ mangle d t
  | TArena _ -> "A"
  | TOFn _ -> "Fn"   (* 所有 Fn(…) 的 C 表示相同 *)
  | TApp _ | TParam _ | TFn _ -> failwith "泛型的类型参数和函数类型不会出现在生成的 C 里"

let rec cty d = function
  | TI64 -> "int64_t"
  | TF64 -> "double"
  | TChar -> "uint32_t"
  | TBool -> "bool"
  | TStr -> "sn_str"
  | TBytes -> "sn_str"   (* 字节串和字符串的表示相同 *)
  | TView -> "sn_view"
  | TUnit -> "sn_unit"   (* 存值的地方（变量、字段、载荷、Vec 元素）占一个字节；函数返回 () 时仍是 void *)
  | TStruct n -> sname d n
  | TBox t -> cty d t ^ "*"
  | TResult _ as t -> "S_" ^ mangle d t
  | TVec _ as t -> "S_" ^ mangle d t
  | TRef (TView, _) -> "sn_view"   (* 字符串切片：指针和长度，按值传 *)
  | TRef (t, _) -> cty d t ^ "*"
  | TArena _ -> "sn_arena"
  | TOFn _ -> "sn_fn"
  | TApp _ | TParam _ | TFn _ -> failwith "泛型的类型参数和函数类型不会出现在生成的 C 里"

(* 局部变量的 C 类型：指针局部多一层 *。 *)
let lty d (l : Mir.local_info) = if l.lref then cty d l.lty ^ "*" else cty d l.lty
