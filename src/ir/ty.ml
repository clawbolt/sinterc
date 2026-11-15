(* 类型和参数约定：语法树（类型标注）、定义、带类型的语法树、MIR 都用它，所以不属于任何一个阶段。
   语法树里写出的类型也是这个（TApp、TParam 由类型检查换成具体的类型）；ast.ml include 这个模块。 *)

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

(* 类型的名字（不带类型实参）：Vec<I64> 是 Vec，map.Map<K, V> 是 map.Map。类型函数 T.f(…) 按它查找。 *)
let type_head = function
  | TVec _ -> "Vec"
  | TApp (n, _) | TStruct n -> ( match String.index_opt n '<' with Some i -> String.sub n 0 i | None -> n)
  | TBox _ -> "Box"
  | TResult _ -> "Result"
  | (TI64 | TF64 | TChar | TBool | TStr | TBytes) as t -> string_of_ty t
  | _ -> ""
