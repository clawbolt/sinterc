(* 类型检查：结构体按名字区分，泛型按实例化后的名字区分（见下面的“泛型”）；`return` 之后的代码类型记为 Never。
   `Ok(e)` / `Err(e)` 的类型要靠上下文确定（let 标注、返回值、实参、字段），
   所以有两种检查方式：expr 自己推出类型，check 按期望类型检查。 *)
open Ast

exception Error of pos * string

(* 一次报多个错误（C28）：每个函数各自的第一个错误，按出现顺序。 *)
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
      (* 特化里 fn(…) 参数绑定的闭包，和这个特化里传它的捕获的参数（名字和类型）（C40） *)
}

(* ---------- 闭包（C40，Lean M21） ----------
   闭包 |x| e 只能写在实参的位置，传给类型是 fn(…) 的参数；fn(…) 类型的值不能存、不能返回（second-class）。
   闭包用到的外层变量是它的捕获：只读的按 let，赋值、作 inout 实参或改字段的按 inout。
   实现是按闭包特化：闭包提升成一个普通函数（捕获在前，闭包的参数在后）；调用 h(a, |x| …) 换成调用
   h 的特化 h[闭包]，它没有 fn(…) 参数，多出闭包的捕获作参数，函数体里的 f(y) 换成调用提升出的函数。
   于是捕获就是一层层传下去的额外实参：调用处按 M3n 的规则检查一次（M21 的 inner_ok 说明这就够了），
   之后的所有权、排他性检查和翻译验证看到的都是普通的函数，不用改。闭包不能逃逸，引用的栈帧总还活着（M21 的 clo_live）。 *)
and closure = {
  cname : string;                      (* 提升出来的函数名，按闭包的位置和类型确定 *)
  caps : (mode * string * ty) list;    (* 捕获的外层变量（按第一次用到的顺序）：只读的 let，修改的 inout *)
  rord : (string * string list) list;
      (* 捕获的类型里的区域换成的区域参数 'c00、'c01…（按闭包所在位置的嵌套顺序，外层在前），
         和比它活得久的区域参数（只取那里真实成立的先后关系，M22） *)
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

(* 结构体定义和标了 linear 的结构体，给降级和代码生成用。 *)
let struct_tbl : (string, field list) Hashtbl.t = Hashtbl.create 16
let linear_tbl : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 枚举定义：变体按声明顺序，下标就是运行时的标签（Lean M8）。 *)
let enum_tbl : (string, (string * ty option) list) Hashtbl.t = Hashtbl.create 16

(* 一个类型的变体（不是枚举时为 None）。Result<T, E> 就是变体 Ok(T)、Err(E) 的枚举。
   () 载荷是真的载荷（C37：泛型代码里 T 可以是 ()），只是写的时候可以省略：Ok 就是 Ok(())，分支可以写 Ok => …。 *)
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

(* match 的分支（C46）：第 v 个变体用写了它的分支，没写的用 _ 分支（Lean M23 的 expand）。
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
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TUnit | TRef _ | TArena _ | TApp _ | TFn _ | TOFn _ -> false

(* 需要析构（或者是 linear）的类型：这些类型的值只能移动，不能复制。 *)
let rec needs_drop = function
  | TBox _ | TVec _ | TStr | TBytes | TOFn _ -> true   (* Fn(…)（C59）拥有堆上的环境 *)
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
  | TParam _ -> true            (* 类型参数按抽象类型检查：可能需要析构，只能移动（Lean M9） *)
  | TI64 | TF64 | TChar | TBool | TUnit | TRef _ | TArena _ | TApp _ | TFn _ -> false

(* 内建函数按值接收可复制的参数，记作 sink；字符串按 let（只读借用）或 inout 传入（C29）。 *)
let builtins =
  [
    "print_i64", ([ (MSink, TI64) ], TUnit);
    "print_bool", ([ (MSink, TBool) ], TUnit);
    "print", ([ (MLet, TStr) ], TUnit);
    (* C53：不换行的输出。print_str 原样写出字符串的字节；print_byte 写一个原始字节（0 到 255，否则 panic）。
       输出经过 C 的缓冲，read_line 之前、panic 和程序结束时都会刷新 *)
    "print_str", ([ (MLet, TStr) ], TUnit);
    "print_byte", ([ (MSink, TI64) ], TUnit);
    "str_len", ([ (MLet, TStr) ], TI64);
    "str_eq", ([ (MLet, TStr); (MLet, TStr) ], TBool);
    "str_concat", ([ (MLet, TStr); (MLet, TStr) ], TStr);
    "str_push", ([ (MInout, TStr); (MLet, TStr) ], TUnit);
    "str_clone", ([ (MLet, TStr) ], TStr);
    "str_from_i64", ([ (MSink, TI64) ], TStr);
    "str_from_bool", ([ (MSink, TBool) ], TStr);
    (* C47：F64。print_f64 按能还原的最短写法打印（整数值带 .0）；to_f64 把 I64 换成 F64，
       to_i64 向零取整（NaN 或超出 I64 范围时 panic）；str_to_f64 解析（格式不对时 .None）。
       f64_* 是 std.math 里 sqrt 等函数用的内建函数 *)
    "print_f64", ([ (MSink, TF64) ], TUnit);
    "str_from_f64", ([ (MSink, TF64) ], TStr);
    "str_to_f64", ([ (MLet, TStr) ], TApp ("Option", [ TF64 ]));
    "to_f64", ([ (MSink, TI64) ], TF64);
    "to_i64", ([ (MSink, TF64) ], TI64);
    "f64_sqrt", ([ (MSink, TF64) ], TF64);
    "f64_floor", ([ (MSink, TF64) ], TF64);
    "f64_ceil", ([ (MSink, TF64) ], TF64);
    "f64_round", ([ (MSink, TF64) ], TF64);
    "f64_is_nan", ([ (MSink, TF64) ], TBool);
    (* C48（Lean S7）：字符。char_from 在不是合法码点时返回 .None；str_chars 按 S7 的 decode1 逐个解码
       （不合法的字节解成 U+FFFD），结果是新的 Vec<Char>；str_char_count 是字符的个数 *)
    "print_char", ([ (MSink, TChar) ], TUnit);
    "char_code", ([ (MSink, TChar) ], TI64);
    "char_from", ([ (MSink, TI64) ], TApp ("Option", [ TChar ]));
    "str_from_char", ([ (MSink, TChar) ], TStr);
    "str_push_char", ([ (MInout, TStr); (MSink, TChar) ], TUnit);
    "str_chars", ([ (MLet, TStr) ], TVec TChar);
    "str_char_count", ([ (MLet, TStr) ], TI64);
    (* C32：读一行（读到末尾时 .None，去掉行尾的换行）、解析整数（格式不对或超出范围时 .None）、
       第 i 个字节、[a, b) 的子串（新的 Str）；下标越界时 panic *)
    "read_line", ([], TApp ("Option", [ TStr ]));
    "str_to_i64", ([ (MLet, TStr) ], TApp ("Option", [ TI64 ]));
    "str_byte", ([ (MLet, TStr); (MSink, TI64) ], TI64);
    "str_slice", ([ (MLet, TStr); (MSink, TI64); (MSink, TI64) ], TStr);
    "choose", ([], TBool);       (* 对照测试用：按运行时给定的选择序列返回真假 *)
    (* C36（Lean S3）：命令行参数（不含程序名）；读整个文件；把文件换成 s（先写临时文件再 rename，失败时原文件不变）。
       出错时 Err(说明)，返回的字符串都是新分配的 *)
    "args", ([], TVec TStr);
    "read_file", ([ (MLet, TStr) ], TResult (TStr, TStr));
    "write_file", ([ (MLet, TStr); (MLet, TStr) ], TResult (TUnit, TStr));
    (* C57：字节串 Bytes（任意字节）。get、set、push 的字节是 0 到 255 的 I64（push、set 超出时 panic），
       下标越界时 panic；str_to_bytes 复制字符串的字节；str_from_utf8 只在合法的 UTF-8 时得到 .Some（Lean S11）；
       read_bytes、write_bytes 和 read_file、write_file 一样（write_bytes 也是先写临时文件再 rename）；print_bytes 原样输出 *)
    "bytes_new", ([], TBytes);
    "bytes_len", ([ (MLet, TBytes) ], TI64);
    "bytes_get", ([ (MLet, TBytes); (MSink, TI64) ], TI64);
    "bytes_set", ([ (MInout, TBytes); (MSink, TI64); (MSink, TI64) ], TUnit);
    "bytes_push", ([ (MInout, TBytes); (MSink, TI64) ], TUnit);
    "bytes_append", ([ (MInout, TBytes); (MLet, TBytes) ], TUnit);
    "bytes_slice", ([ (MLet, TBytes); (MSink, TI64); (MSink, TI64) ], TBytes);
    "bytes_clone", ([ (MLet, TBytes) ], TBytes);
    "bytes_eq", ([ (MLet, TBytes); (MLet, TBytes) ], TBool);
    "str_to_bytes", ([ (MLet, TStr) ], TBytes);
    "str_from_utf8", ([ (MLet, TBytes) ], TApp ("Option", [ TStr ]));
    "read_bytes", ([ (MLet, TStr) ], TResult (TBytes, TStr));
    "write_bytes", ([ (MLet, TStr); (MLet, TBytes) ], TResult (TUnit, TStr));
    "print_bytes", ([ (MLet, TBytes) ], TUnit);
    (* C58：std.os 用的内建函数（名字带 os_，不占用户的名字）。和 C36 一样只读外部的状态、返回新的值（Lean S3 的形状）：
       环境变量；目录里的名字（不含 . 和 ..，按字节从小到大排好）；路径是否存在、是不是目录；建目录、删文件或空目录；
       os_run 启动子进程（不经过 shell，参数原样传过去，标准输入输出和父进程共用），返回退出码（被信号结束时是 128 + 信号）；
       os_output 另外收集子进程的标准输出，退出码不是 0 时是 Err。os_exit 见 expr（和 panic 一样不返回） *)
    "os_env", ([ (MLet, TStr) ], TApp ("Option", [ TStr ]));
    "os_list_dir", ([ (MLet, TStr) ], TResult (TVec TStr, TStr));
    "os_exists", ([ (MLet, TStr) ], TBool);
    "os_is_dir", ([ (MLet, TStr) ], TBool);
    "os_make_dir", ([ (MLet, TStr) ], TResult (TUnit, TStr));
    "os_remove", ([ (MLet, TStr) ], TResult (TUnit, TStr));
    "os_run", ([ (MLet, TStr); (MLet, TVec TStr) ], TResult (TI64, TStr));
    "os_output", ([ (MLet, TStr); (MLet, TVec TStr) ], TResult (TStr, TStr));
  ]

(* 字符串的方法调用：s.len() 就是 str_len(s)。 *)
let str_methods =
  [ "len"; "eq"; "concat"; "push"; "clone"; "to_i64"; "to_f64"; "byte"; "slice"; "chars"; "char_count"; "push_char"; "to_bytes" ]

(* 字节串的方法调用（C57）：b.get(i) 就是 bytes_get(b, i)。 *)
let bytes_methods = [ "len"; "get"; "set"; "push"; "append"; "slice"; "clone"; "eq" ]

(* 特殊处理的内建函数：参数类型不固定。 *)
let special = [ "box"; "unbox"; "consume"; "alloc"; "vec_new"; "vec_push"; "vec_pop"; "vec_len"; "vec_get"; "vec_swap"; "panic"; "os_exit" ]

(* Vec 的内建函数的参数约定（给降级用；类型由类型检查单独处理）。vec_get 是位置，不是调用。 *)
let vec_sigs =
  [
    "vec_new", ([], TUnit);
    "vec_push", ([ (MInout, TUnit); (MSink, TUnit) ], TUnit);
    "vec_pop", ([ (MInout, TUnit) ], TUnit);
    "vec_len", ([ (MLet, TUnit) ], TI64);
    "vec_swap", ([ (MInout, TUnit); (MLet, TI64); (MLet, TI64) ], TUnit);
  ]

(* Vec 内建函数的实例（降级后的调用名，例如 vec_push<I64>）→ 函数（push 等）和元素类型，给代码生成找辅助函数用。 *)
let vec_calls : (string, string * ty) Hashtbl.t = Hashtbl.create 16

(* 方法调用 e.f(…)（UFCS）：解析成 Call (".f", e :: …)。类型检查按接收者的类型确定函数名
   （Vec 的 push / pop / len / get 是内建的 vec_…，其他就是函数 f），按表达式 id 记下来，检查完再把名字换掉。 *)
let method_res : (int, string) Hashtbl.t = Hashtbl.create 16

let is_method f = f <> "" && f.[0] = '.'

(* 调用实际的函数名。 *)
let callee (x : expr) f =
  if is_method f then Option.value (Hashtbl.find_opt method_res x.id) ~default:f else f

(* 位置表达式（变量、字段、解引用、vec_get(v, i)）所在的变量；其他表达式没有。 *)
let rec root (x : expr) =
  match x.e with
  | Var v -> Some v
  | Field (a, _) | Deref a -> root a
  | Call (f, [ a; _ ]) when callee x f = "vec_get" -> root a
  | _ -> None

(* inout 实参必须是可修改的位置：var 变量或 inout / sink 参数里面的位置。 *)
let inout_place env (a : expr) =
  match root a with
  | None -> err a.pos "inout 实参必须是变量、字段或解引用这样的位置"
  | Some v -> (
      note ~write:true env.vars v;
      match List.assoc_opt v env.vars with
      | Some (_, true) -> ()
      | Some (TOFn _, _) when a.e = Var v ->
          err a.pos "%s 不可修改：调用 Fn(…) 闭包时闭包按 inout（它可以改自己的环境），要用 var 声明，或者作为 inout / sink 参数" v
      | _ -> err a.pos "%s 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）" v)

let show = function Ty t -> string_of_ty t | Never -> "!"

(* ---------- 区域（Lean M5、M7（四）） ----------
   当前可见的区域，内层在前；每个区域记下比它活得久的区域。
   函数的区域参数（a: Arena，以及参数类型里出现的其他区域名）之间没有顺序（D3）；
   arena 块引入的区域比所有外层区域和区域参数都活得短。 *)
let regions : (string * string list) list ref = ref []

(* --unchecked（C22）：不检查区域的先后和 R3，照样导出，看 Lean 是否独立地拒绝。 *)
let lax_regions = ref false

(* 当前所在的循环有几层：break / continue 只能用在循环里（C31）。 *)
let loop_depth = ref 0

let outlives x y =
  !lax_regions || x = y || match List.assoc_opt y !regions with Some a -> List.mem x a | None -> false

(* 实例名 → 泛型名和类型实参。 *)
let inst_tbl : (string, string * ty list) Hashtbl.t = Hashtbl.create 16

(* 泛型结构体、枚举 g 的第 i 个类型参数能不能协变（后面定义）。 *)
let covariant_fwd : (string -> int -> bool) ref = ref (fun _ _ -> false)

(* got 能不能当作 want 用：外层区域的引用可以当作内层区域的引用（D2 协变），其余要求相同。
   同一个泛型结构体、枚举的两个实例：能协变的类型参数逐个按 sub 比较，其他的要求相同（C24，Lean M16）。 *)
let rec sub got want =
  match got, want with
  | TRef (t, x), TRef (t', y) -> t = t' && outlives x y
  | TResult (a, b), TResult (c, d) -> sub a c && sub b d
  | TStruct a, TStruct b when a <> b -> (
      match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
      | Some (g, ts), Some (h, us) when g = h && List.length ts = List.length us ->
          List.for_all Fun.id
            (List.mapi (fun i (t, u) -> t = u || (!covariant_fwd g i && sub t u)) (List.combine ts us))
      | _ -> false)
  | _ -> got = want

(* 泛型结构体、枚举的实例的类型实参（不是实例时为空）。区域名的收集、换名都要看进类型实参（C20，Lean M15）。 *)
let inst_args n = match Hashtbl.find_opt inst_tbl n with Some (_, ts) -> ts | None -> []

(* 换名之后重新实例化（instantiate 在后面定义）。 *)
let instantiate_fwd : (string -> ty list -> ty) ref = ref (fun _ _ -> assert false)

let rec mentions r = function
  | TRef (t, x) -> x = r || mentions r t
  | TArena x -> x = r
  | TBox t | TVec t -> mentions r t
  | TResult (a, b) -> mentions r a || mentions r b
  | TStruct n -> List.exists (mentions r) (inst_args n)
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TUnit | TApp _ | TParam _ | TFn _ | TOFn _ -> false   (* fn(…)、Fn(…) 里不能有区域（check_ty） *)

let rec region_names = function
  | TRef (t, x) -> x :: region_names t
  | TArena x -> [ x ]
  | TBox t | TVec t -> region_names t
  | TResult (a, b) -> region_names a @ region_names b
  | TStruct n -> List.concat_map region_names (inst_args n)
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TUnit | TApp _ | TParam _ | TFn _ | TOFn _ -> []

let rec rename s = function
  | TRef (t, x) -> TRef (rename s t, Option.value (List.assoc_opt x s) ~default:x)
  | TArena x -> TArena (Option.value (List.assoc_opt x s) ~default:x)
  | TBox t -> TBox (rename s t)
  | TVec t -> TVec (rename s t)
  | TResult (a, b) -> TResult (rename s a, rename s b)
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) when List.exists (fun t -> region_names t <> []) ts -> !instantiate_fwd g (List.map (rename s) ts)
      | _ -> t)
  | t -> t

(* 区域名都换成 _：C 后端里只差区域的类型是同一个 C 类型。 *)
let rec erase = function
  | TRef (t, _) -> TRef (erase t, "_")
  | TBox t -> TBox (erase t)
  | TVec t -> TVec (erase t)
  | TResult (a, b) -> TResult (erase a, erase b)
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) -> TStruct (Ast.app_name g (List.map (fun t -> string_of_ty (erase t)) ts))
      | None -> t)
  | t -> t

(* 两个分支的类型合并：Never 和任何类型相容；区域不同时取活得短的那个。 *)
let join pos what a b =
  match a, b with
  | Never, x | x, Never -> x
  | Ty x, Ty y when sub x y -> Ty y
  | Ty x, Ty y when sub y x -> Ty x
  | _ -> err pos "%s的两个分支类型不同：%s 和 %s" what (show a) (show b)

let expect pos want got =
  match got with
  | Never -> ()
  | Ty t when sub t want -> ()
  | Ty t -> err pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty want) (string_of_ty t)

(* param：函数参数的类型，可以是 Arena。 *)
let rec check_ty ?(param = false) env pos = function
  | TStruct n when not (Hashtbl.mem env.structs n || Hashtbl.mem enum_tbl n) -> err pos "未知类型 %s" n
  | TStruct n -> List.iter (check_ty env pos) (inst_args n)
  | TBox t ->
      if is_linear t then err pos "Box 里不能放 linear 值（还不能从 Box 里移出）";
      check_ty env pos t
  | TVec t -> check_ty env pos t
  | TResult (a, b) -> check_ty env pos a; check_ty env pos b
  | TArena _ when not param -> err pos "Arena 只能作为函数参数的类型"
  | TFn _ when not param ->
      err pos "函数类型 fn(…) 只能作为函数参数的类型（闭包不能存进变量、字段、Vec，也不能返回）"
  | TFn (ps, r) ->
      if List.exists (fun (_, t) -> region_names t <> []) ps || region_names r <> [] then
        err pos "函数类型 fn(…) 里暂不能有引用或区域";
      List.iter (fun (_, t) -> check_ty env pos t) ps;
      check_ty env pos r
  | TOFn (ps, r) ->
      (* Fn(…)（C59，Lean M26）：可以存、可以返回，所以里面不能有引用或区域（环境里也不能有，见 pack） *)
      if List.exists (fun (_, t) -> region_names t <> []) ps || region_names r <> [] then
        err pos "函数类型 Fn(…) 里暂不能有引用或区域";
      List.iter (fun (_, t) -> check_ty env pos t) ps;
      check_ty env pos r
  | TRef (t, r) ->
      if not (List.mem_assoc r !regions) then err pos "未知区域 %s" r;
      if region_names t <> [] then err pos "还不支持指向引用或区域的引用";
      if t = TUnit then err pos "不能引用 ()";
      check_ty env pos t
  | _ -> ()

(* ---------- 泛型（Lean M9） ----------
   泛型结构体和枚举按用到的类型实参实例化成具体的类型，名字就是写法本身（例如 "Option<I64>"），
   登记到 struct_tbl / enum_tbl，之后的检查、降级、代码生成都把它当普通类型。
   泛型函数里的类型参数记作 TParam，按抽象类型检查：值只能移动，普通的 T 可以自动析构，
   T: linear 必须交出去；不能对 T 做运算、取字段或 match。调用处推出类型实参并检查约束，
   用到的每个实例复制一份函数体，换成具体类型后照常检查（单态化）。 *)

(* 泛型结构体、枚举的定义：类型参数名和原始定义（类型还没解析）。 *)
let gtypes : (string, string list * item) Hashtbl.t = Hashtbl.create 16

(* 泛型函数的类型参数；解析过类型的泛型函数本身，实例化时复制它。 *)
let gparams : (string, (string * bool) list) Hashtbl.t = Hashtbl.create 16
let gfns : (string, item) Hashtbl.t = Hashtbl.create 16

(* trait（Lean M10）：每个方法当作带一个类型参数 Self 的泛型函数，Self 换成实现的类型。
   traits：trait → 方法名；methods：方法名 → trait；impls：trait → 它的每个 impl；
   gbounds：泛型函数（和方法）的类型参数要满足的 trait 约束。
   impl 里的方法是普通的具体函数，名字记作 方法<类型>（和泛型函数的实例同一种写法），单态化时直接调用它。
   C56（Lean M25）：impl 可以是泛型的（头部 Vec<T>，约束 T: ToStr），它的方法是以 impl 的类型参数为类型参数的泛型函数，
   名字是 方法<头部>；trait 的方法可以有默认的函数体（defaults），它是泛型函数 方法（类型参数 Self）。 *)
type impl_info = {
  head : ty;   (* 实现的类型，impl 的类型参数是 TParam *)
  itps : string list;   (* impl 的类型参数 *)
  ibounds : (string * string) list;
  defined : string list;   (* impl 里写了的方法 *)
}

let traits : (string, string list) Hashtbl.t = Hashtbl.create 16
let methods : (string, string) Hashtbl.t = Hashtbl.create 16
let impls : (string, impl_info) Hashtbl.t = Hashtbl.create 16
let gbounds : (string, (string * string) list) Hashtbl.t = Hashtbl.create 16
let defaults : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 泛型 impl 的方法的实例（C56）：实例名 方法<具体类型> → (泛型函数 方法<头部>, impl 的类型实参)。 *)
let minst : (string, string * ty list) Hashtbl.t = Hashtbl.create 16

(* 内建类型的 ToStr（C56）：实例名 to_str<I64> 等直接调用内建函数，降级时换名。 *)
let aliases : (string, string) Hashtbl.t = Hashtbl.create 16

(* 每个泛型调用（按调用表达式的 id）推出的类型实参，给降级时找实例的名字用。 *)
let call_inst : (int, ty list) Hashtbl.t = Hashtbl.create 16

(* 还没有生成的函数实例。 *)
let pending : (string * ty list) Queue.t = Queue.create ()
let requested : (string, unit) Hashtbl.t = Hashtbl.create 16

let inst_name g ts = Ast.app_name g (List.map string_of_ty ts)

(* 闭包（C40）：当前检查的函数会不会生成代码（不是泛型函数、没有 fn(…) 参数；泛型实例、特化、提升出的闭包都是）。
   只有这时才提升闭包、登记特化、改写调用；其他函数里的闭包只检查。 *)
let concrete = ref false
let cur_fn = ref ""

(* 函数体里有闭包、自己又不生成代码的泛型函数：不在定义处降级（它的实例照常降级、检查）。 *)
let skip_lower : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 还没检查的提升出的闭包和特化：条目、特化里 fn(…) 参数绑定的闭包、这些 fn(…) 参数的名字和类型（仍可在函数体里用）、
   区域参数的先后关系（M22）。 *)
let pending_items :
    (item * (string * (closure * (string * ty) list)) list * (string * ty) list * (string * string list) list) Queue.t =
  Queue.create ()

(* 调用的改写：调用表达式的 id → 新的函数名和实参，检查完之后换上（和方法调用的名字一样）。 *)
let rewrites : (int, string * expr list) Hashtbl.t = Hashtbl.create 16

(* ---------- 可以存起来的闭包 Fn(…)（C59，Lean M26） ----------
   写在需要 Fn(pts) -> r 的地方（let 标注、返回值、实参、字段、Vec 元素）的闭包：照 C40 提升成普通函数
   （捕获在前，只读的按 let、修改的按 inout），闭包表达式换成调用内建的 Fn#提升出的函数名(捕获…)，
   捕获按 sink 传入（按值：会移动的值被移走，可复制的值复制），存进堆上的环境。调用 f(x) 换成 call#类型(f, x)，
   闭包按 inout：闭包体访问的环境和实参不重叠（M26 的 env_disjoint），所以提升出的函数照常检查就够了。
   Fn 值需要析构（析构环境），只能移动；环境里不能有引用、区域和 fn(…) 参数（M26 的 ok：存起来也不会悬空）。
   packs：Fn#… → (闭包, 它的类型)；fcalls：call#… → 闭包的类型。代码生成为它们写 C 函数。 *)
let packs : (string, closure * ty) Hashtbl.t = Hashtbl.create 16
let fcalls : (string, ty) Hashtbl.t = Hashtbl.create 16


(* 带嵌套模式的 match 展开后的表达式（按 PMatch 的 id，C55）：map_body 把 PMatch 换成它，之后的阶段只看到 Match、If、Destruct。 *)
let pdesugar : (int, expr) Hashtbl.t = Hashtbl.create 16

(* 当前函数按 let 传入的参数（借来的）：在它们上面的嵌套模式按借用拆开，和 match let 一样（C55） *)
let let_params : string list ref = ref []

(* 解析过类型的非泛型函数，特化时复制。 *)
let fn_items : (string, item) Hashtbl.t = Hashtbl.create 16

let inst_of n = Option.map fst (Hashtbl.find_opt inst_tbl n)

let is_generic f = Hashtbl.mem gparams f

let rec has_arena = function
  | TArena _ -> true
  | TBox t | TVec t | TRef (t, _) -> has_arena t
  | TResult (a, b) -> has_arena a || has_arena b
  | _ -> false

(* 类型实参的共同限制。泛型函数和 Vec 的类型实参可以含引用（C19）：实例的区域参数就是实参里的区域名（Lean M14）；
   可以是 ()（C37）：() 是 copy 类别，换进任何类型参数都满足约束，实例照样安全（Lean M9u）。 *)
let type_arg pos t =
  if has_arena t then err pos "类型实参不能是 Arena"

let lin_arg pos t =
  if is_linear t then err pos "泛型结构体、枚举的类型实参暂不能是 linear 值（%s）" (string_of_ty t)

(* 泛型结构体、枚举的类型实参可以含引用（C20，Lean M15），还不能是 linear 值（字段和载荷的 linear 规则还没有和泛型一起证明）。 *)
let data_arg pos t =
  type_arg pos t;
  lin_arg pos t

(* 解析类型：sc 把类型参数名映射到类型（泛型函数里是 TParam，实例化时是类型实参）；
   Option<I64> 这样的写法换成实例。 *)
let rec resolve sc pos = function
  | TStruct n when List.mem_assoc n sc -> List.assoc n sc
  | TStruct "Vec" when not (List.mem_assoc "Vec" sc) -> err pos "Vec 要写元素类型：Vec<…>"
  | TStruct n when Hashtbl.mem gtypes n -> err pos "%s 是泛型类型，要写类型实参：%s<…>" n n
  | TApp ("Vec", ts) -> (
      (* 内建的动态数组：元素不能是引用或 linear 值（pop 返回 Option<T>，Option 的实参还不能是 linear 的） *)
      match List.map (resolve sc pos) ts with
      | [ t ] ->
          type_arg pos t;
          lin_arg pos t;
          TVec t
      | ts -> err pos "Vec 需要 1 个类型实参，实际给了 %d 个" (List.length ts))
  | TApp (n, ts) -> (
      match Hashtbl.find_opt gtypes n with
      | None -> err pos "%s 不是泛型类型，不能写类型实参" n
      | Some (ps, _) ->
          if List.length ps <> List.length ts then
            err pos "%s 需要 %d 个类型实参，实际给了 %d 个" n (List.length ps) (List.length ts);
          let ts = List.map (resolve sc pos) ts in
          List.iter (data_arg pos) ts;
          instantiate n ts)
  | TBox t -> TBox (resolve sc pos t)
  | TVec t -> TVec (resolve sc pos t)
  | TResult (a, b) -> TResult (resolve sc pos a, resolve sc pos b)
  | TRef (t, r) -> TRef (resolve sc pos t, r)
  | TFn (ps, r) -> TFn (List.map (fun (m, t) -> (m, resolve sc pos t)) ps, resolve sc pos r)
  | TOFn (ps, r) -> TOFn (List.map (fun (m, t) -> (m, resolve sc pos t)) ps, resolve sc pos r)
  | t -> t

and instantiate g ts =
  let name = inst_name g ts in
  if not (Hashtbl.mem inst_tbl name) then begin
    Hashtbl.replace inst_tbl name (g, ts);
    if Hashtbl.length inst_tbl > 1000 then
      err { line = 1; col = 1 } "泛型类型的实例超过 1000 个（可能是类型实参越来越大的递归类型，例如 W<T> 里有 Box<W<Box<T>>>）";
    let ps, it = Hashtbl.find gtypes g in
    let sc = List.combine ps ts in
    match it with
    | Struct { fields; linear; ipos; _ } ->
        Hashtbl.replace struct_tbl name
          (List.map (fun (f : field) -> { f with fty = resolve sc ipos f.fty }) fields);
        if linear then Hashtbl.replace linear_tbl name ()
    | Enum { variants; ipos; _ } ->
        Hashtbl.replace enum_tbl name
          (List.map (fun (v, t) -> (v, Option.map (resolve sc ipos) t)) variants)
    | Fn _ | Trait _ | Impl _ -> ()
  end;
  TStruct name

let () = instantiate_fwd := instantiate

(* 把类型参数换成类型（一次换完，换进去的类型不再替换）；实例里的类型参数也一起换。 *)
let rec subst s = function
  | TParam (n, _) as t -> Option.value (List.assoc_opt n s) ~default:t
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) -> instantiate g (List.map (subst s) ts)
      | None -> t)
  | TBox t -> TBox (subst s t)
  | TVec t -> TVec (subst s t)
  | TResult (a, b) -> TResult (subst s a, subst s b)
  | TRef (t, r) -> TRef (subst s t, r)
  | TFn (ps, r) -> TFn (List.map (fun (m, t) -> (m, subst s t)) ps, subst s r)
  | TOFn (ps, r) -> TOFn (List.map (fun (m, t) -> (m, subst s t)) ps, subst s r)
  | t -> t

(* 类型里出现的类型参数。 *)
let rec params_of = function
  | TParam (n, _) -> [ n ]
  | TStruct n -> (
      match Hashtbl.find_opt inst_tbl n with Some (_, ts) -> List.concat_map params_of ts | None -> [])
  | TBox t | TVec t | TRef (t, _) -> params_of t
  | TResult (a, b) -> params_of a @ params_of b
  | TFn (ps, r) | TOFn (ps, r) -> List.concat_map (fun (_, t) -> params_of t) ps @ params_of r
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TUnit | TArena _ | TApp _ -> []

(* ---- impl 的查找（C56，Lean M25） ---- *)

(* 泛型类型的实例 Pair<I64> 记作名字，这里拆回 (Pair, [I64])；元组也一样。 *)
let app_of n = Hashtbl.find_opt inst_tbl n

(* 头部 p 对类型 t 的匹配（M25 的 mtch）：vs 是 impl 的类型参数，s 是已经匹配出的类型实参。
   t 里的类型参数（当前泛型函数的）当作普通的类型。 *)
let rec tmatch vs p t s =
  match p, t with
  | TParam (n, _), _ when List.mem n vs -> (
      match List.assoc_opt n s with Some u -> if u = t then Some s else None | None -> Some ((n, t) :: s))
  | TStruct a, TStruct b -> (
      match app_of a, app_of b with
      | Some (g, ps), Some (h, ts) when g = h && List.length ps = List.length ts -> tmatch_list vs ps ts s
      | None, None when a = b -> Some s
      | _ -> None)
  | TBox p, TBox t | TVec p, TVec t -> tmatch vs p t s
  | TResult (a, b), TResult (c, d) -> tmatch_list vs [ a; b ] [ c; d ] s
  | _ -> if p = t then Some s else None

and tmatch_list vs ps ts s =
  match ps, ts with
  | [], [] -> Some s
  | p :: ps, t :: ts -> Option.bind (tmatch vs p t s) (tmatch_list vs ps ts)
  | _ -> None

(* 两个头部可能有共同的实例（M25 的 compat，保守：类型参数和什么都相容）。 *)
let rec compat p q =
  match p, q with
  | TParam _, _ | _, TParam _ -> true
  | TStruct a, TStruct b -> (
      match app_of a, app_of b with
      | Some (g, ps), Some (h, qs) -> g = h && List.length ps = List.length qs && List.for_all2 compat ps qs
      | None, None -> a = b
      | _ -> false)
  | TBox p, TBox q | TVec p, TVec q -> compat p q
  | TResult (a, b), TResult (c, d) -> compat a c && compat b d
  | _ -> p = q

(* 头部能匹配 t 的 tr 的 impl（Coherent 保证至多一个），和匹配出的类型实参。 *)
let find_impl tr t =
  List.find_map
    (fun i -> Option.map (fun s -> (i, s)) (tmatch i.itps i.head t []))
    (Hashtbl.find_all impls tr)

(* t: tr 成立吗（M25 的 solve）：类型参数查当前函数的约束 tb，否则找 impl，再查它的约束。
   impl 的头部不是单独的类型参数，约束里的类型都是 t 的真子项，所以递归一定会停。 *)
let rec has_impl tb tr t =
  match t with
  | TParam (m, _) -> List.mem (m, tr) tb
  | _ -> (
      match find_impl tr t with
      | Some (i, s) -> List.for_all (fun (n, tr') -> has_impl tb tr' (List.assoc n s)) i.ibounds
      | None -> false)

(* t: tr 不成立时，找出最里面不成立的那个约束（报错用）。 *)
let rec why_not tb tr t =
  match t with
  | TParam _ -> (tr, t)
  | _ -> (
      match find_impl tr t with
      | Some (i, s) -> (
          match List.find_opt (fun (n, tr') -> not (has_impl tb tr' (List.assoc n s))) i.ibounds with
          | Some (n, tr') -> why_not tb tr' (List.assoc n s)
          | None -> (tr, t))
      | None -> (tr, t))

(* 协变（C24，Lean M16）：泛型结构体、枚举 g 的第 i 个类型参数只出现在只读的位置时，
   g<…外层…> 可以当作 g<…内层…> 用。只读的位置：字段、载荷直接是这个参数，
   或者在 Result 里，或者是另一个实例里能协变的类型实参。
   Box、Vec 里的保守地算不变（实例之间不比较）；可以修改的 inout 实参本来就要求类型完全相同。 *)
let cov_memo : (string * int, bool) Hashtbl.t = Hashtbl.create 16

let rec covariant g i =
  match Hashtbl.find_opt cov_memo (g, i) with
  | Some b -> b
  | None ->
      (* 先记一个结果，防止递归的类型（经过自己）死循环 *)
      Hashtbl.replace cov_memo (g, i) true;
      let ps, _ = Hashtbl.find gtypes g in
      let p = List.nth ps i in
      let abs = match instantiate g (List.map (fun q -> TParam (q, false)) ps) with TStruct n -> n | _ -> g in
      let tys =
        match Hashtbl.find_opt struct_tbl abs, Hashtbl.find_opt enum_tbl abs with
        | Some fs, _ -> List.map (fun (f : field) -> f.fty) fs
        | None, Some vs -> List.filter_map snd vs
        | None, None -> []
      in
      let b = List.for_all (only_cov p) tys in
      Hashtbl.replace cov_memo (g, i) b;
      b

(* 类型参数 p 在 t 里只出现在只读的位置（或者不出现）。 *)
and only_cov p t =
  match t with
  | TParam _ -> true
  | TResult (a, b) -> only_cov p a && only_cov p b
  | TStruct n -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) ->
          List.for_all Fun.id
            (List.mapi (fun i t -> (not (List.mem p (params_of t))) || (covariant g i && only_cov p t)) ts)
      | None -> true)
  | TBox _ | TVec _ | TRef _ | TFn _ | TOFn _ -> not (List.mem p (params_of t))
  | TI64 | TF64 | TChar | TBool | TStr | TBytes | TUnit | TArena _ | TApp _ -> true

let () = covariant_fwd := covariant

(* 推断类型实参：模式 p 里的类型参数按 t 里对应位置的类型确定，先确定的为准。 *)
let rec unify s p t =
  match p, t with
  | TParam (n, _), t -> if not (List.mem_assoc n !s) then s := (n, t) :: !s
  | TBox p, TBox t | TVec p, TVec t -> unify s p t
  | TResult (a, b), TResult (c, d) -> unify s a c; unify s b d
  | TFn (ps, r), (TFn (qs, u) | TOFn (qs, u)) | TOFn (ps, r), TOFn (qs, u) when List.length ps = List.length qs ->
      List.iter2 (fun (_, p) (_, q) -> unify s p q) ps qs;
      unify s r u
  | TStruct a, TStruct b -> (
      match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
      | Some (g, ps), Some (h, ts) when g = h -> List.iter2 (unify s) ps ts
      | _ -> ())
  | _ -> ()

(* 调用 Fn(…) 类型 t 的闭包的内建函数 call#t（C59）：闭包按 inout，其余参数按类型里写的约定。 *)
let fcall_name (fns : (string, (mode * ty) list * ty) Hashtbl.t) t =
  match t with
  | TOFn (pts, r) ->
      let n = "call#" ^ string_of_ty t in
      if not (Hashtbl.mem fns n) then begin
        let s = ((MInout, t) :: pts, r) in
        Hashtbl.replace sigs n s;
        Hashtbl.replace fns n s
      end;
      if params_of t = [] then Hashtbl.replace fcalls n t;
      n
  | _ -> assert false

(* 改写函数体里写出的类型（let 标注、var 声明），方法调用换成解析出的函数名（检查过的才有）。
   fresh：表达式换新的 id（复制出实例时用）；不是 fresh 时还换上记下的调用改写（C40）。 *)
let map_body ~fresh (ft : pos -> ty -> ty) (b : block) : block =
  let rec ex (x : expr) =
    let e =
      match x.e with
      | (Int _ | Float _ | Char _ | Bool _ | Unit | Str _ | Var _) as d -> d
      | Binop (o, a, b) -> Binop (o, ex a, ex b)
      | Unop (o, a) -> Unop (o, ex a)
      | Call (f, args) -> (
          match if fresh then None else Hashtbl.find_opt rewrites x.id with
          | Some (g, args) -> Call (g, List.map ex args)
          | None -> Call (callee x f, List.map ex args))
      | StructLit (n, fs) -> StructLit (n, List.map (fun (f, e) -> (f, ex e)) fs)
      | Field (a, f) -> Field (ex a, f)
      | Deref a -> Deref (ex a)
      | Try a -> Try (ex a)
      | Variant (v, a) -> Variant (v, Option.map ex a)
      | Match (bw, s, arms) -> Match (bw, ex s, List.map (fun (v, y, e) -> (v, y, ex e)) arms)
      | PMatch (bw, s, arms) -> (
          match if fresh then None else Hashtbl.find_opt pdesugar x.id with
          | Some d -> (ex d).e
          | None -> PMatch (bw, ex s, List.map (fun (q, e) -> (q, ex e)) arms))
      | Arena (r, b) -> Arena (r, blk b)
      | If (c, a, b) -> If (ex c, blk a, Option.map blk b)
      | Block b -> Block (blk b)
      | Lambda (ps, a) -> (
          (* 需要 Fn(…) 的地方的闭包（C59）：换成 Fn#…(捕获…) *)
          match if fresh then None else Hashtbl.find_opt rewrites x.id with
          | Some (g, args) -> Call (g, List.map ex args)
          | None -> Lambda (ps, ex a))
    in
    { x with e; id = (if fresh then fresh_id () else x.id) }
  and st (s : stmt) =
    let d =
      match s.s with
      | Let (m, v, t, e) -> Let (m, v, Option.map (ft s.spos) t, ex e)
      | Decl (v, t) -> Decl (v, ft s.spos t)
      | Assign (v, e) -> Assign (v, ex e)
      | SetField (a, f, e) -> SetField (ex a, f, ex e)
      | SetPlace (a, e) -> SetPlace (ex a, ex e)
      | While (c, b) -> While (ex c, blk b)
      | Return e -> Return (Option.map ex e)
      | Expr e -> Expr (ex e)
      | Errdefer e -> Errdefer (ex e)
      | (Break | Continue) as d -> d
      | Destruct (n, bs, e) -> Destruct (n, bs, ex e)
    in
    { s with s = d }
  and blk (b : block) = { stmts = List.map st b.stmts; tail = Option.map ex b.tail } in
  blk b

(* Vec 的内建函数按元素类型实例化：降级时调用名换成 vec_push<T> 这样的实例名；
   元素类型是具体类型时登记，代码生成为它写辅助函数。 *)
let vec_inst (x : expr) b t =
  Hashtbl.replace call_inst x.id [ t ];
  if params_of t = [] then Hashtbl.replace vec_calls (inst_name ("vec_" ^ b) [ t ]) (b, t)

(* 泛型函数 f 按类型实参 ts 的实例：复制函数体（表达式换新的 id），类型参数换成具体类型。
   泛型 impl 的方法（C56）：方法<具体类型> 是 方法<头部> 按匹配出的类型实参的实例。 *)
let instance (f, ts) =
  let name = inst_name f ts in
  let f, ts = Option.value (Hashtbl.find_opt minst name) ~default:(f, ts) in
  match Hashtbl.find gfns f with
  | Fn g ->
      let s = List.combine (List.map fst g.tparams) ts in
      Fn
        {
          g with
          name;
          tparams = [];
          bounds = [];
          params = List.map (fun (m, x, t) -> (m, x, subst s t)) g.params;
          ret = subst s g.ret;
          body = map_body ~fresh:true (fun _ -> subst s) g.body;
        }
  | it -> it

(* trait 的方法 f 对具体类型 t 第一次被调用（C56）：impl 里写了的就是函数 f<t>；
   内建类型的 to_str 换成内建函数；否则生成实例（泛型 impl 的方法，或者默认方法）。 *)
let method_inst f t =
  let name = inst_name f [ t ] in
  if not (Hashtbl.mem sigs name) then
    match f, t with
    | "to_str", TI64 -> Hashtbl.replace aliases name "str_from_i64"
    | "to_str", TF64 -> Hashtbl.replace aliases name "str_from_f64"
    | "to_str", TChar -> Hashtbl.replace aliases name "str_from_char"
    | "to_str", TBool -> Hashtbl.replace aliases name "str_from_bool"
    | "to_str", TStr -> Hashtbl.replace aliases name "str_clone"
    | _ -> (
        match find_impl (Hashtbl.find methods f) t with
        | Some (i, s) when List.mem f i.defined ->
            Hashtbl.replace minst name (inst_name f [ i.head ], List.map (fun n -> List.assoc n s) i.itps);
            Queue.add (f, [ t ]) pending
        | Some _ -> Queue.add (f, [ t ]) pending
        | None -> ())

(* 改写调用时新加的实参：变量 v（类型是 t），类型直接记下（C40）。 *)
let var_expr pos v t =
  let id = fresh_id () in
  Hashtbl.replace types id (Ty t);
  { e = Var v; pos; id }

(* 每个表达式的类型按 id 记下来，给降级用。 *)
(* 算术的类型：左边是 F64 时按 F64，否则按 I64（右边、报错都跟着它）。 *)
let num = function Ty TF64 -> TF64 | _ -> TI64

(* ---- 嵌套模式（C55，Lean M24、M24b） ---- *)

let pm_count = ref 0
let fresh_pvar () = incr pm_count; "#p" ^ string_of_int !pm_count
let mkx pos e = { e; pos; id = fresh_id () }

let lit_key (l : expr) =
  match l.e with
  | Int n -> "i" ^ Int64.to_string n
  | Unop (Neg, { e = Int n; _ }) -> "i" ^ Int64.to_string (Int64.neg n)
  | Bool b -> "b" ^ string_of_bool b
  | Str s -> "s" ^ s
  | Char c -> "c" ^ string_of_int c
  | _ -> ""

let lit_ty (l : expr) =
  match l.e with Int _ | Unop (Neg, _) -> TI64 | Bool _ -> TBool | Str _ -> TStr | Char _ -> TChar | _ -> TUnit

let show_key k =
  let rest = String.sub k 1 (String.length k - 1) in
  match k.[0] with
  | 's' -> "\"" ^ rest ^ "\""
  | 'c' ->
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b (Uchar.of_int (int_of_string rest));
      "'" ^ Buffer.contents b ^ "'"
  | _ -> rest

let copy_lit (l : expr) =
  mkx l.pos (match l.e with Unop (Neg, a) -> Unop (Neg, mkx a.pos a.e) | d -> d)

let rec pvars (q : pat) =
  match q.p with
  | PBind x -> [ x ]
  | PVariant (_, Some r) -> pvars r
  | PTuple qs -> List.concat_map pvars qs
  | PWild | PLit _ | PVariant (_, None) -> []

(* M24b 的 depth、lits *)
let rec pat_depth (q : pat) =
  match q.p with
  | PWild | PBind _ -> 0
  | PLit _ | PVariant (_, None) -> 1
  | PVariant (_, Some r) -> pat_depth r + 1
  | PTuple qs -> 1 + List.fold_left (fun m r -> max m (pat_depth r)) 0 qs

let rec pat_lits (q : pat) =
  match q.p with
  | PLit l -> [ lit_key l ]
  | PVariant (_, Some r) -> pat_lits r
  | PTuple qs -> List.concat_map pat_lits qs
  | PWild | PBind _ | PVariant (_, None) -> []

(* 元组类型的各个分量 *)
let tuple_tys env ty =
  match ty with
  | TStruct m -> (
      match inst_of m with
      | Some g when Ast.is_tuple g ->
          Option.map (List.map (fun (f : field) -> f.fty)) (Hashtbl.find_opt env.structs m)
      | _ -> None)
  | _ -> None

(* 检查模式和值的类型相符 *)
let rec check_pat env ty (q : pat) =
  match q.p with
  | PWild | PBind _ -> ()
  | PLit l ->
      let t = lit_ty l in
      if t <> ty then err q.ppos "模式 %s 的类型是 %s，这里的值是 %s" (show_key (lit_key l)) (string_of_ty t) (string_of_ty ty)
  | PVariant (n, sub) -> (
      let w = vname n in
      match variants ty with
      | None -> err q.ppos "%s 不是枚举，不能用变体模式 %s" (string_of_ty ty) w
      | Some vs -> (
          if not (List.mem_assoc n vs) then err q.ppos "%s 没有变体 %s" (string_of_ty ty) w;
          match List.assoc n vs, sub with
          | None, Some _ -> err q.ppos "%s 没有载荷，不能写 %s(…)" w w
          | (None | Some TUnit), None -> ()
          | Some t, None -> err q.ppos "%s 带一个 %s 类型的载荷，要写 %s(…)" w (string_of_ty t) w
          | Some t, Some r -> check_pat env t r))
  | PTuple qs -> (
      match tuple_tys env ty with
      | Some ts when List.length ts = List.length qs -> List.iter2 (check_pat env) ts qs
      | _ -> err q.ppos "%s 不是 %d 个元素的元组，不能用元组模式" (string_of_ty ty) (List.length qs))

(* M24b 的代表值：Other 代表“不等于任何写出的字面量的值”，Any 代表深度用完以后的任意值。 *)
type aval = AAny | AOther | ALit of string | ATag of string * aval option | ATup of aval list

exception Too_many

let reps env keys depth ty =
  let count = ref 0 in
  let add l =
    count := !count + List.length l;
    if !count > 100000 then raise Too_many;
    l
  in
  let rec go d ty =
    if d = 0 then [ AAny ]
    else
      match ty with
      | TBool -> [ ALit "btrue"; ALit "bfalse" ]
      | TI64 | TChar | TStr ->
          let pre = match ty with TI64 -> 'i' | TChar -> 'c' | _ -> 's' in
          add (List.filter_map (fun k -> if k <> "" && k.[0] = pre then Some (ALit k) else None) keys @ [ AOther ])
      | _ -> (
          match variants ty, tuple_tys env ty with
          | Some vs, _ ->
              add
                (List.concat_map
                   (fun (n, pty) ->
                     match pty with
                     | None -> [ ATag (n, None) ]
                     | Some t -> List.map (fun r -> ATag (n, Some r)) (go (d - 1) t))
                   vs)
          | None, Some ts ->
              let rec prod = function
                | [] -> [ [] ]
                | t :: ts ->
                    let rs = go (d - 1) t and rest = prod ts in
                    List.concat_map (fun r -> List.map (fun l -> r :: l) rest) rs
              in
              add (List.map (fun l -> ATup l) (prod ts))
          | None, None -> [ AAny ])
  in
  go depth ty

let rec amatch (q : pat) r =
  match q.p, r with
  | (PWild | PBind _), _ -> true
  | PLit l, ALit k -> lit_key l = k
  | PVariant (n, None), ATag (m, _) -> n = m
  | PVariant (n, Some q'), ATag (m, Some r') -> n = m && amatch q' r'
  | PTuple qs, ATup rs -> List.length qs = List.length rs && List.for_all2 amatch qs rs
  | _ -> false

(* M24b 的 aselect：第一个匹配的分支 *)
let first_arm arms r =
  let rec go i = function [] -> None | (q, _) :: rest -> if amatch q r then Some i else go (i + 1) rest in
  go 0 arms

let rec show_aval = function
  | AAny | AOther -> "_"
  | ALit k -> show_key k
  | ATag (n, None) -> vname n
  | ATag (n, Some r) -> vname n ^ "(" ^ show_aval r ^ ")"
  | ATup rs -> "(" ^ String.concat ", " (List.map show_aval rs) ^ ")"

let unreachable pos = mkx pos (Call ("panic", [ mkx pos (Str "match 走到了不可能的分支（编译器的错误）") ]))

(* M24 的 test：按借用检查是否匹配（match let、==）；None 表示总是匹配 *)
let rec ptest env pos ty (mkp : unit -> expr) (q : pat) : expr option =
  match q.p with
  | PWild | PBind _ -> None
  | PLit l ->
      Some
        (mkx pos
           (match lit_ty l with
           | TStr -> Call ("str_eq", [ mkp (); copy_lit l ])
           | _ -> Binop (Eq, mkp (), copy_lit l)))
  | PVariant (n, sub) ->
      let vs = Option.get (variants ty) in
      let y = fresh_pvar () in
      let inner, binder =
        match List.assoc n vs, sub with
        | Some t, Some r when t <> TUnit -> (ptest env pos t (fun () -> mkx pos (Var y)) r, y)
        | Some t, _ when t <> TUnit -> (None, "_")
        | _ -> (None, "")
      in
      let yes = match inner with Some t -> t | None -> mkx pos (Bool true) in
      let others = if List.length vs > 1 then [ ("_", "", mkx pos (Bool false)) ] else [] in
      Some (mkx pos (Match (true, mkp (), (n, binder, yes) :: others)))
  | PTuple qs -> (
      let ts = Option.get (tuple_tys env ty) in
      let parts =
        List.concat
          (List.mapi
             (fun i (t, r) -> Option.to_list (ptest env pos t (fun () -> mkx pos (Field (mkp (), string_of_int i))) r))
             (List.combine ts qs))
      in
      match parts with
      | [] -> None
      | a :: rest -> Some (List.fold_left (fun acc b -> mkx pos (Binop (And, acc, b))) a rest))

(* M24 的 extract：只对选中的分支拆开、绑定变量（按移动，match let 时按借用）；标签不对的位置走不到 *)
let rec pextract env pos borrow ty (mkp : unit -> expr) (q : pat) (k : expr) : expr =
  let blockx stmts tail = mkx pos (Block { stmts; tail = Some tail }) in
  match q.p with
  | PWild | PLit _ -> k
  | PBind x -> blockx [ { s = Let (false, x, None, mkp ()); spos = pos } ] k
  | PVariant (n, sub) ->
      let vs = Option.get (variants ty) in
      let binder, inner =
        match List.assoc n vs, sub with
        | Some _, Some { p = PBind x; _ } -> (x, k)
        | Some t, Some r when t <> TUnit ->
            let y = fresh_pvar () in
            (y, pextract env pos borrow t (fun () -> mkx pos (Var y)) r k)
        | Some t, _ when t <> TUnit -> ("_", k)
        | _ -> ("", k)
      in
      let others = if List.length vs > 1 then [ ("_", "", unreachable pos) ] else [] in
      mkx pos (Match (borrow, mkp (), (n, binder, inner) :: others))
  | PTuple qs ->
      let ts = Option.get (tuple_tys env ty) in
      let items = List.mapi (fun i (t, r) -> (i, t, r)) (List.combine ts qs) in
      if borrow then
        List.fold_right
          (fun (i, t, r) acc -> pextract env pos true t (fun () -> mkx pos (Field (mkp (), string_of_int i))) r acc)
          items k
      else
        let names = List.map (fun (_, _, (r : pat)) -> match r.p with PBind x -> x | _ -> fresh_pvar ()) items in
        let body =
          List.fold_right2
            (fun (_, t, (r : pat)) nm acc ->
              match r.p with PBind _ -> acc | _ -> pextract env pos false t (fun () -> mkx pos (Var nm)) r acc)
            items names k
        in
        blockx
          [ { s = Destruct (Ast.tuple_name (List.length qs), List.mapi (fun i x -> (string_of_int i, x)) names, mkp ()); spos = pos } ]
          body

let rec simple_place (x : expr) = match x.e with Var _ -> true | Field (a, _) | Deref a -> simple_place a | _ -> false

let rec copy_place (x : expr) =
  mkx x.pos (match x.e with Field (a, f) -> Field (copy_place a, f) | Deref a -> Deref (copy_place a) | d -> d)

let rec expr env (x : expr) : t =
  let t = expr1 env x in
  Hashtbl.replace types x.id t;
  t

(* 按期望类型检查。 *)
and check env want (x : expr) : t =
  let t = check1 env want x in
  Hashtbl.replace types x.id t;
  t

and check1 env want (x : expr) : t =
  match x.e with
  | Call (f, (r :: _ as args)) when is_method f -> check1 env want { x with e = Call (method_name env x f r, args) }
  | Variant (v, a) -> (
      let w = vname v in
      match variant want v, a with
      | _ when variants want = None -> err x.pos "这里需要 %s，%s 是枚举的变体" (string_of_ty want) w
      | None, _ -> err x.pos "%s 没有变体 %s" (string_of_ty want) w
      | Some (_, Some t), Some a ->
          ignore (check env t a);
          Ty want
      | Some (_, None), None -> Ty want
      | Some (_, Some TUnit), None -> Ty want   (* 载荷是 () 时可以省略：Ok 就是 Ok(()) *)
      | Some (_, Some t), None -> err x.pos "%s 带一个 %s 类型的载荷，要写 %s(…)" w (string_of_ty t) w
      | Some (_, None), Some _ -> err x.pos "%s 没有载荷，不能写 %s(…)" w w)
  | If (c, a, Some b) ->
      expect c.pos TBool (expr env c);
      expect x.pos want (block env ~want a);
      expect x.pos want (block env ~want b);
      Ty want
  | Block b -> block env ~want b
  | Match (b, s, arms) -> match_ env (Some want) x.pos b s arms
  | PMatch (b, s, arms) -> check env want (pmatch_ env x b s arms)
  | Arena (r, b) -> arena env (Some want) x.pos r b
  | Call ("vec_new", []) -> (
      match want with
      | TVec t ->
          vec_inst x "new" t;
          Ty want
      | _ -> err x.pos "这里需要 %s，vec_new() 是 Vec" (string_of_ty want))
  | Call (f, args) when not (List.mem f special) ->
      let t = call env (Some want) x f args in
      expect x.pos want t;
      t
  | StructLit (n, inits) ->
      let t = struct_lit env (Some want) x n inits in
      expect x.pos want t;
      t
  | Lambda (ys, body) when (match want with TOFn _ -> true | _ -> false) -> pack env x ys body want
  | _ ->
      let t = expr1 env x in
      expect x.pos want t;
      t

and expr1 env (x : expr) : t =
  let pos = x.pos in
  match x.e with
  | Call (f, (r :: _ as args)) when is_method f -> expr1 env { x with e = Call (method_name env x f r, args) }
  | Int _ -> Ty TI64
  | Float _ -> Ty TF64
  | Char _ -> Ty TChar
  | Bool _ -> Ty TBool
  | Unit -> Ty TUnit
  | Str _ -> Ty TStr
  | Var v -> (
      note env.vars v;
      match List.assoc_opt v env.vars with
      | Some (TFn _, _) ->
          err pos "%s 是函数参数，只能调用它（%s(…)），或者作为实参传给 fn(…) 类型的参数" v v
      | Some (t, _) -> Ty t
      | None -> err pos "未定义的变量 %s" v)
  | Lambda _ ->
      err pos "无法确定闭包的类型：写在调用的实参里传给 fn(…) 参数，或者写在知道类型的地方（例如 var f: Fn(I64) -> I64 = |x| …）"
  | Binop (op, a, b) -> (
      let ta = expr env a in
      let tb = expr env b in
      match op with
      (* 算术和大小比较：两边都是 I64，或者都是 F64（C47，不做隐式转换，用 to_f64 / to_i64）；% 只用于 I64 *)
      | Rem -> expect a.pos TI64 ta; expect b.pos TI64 tb; Ty TI64
      | Add | Sub | Mul | Div ->
          let t = num ta in
          expect a.pos t ta; expect b.pos t tb; Ty t
      | Lt | Le | Gt | Ge ->
          let t = match ta with Ty TChar -> TChar | _ -> num ta in   (* 字符按码点比较（C48） *)
          expect a.pos t ta; expect b.pos t tb; Ty TBool
      | Eq | Ne -> (
          match ta, tb with
          | Ty TStr, _ | _, Ty TStr -> err pos "字符串用 str_eq(a, b) 或 a.eq(b) 比较"
          | Ty TBytes, _ | _, Ty TBytes -> err pos "字节串用 a.eq(b) 比较"
          | Ty (TStruct _ | TUnit | TBox _ | TVec _ | TResult _ | TRef _ | TArena _ | TApp _ | TParam _ | TFn _ | TOFn _), _
          | _, Ty (TStruct _ | TUnit | TBox _ | TVec _ | TResult _ | TRef _ | TArena _ | TApp _ | TParam _ | TFn _ | TOFn _) ->
              err pos "只能比较 I64、F64 和 Bool"
          | _ -> ignore (join pos "比较" ta tb); Ty TBool)
      | And | Or -> expect a.pos TBool ta; expect b.pos TBool tb; Ty TBool)
  | Unop (Neg, a) ->
      let ta = expr env a in
      let t = num ta in
      expect a.pos t ta; Ty t
  | Unop (Not, a) -> expect a.pos TBool (expr env a); Ty TBool
  | Call ("panic", [ a ]) ->
      (* panic(msg)（C38，Lean M19）：说明按借用读；不会返回，类型是 Never（和 return 一样） *)
      expect a.pos TStr (expr env a);
      Never
  | Call ("panic", _) -> err pos "panic 需要 1 个参数（说明文字）"
  | Call ("os_exit", [ a ]) ->
      (* os_exit(code)（C58）：和 panic 一样结束进程、不析构任何值（Lean M19），退出码是 code 的低 8 位 *)
      expect a.pos TI64 (expr env a);
      Never
  | Call ("os_exit", _) -> err pos "os_exit 需要 1 个参数（退出码）"
  | Call ("box", [ a ]) -> (
      match expr env a with
      | Ty t when is_linear t -> err pos "Box 里不能放 linear 值（还不能从 Box 里移出）"
      | Ty t -> Ty (TBox t)
      | Never -> Never)
  | Call ("consume", [ a ]) -> (
      match expr env a with
      | Ty (TStruct n) when Hashtbl.mem linear_tbl n ->
          (* 只能在定义这个 linear 类型的文件里消费（C34）：别的模块只能交给它的 pub 函数 *)
          let base = match String.index_opt n '<' with Some i -> String.sub n 0 i | None -> n in
          (match Hashtbl.find_opt Ast.type_file base with
          | Some fi when fi <> Ast.file_of pos ->
              let m = match String.rindex_opt base '.' with Some i -> String.sub base 0 i | None -> "" in
              err pos "%s 定义在模块 %s 里，只能在那里 consume（可以让模块 %s 提供一个 pub 的函数来消费它）" n m m
          | _ -> ());
          Ty TUnit
      | Never -> Never
      | Ty t -> err pos "consume 只能用于 linear 结构体，%s 不是" (string_of_ty t))
  | Call ("unbox", [ a ]) -> (
      (* 取出 Box 里的值：Box 按 sink 交出，堆内存释放（Lean M11 的 unbox） *)
      match expr env a with
      | Ty (TBox t) -> Ty t
      | Never -> Never
      | Ty t -> err pos "unbox 的参数必须是 Box，%s 不是" (string_of_ty t))
  | Call ((("box" | "unbox" | "consume") as f), _) -> err pos "%s 需要 1 个参数" f
  | Call ("alloc", [ a; v ]) -> (
      match expr env a with
      | Ty (TArena r) -> (
          match expr env v with
          | Never -> Never
          | Ty TUnit -> err v.pos "区域里不能放 ()"
          | Ty t when needs_drop t ->
              err pos "区域里不能放需要析构或 linear 的值 %s：区域结束时不运行析构（D4）" (string_of_ty t)
          | Ty t when region_names t <> [] -> err v.pos "还不支持在区域里存放引用"
          | Ty t -> Ty (TRef (t, r)))
      | Never -> Never
      | Ty t -> err a.pos "alloc 的第一个参数必须是区域，%s 不是" (string_of_ty t))
  | Call ("alloc", _) -> err pos "alloc 需要 2 个参数：区域和值"
  | Call ("vec_new", []) ->
      err pos "无法确定 vec_new() 的元素类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方"
  | Call (("vec_push" | "vec_pop" | "vec_len" | "vec_get" | "vec_swap") as f, v :: rest) -> (
      match expr env v, f, rest with
      | Never, _, _ -> Never
      | Ty (TVec t), "vec_push", [ e ] ->
          inout_place env v;
          ignore (check env t e);
          vec_inst x "push" t;
          Ty TUnit
      | Ty (TVec t), "vec_pop", [] ->
          inout_place env v;
          vec_inst x "pop" t;
          Ty (instantiate "Option" [ t ])
      | Ty (TVec t), "vec_len", [] ->
          vec_inst x "len" t;
          Ty TI64
      | Ty (TVec t), "vec_swap", [ i; j ] ->
          (* 交换第 i、j 个元素：只调换顺序，拥有关系不变（Lean M20），越界时 panic *)
          inout_place env v;
          expect i.pos TI64 (expr env i);
          expect j.pos TI64 (expr env j);
          vec_inst x "swap" t;
          Ty TUnit
      | Ty (TVec t), "vec_get", [ i ] ->
          (* 第 i 个元素这个位置：可复制的值复制，需要析构的值 let 绑定成投影（锁住 v），越界时 panic *)
          expect i.pos TI64 (expr env i);
          Ty t
      | Ty (TVec _), _, _ ->
          err pos "%s 需要 %d 个参数" f (match f with "vec_push" | "vec_get" -> 2 | "vec_swap" -> 3 | _ -> 1)
      | Ty t, _, _ -> err v.pos "%s 的第一个参数必须是 Vec，%s 不是" f (string_of_ty t))
  | Call (("vec_new" | "vec_push" | "vec_pop" | "vec_len" | "vec_get" | "vec_swap") as f, _) -> err pos "%s 的参数个数不对" f
  | Deref a -> (
      match expr env a with
      | Ty (TBox t | TRef (t, _)) -> Ty t
      | Never -> Never
      | Ty t -> err pos "%s 不是 Box 或引用，不能解引用" (string_of_ty t))
  | Call (f, args) -> call env None x f args
  | StructLit (n, inits) -> struct_lit env None x n inits
  | Field (a, f) -> (
      match expr env a with
      | Ty (TStruct n) when Hashtbl.mem enum_tbl n -> err pos "%s 是枚举，不能取字段 %s（用 match）" n f
      | Ty (TStruct n) -> (
          let fs = Hashtbl.find env.structs n in
          match List.find_opt (fun (g : field) -> g.fname = f) fs with
          | Some g -> Ty g.fty
          | None -> err pos "%s 没有字段 %s" n f)
      | Never -> Never
      | Ty t -> err pos "%s 不是结构体，不能取字段 %s" (string_of_ty t) f)
  | If (c, a, None) ->
      expect c.pos TBool (expr env c);
      expect pos TUnit (block env a);
      Ty TUnit
  | If (c, a, Some b) ->
      expect c.pos TBool (expr env c);
      join pos "if " (block env a) (block env b)
  | Block b -> block env b
  | Variant (v, _) ->
      err pos "无法确定 %s 的类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方" (vname v)
  | Try a -> (
      match expr env a with
      | Never -> Never
      | Ty (TResult (t, e)) -> (
          match env.ret with
          | TResult (_, e') when e = e' -> Ty t
          | TResult (_, e') ->
              err pos "? 传出的错误类型是 %s，但函数返回的错误类型是 %s" (string_of_ty e)
                (string_of_ty e')
          | r -> err pos "? 只能用在返回 Result 的函数里（这个函数返回 %s）" (string_of_ty r))
      | Ty t -> err pos "%s 不是 Result，不能用 ?" (string_of_ty t))
  | Match (b, s, arms) -> match_ env None pos b s arms
  | PMatch (b, s, arms) -> expr env (pmatch_ env x b s arms)
  | Arena (r, b) -> arena env None pos r b

(* 函数调用。want：期望的返回类型（知道时用来推断泛型函数的类型实参）。 *)
(* 方法调用 r.f(…) 的函数名：接收者是 Vec 时 push / pop / len / get 是内建的 vec_…，
   是 Str 时 len / eq / concat / push / clone / to_i64 / byte / slice 是内建的 str_…，否则是函数 f。 *)
and method_name env (x : expr) f (r : expr) =
  let m = String.sub f 1 (String.length f - 1) in
  let g =
    match expr env r, m with
    | Ty (TVec _), ("push" | "pop" | "len" | "get" | "swap") -> "vec_" ^ m
    | Ty TStr, m when List.mem m str_methods -> "str_" ^ m
    | Ty TBytes, m when List.mem m bytes_methods -> "bytes_" ^ m
    | Ty TChar, "code" -> "char_code"
    (* x.to_str()，也是字符串插值 "{x}" 用的（C49） *)
    | Ty TI64, "to_str" -> "str_from_i64"
    | Ty TF64, "to_str" -> "str_from_f64"
    | Ty TChar, "to_str" -> "str_from_char"
    | Ty TBool, "to_str" -> "str_from_bool"
    | Ty TStr, "to_str" -> "str_clone"
    | Ty (TOFn _ as t), "call" -> fcall_name env.fns t   (* f.call(x)：字段、Vec 元素里的闭包也能这样调用（C59） *)
    | _ -> m
  in
  Hashtbl.replace method_res x.id g;
  g

and call env want (x : expr) f args : t =
  match List.assoc_opt f env.vars with
  | Some (TFn (pts, r), _) ->
      (* 调用 fn(…) 参数：实参按参数约定检查；在特化里换成调用绑定的闭包提升出的函数，前面接上捕获（C40） *)
      note env.vars f;
      if List.length pts <> List.length args then
        err x.pos "%s 需要 %d 个参数，实际给了 %d 个" f (List.length pts) (List.length args);
      List.iter2
        (fun (m, p) (a : expr) ->
          ignore (check env p a);
          if m = MInout then inout_place env a)
        pts args;
      (if !concrete then
         match List.assoc_opt f env.clos with
         | Some (c, caps) ->
             Hashtbl.replace rewrites x.id (c.cname, List.map (fun (n, t) -> var_expr x.pos n t) caps @ args)
         | None -> ());
      Ty r
  | Some ((TOFn _ as t), _) ->
      (* 调用 Fn(…) 类型的变量 f（C59）：就是 call#…(f, …)，闭包按 inout（M26 的 env_disjoint） *)
      let g = fcall_name env.fns t in
      let args = var_expr x.pos f t :: args in
      Hashtbl.replace rewrites x.id (g, args);
      call_named env want x g args
  | _ -> call_named env want x f args

(* 需要 Fn(pts) -> r 的地方的闭包（C59，Lean M26）：照 C40 检查、提升，捕获按值存进环境。 *)
and pack env (x : expr) ys body want : t =
  match want with
  | TOFn (pts, r) ->
      let c, _ = lambda env x ys body pts (Some r) in
      List.iter
        (fun (_, v, t) ->
          if region_names t <> [] then
            err x.pos "Fn(…) 闭包暂不能用含引用或区域的变量 %s：闭包可以存起来，引用可能活不过它的区域" v;
          if is_linear t then err x.pos "Fn(…) 闭包暂不能用 linear 值 %s" v)
        c.caps;
      let pk = "Fn#" ^ c.cname in
      if not (Hashtbl.mem sigs pk) then Hashtbl.replace sigs pk (List.map (fun (_, _, t) -> (MSink, t)) c.caps, want);
      if !concrete then Hashtbl.replace packs pk (c, want);
      Hashtbl.replace rewrites x.id (pk, List.map (fun (_, v, t) -> var_expr x.pos v t) c.caps);
      Ty want
  | _ -> assert false

(* 闭包 |ys| body 作 fn(pts) -> r 类型的参数的实参（C40）：参数的类型取 pts，在原地检查函数体，找出捕获。
   r 为 None 时只推出函数体的类型（泛型函数的类型实参还没确定时）。会生成代码时把闭包提升成普通函数：
   捕获在前、闭包的参数在后；名字按位置和类型确定，同一个闭包只提升一次。 *)
and lambda env (a : expr) ys body pts r : closure * t =
  let pos = a.pos in
  if !lambda_frame <> None then err pos "闭包里暂不能再写闭包";
  if List.length ys <> List.length pts then
    err pos "闭包有 %d 个参数，这里需要 %d 个" (List.length ys) (List.length pts);
  List.iteri
    (fun i y -> if List.mem y (List.filteri (fun j _ -> j < i) ys) then err pos "闭包的参数 %s 重复" y)
    ys;
  let fr = { outer = List.length env.vars; used = ref []; written = Hashtbl.create 8 } in
  let vars = List.rev (List.map2 (fun y (m, t) -> (y, (t, m <> MLet))) ys pts) @ env.vars in
  let env' = { env with vars; guards = []; ret = Option.value r ~default:TUnit } in
  let depth = !loop_depth in
  lambda_frame := Some fr;
  loop_depth := 0;
  let t =
    Fun.protect
      ~finally:(fun () ->
        lambda_frame := None;
        loop_depth := depth)
      (fun () -> match r with Some r -> check env' r body | None -> expr env' body)
  in
  let caps0 =
    List.map
      (fun v ->
        let t, _ = List.assoc v env.vars in
        (match t with
        | TFn _ -> err pos "闭包里暂不能用函数参数 %s（可以把它作为实参直接传下去）" v
        | _ -> ());
        ((if Hashtbl.mem fr.written v then MInout else MLet), v, t))
      !(fr.used)
  in
  (* 捕获的类型里的区域换成区域参数 'c00、'c01…：按这里的嵌套顺序编号（外层在前），
     先后关系只取这里真实成立的（M22 的 capture_mono；调用时这些区域都活着是 capture_below） *)
  let outer_first = List.rev_map fst !regions in
  let idx r = Option.value (List.find_index (( = ) r) outer_first) ~default:max_int in
  let rs =
    List.sort_uniq compare (List.concat_map (fun (_, _, t) -> region_names t) caps0)
    |> List.stable_sort (fun a b -> compare (idx a) (idx b))
  in
  if List.length rs > 100 then err pos "闭包捕获的区域太多（最多 100 个）";
  let s = List.mapi (fun k r -> (r, Printf.sprintf "'c%02d" k)) rs in
  let rord = List.map (fun (r, c) -> (c, List.filter_map (fun (r', c') -> if r' <> r && outlives r' r then Some c' else None) s)) s in
  let caps = List.map (fun (m, v, t) -> (m, v, rename s t)) caps0 in
  let file, line = Ast.locate (if Array.length !Ast.files > 0 then !Ast.files.(0) else "") pos in
  let tys = List.map (fun (_, _, t) -> string_of_ty t) caps @ [ string_of_ty (TFn (pts, Option.value r ~default:TUnit)) ] in
  let cname = Printf.sprintf "闭包<%s:%d:%d, %s>" (Filename.basename file) line pos.col (String.concat ", " tys) in
  (match r with
  | Some r when !concrete ->
      if not (Hashtbl.mem requested cname) then begin
        Hashtbl.replace requested cname ();
        let params = caps @ List.map2 (fun y (m, t) -> (m, y, t)) ys pts in
        Hashtbl.replace sigs cname (List.map (fun (m, _, t) -> (m, t)) params, r);
        let b = match body.e with Block b -> b | _ -> { stmts = []; tail = Some body } in
        let body = map_body ~fresh:true (fun _ t -> rename s t) b in
        Queue.add
          (Fn { name = cname; tparams = []; bounds = []; params; ret = r; body; ipos = pos }, [], [], rord)
          pending_items
      end
  | _ -> if not !concrete then Hashtbl.replace skip_lower !cur_fn ());
  ({ cname; caps; rord }, t)

(* fn(…) 类型的参数 p 的实参：闭包，或者当前函数的同类型 fn(…) 参数（原样传下去）。
   返回绑定的闭包和它的捕获在调用处的实参（闭包写在这里时是外层变量本身，传下去的是这个特化里的捕获参数）。 *)
and fn_arg env f p pts pr (a : expr) : closure * expr list =
  let bad () = err a.pos "%s 的这个参数是 %s：要写闭包 |…| …，或者传入同类型的函数参数" f (string_of_ty p) in
  match a.e with
  | Lambda (ys, body) ->
      let c, _ = lambda env a ys body pts (Some pr) in
      (* 闭包写在这里：捕获的实参是外层变量本身（真实的区域；类型检查不再比较，由 M22 保证、Lean 复核） *)
      (c, List.map (fun v -> var_expr a.pos v (fst (List.assoc v env.vars))) (List.map (fun (_, v, _) -> v) c.caps))
  | Var g -> (
      note env.vars g;
      match List.assoc_opt g env.vars with
      | Some ((TFn _ as t), _) -> (
          if t <> p then err a.pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty p) (string_of_ty t);
          match List.assoc_opt g env.clos with
          | Some (c, caps) -> (c, List.map (fun (n, t) -> var_expr a.pos n t) caps)
          | None -> ({ cname = ""; caps = []; rord = [] }, []))
      | Some ((TOFn (qs, u) as t), _) ->
          (* Fn(…) 变量传给 fn(…) 参数（C59）：当作闭包 |a…| g(a…)，它按 inout 捕获 g *)
          if TFn (qs, u) <> p then err a.pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty p) (string_of_ty t);
          let ys = List.mapi (fun i _ -> Printf.sprintf "#a%d" i) pts in
          let body = mkx a.pos (Call (g, List.map (fun y -> mkx a.pos (Var y)) ys)) in
          fn_arg env f p pts pr { a with e = Lambda (ys, body) }
      | _ -> bad ())
  | _ -> bad ()

(* 调用带 fn(…) 参数的函数 f：换成调用 f 对这些闭包的特化（第一次用到时生成，名字是 f[闭包, …]），
   去掉 fn(…) 参数，后面接上每个闭包的捕获（M21：捕获当作额外的实参，排他性在这里一起检查）。 *)
and specialize (x : expr) f args ps cls =
  let ts = Hashtbl.find_opt call_inst x.id in
  let base = match ts with Some ts -> inst_name f ts | None -> f in
  let name = base ^ "[" ^ String.concat ", " (List.map (fun ((c : closure), _) -> c.cname) cls) ^ "]" in
  let keep = List.filteri (fun i _ -> not (is_fn (snd (List.nth ps i)))) args in
  Hashtbl.replace rewrites x.id (name, keep @ List.concat_map snd cls);
  Hashtbl.remove call_inst x.id;
  if not (Hashtbl.mem requested name) then begin
    Hashtbl.replace requested name ();
    match (match ts with Some ts -> instance (f, ts) | None -> Hashtbl.find fn_items f) with
    | Fn g ->
        (* 泛型实例已经复制过；非泛型函数在这里复制（表达式换新的 id） *)
        let body = if ts = None then map_body ~fresh:true (fun _ t -> t) g.body else g.body in
        let fparams = List.filter (fun (_, _, t) -> is_fn t) g.params in
        let binding = List.map2 (fun (_, y, t) (c, _) -> (y, t, c)) fparams cls in
        (* 每个闭包的区域参数加上 @y 区分（两个闭包都有 'c00 时是不同的区域，彼此之间没有顺序） *)
        let ry (c : closure) y = List.map (fun (r, _) -> (r, r ^ "@" ^ y)) c.rord in
        let capsof (y, _, (c : closure)) = List.map (fun (m, v, t) -> (m, v ^ "@" ^ y, rename (ry c y) t)) c.caps in
        let caps = List.concat_map capsof binding in
        let params = List.filter (fun (_, _, t) -> not (is_fn t)) g.params @ caps in
        Hashtbl.replace sigs name (List.map (fun (m, _, t) -> (m, t)) params, g.ret);
        let clos = List.map (fun ((y, _, c) as b) -> (y, (c, List.map (fun (_, v, t) -> (v, t)) (capsof b)))) binding in
        let fvars = List.map (fun (y, t, _) -> (y, t)) binding in
        let rord =
          List.concat_map
            (fun (y, _, (c : closure)) -> List.map (fun (r, os) -> (r ^ "@" ^ y, List.map (fun o -> o ^ "@" ^ y) os)) c.rord)
            binding
        in
        Queue.add (Fn { g with name; params; body; tparams = []; bounds = [] }, clos, fvars, rord) pending_items
    | _ -> assert false
  end

and call_named env want (x : expr) f args : t =
  let pos = x.pos in
  let ps, r =
    match Hashtbl.find_opt env.fns f with
    | Some s -> s
    | None -> (
        match List.assoc_opt f builtins with
        | Some (ps, r) ->
            (* 内建函数签名里的 Option<…> 在这里换成实例（C32） *)
            (List.map (fun (m, t) -> (m, resolve [] pos t)) ps, resolve [] pos r)
        | None -> err pos "未定义的函数 %s" f)
  in
  if List.length ps <> List.length args then
    err pos "%s 需要 %d 个参数，实际给了 %d 个" f (List.length ps) (List.length args);
  let has_fn = List.exists (fun (_, p) -> is_fn p) ps in
  (* 泛型函数：从期望的返回类型和实参类型推出类型实参，检查约束（普通的 T 不能是 linear 类型），
     然后按具体类型检查实参；类型实参都是具体类型时登记这个实例（Lean M9 的 Respects） *)
  let ps, r =
    match Hashtbl.find_opt gparams f with
    | None -> (ps, r)
    | Some tps ->
        let s = ref [] in
        Option.iter (fun w -> unify s r w) want;
        (* .A(e) / .B 自己推不出类型，留到类型实参确定之后按期望类型检查；
           fn(…) 参数：传下去的函数参数按它的类型推，闭包等别的实参推完之后再推（C40） *)
        List.iter2
          (fun (_, p) (a : expr) ->
            if List.exists (fun n -> not (List.mem_assoc n !s)) (params_of p) then
              match p, a.e with
              | TFn _, Var g -> (
                  match List.assoc_opt g env.vars with Some (t, _) -> unify s p t | None -> ())
              | TFn _, _ | _, Variant _ -> ()
              | _ -> ( match expr env a with Ty t -> unify s p t | Never -> ()))
          ps args;
        (* 闭包的参数类型由别的实参确定，函数体的类型确定返回类型里的类型参数（例如 map 的 U） *)
        List.iter2
          (fun (_, p) (a : expr) ->
            match p, a.e with
            | TFn (pts, pr), Lambda (ys, body) when List.exists (fun n -> not (List.mem_assoc n !s)) (params_of pr) -> (
                List.iter
                  (fun (_, t) ->
                    List.iter
                      (fun n ->
                        if not (List.mem_assoc n !s) then
                          err a.pos "无法推断闭包参数的类型（%s 的类型参数 %s 要先从别的实参确定）" f n)
                      (params_of t))
                  pts;
                let pts = List.map (fun (m, t) -> (m, subst !s t)) pts in
                match snd (lambda env a ys body pts None) with
                | Ty t -> unify s pr t
                | Never -> ())
            | _ -> ())
          ps args;
        let ts =
          List.map
            (fun (n, lin) ->
              match List.assoc_opt n !s with
              | None -> err pos "无法推断 %s 的类型参数 %s（可以给返回值或实参写类型标注）" f n
              | Some t ->
                  type_arg pos t;
                  if is_linear t && not lin then
                    err pos "%s 的类型参数 %s 没有标 linear，不能是 linear 类型 %s（要写 %s: linear）" f n
                      (string_of_ty t) n;
                  t)
            tps
        in
        (* trait 约束：类型参数要在当前函数里写了同样的约束，具体类型要有 impl（Lean M10 的 RespectsT；
           泛型 impl 按 M25 递归地查） *)
        let named = List.combine (List.map fst tps) ts in
        List.iter
          (fun (n, tr) ->
            let t = List.assoc n named in
            if not (has_impl env.tbounds tr t) then
              let tr0, t0 = why_not env.tbounds tr t in
              match Hashtbl.mem methods f, t with
              | _ when t0 <> t -> (
                  (* 泛型 impl 的约束不满足：报最里面那个（C56） *)
                  match t0 with
                  | TParam (m, _) ->
                      err pos "%s 没有写约束 %s，而 %s 的 %s 要用到它（要写 %s: %s）" m tr0 (string_of_ty t) tr m tr0
                  | _ -> err pos "%s 没有实现 %s，而 %s 的 %s 要用到它" (string_of_ty t0) tr0 (string_of_ty t) tr)
              | true, _ when tr = "ToStr" && params_of t = [] ->
                  (* 插值 "{p}"、p.to_str() 落到这里：p 的类型不是内建的那几种，也没有写 to_str（C49） *)
                  err pos "%s 没有实现 ToStr，不能在字符串里插值或调用 to_str（先写 fn to_str(x: %s) -> Str）"
                    (string_of_ty t) (string_of_ty t)
              | true, TParam (m, _) -> err pos "%s 没有写约束 %s，不能调用 %s（要写 %s: %s）" m tr f m tr
              | true, _ -> err pos "%s 没有实现 %s，不能调用 %s" (string_of_ty t) tr f
              | false, TParam (m, _) -> err pos "%s 要求 %s: %s，但 %s 没有写约束 %s" f n tr m tr
              | false, _ -> err pos "%s 没有实现 %s（%s 要求 %s: %s）" (string_of_ty t) tr f n tr)
          (Option.value (Hashtbl.find_opt gbounds f) ~default:[]);
        (* 含引用的类型实参（C19）：类型参数要出现在参数类型里，实例的区域才能从实参得到；
           区域名换成按出现顺序编号的 '0、'1…，不同调用处的同一种实例只生成一份，也不会和函数体里 arena 的名字冲突 *)
        List.iter2
          (fun (n, _) t ->
            if region_names t <> [] && not (List.exists (fun (_, p) -> List.mem n (params_of p)) ps) then
              err pos "%s 的类型参数 %s 只出现在返回类型里，不能是含引用的类型 %s（区域要从实参得到）" f n
                (string_of_ty t))
          tps ts;
        let rs =
          List.fold_left (fun acc r -> if List.mem r acc then acc else acc @ [ r ]) [] (List.concat_map region_names ts)
        in
        let ts = List.map (rename (List.mapi (fun i r -> (r, "'" ^ string_of_int i)) rs)) ts in
        Hashtbl.replace call_inst x.id ts;
        (* 方法调用直接用 impl 里的函数；impl 没写、用默认方法的，或者 impl 是泛型的，生成实例（C56）；
           带 fn(…) 参数的函数生成的是特化 *)
        if List.for_all (fun t -> params_of t = []) ts
           && (not has_fn)
           && not (Hashtbl.mem requested (inst_name f ts))
        then begin
          Hashtbl.replace requested (inst_name f ts) ();
          if Hashtbl.mem methods f then method_inst f (List.hd ts) else Queue.add (f, ts) pending
        end;
        let s = List.combine (List.map fst tps) ts in
        (List.map (fun (m, p) -> (m, subst s p)) ps, subst s r)
  in
  (* 区域参数按实参实例化（M5（四））：Arena 参数取实参的区域，
     参数类型里的其他区域名取实参类型里对应位置的区域 *)
  let s = ref [] in
  let rec bind p t =
    match p, t with
    | (TRef (p, x), TRef (t, y)) ->
        if not (List.mem_assoc x !s) then s := (x, y) :: !s;
        bind p t
    | TArena x, TArena y -> if not (List.mem_assoc x !s) then s := (x, y) :: !s
    | TBox p, TBox t | TVec p, TVec t -> bind p t
    | TResult (a, b), TResult (c, d) -> bind a c; bind b d
    | TStruct a, TStruct b -> (
        match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
        | Some (g, ps), Some (h, ts) when g = h -> List.iter2 bind ps ts
        | _ -> ())
    | _ -> ()
  in
  List.iter2
    (fun (_, p) (a : expr) ->
      if region_names p <> [] then match expr env a with Ty t -> bind p t | Never -> ())
    ps args;
  let s = !s in
  let cls = ref [] in
  List.iter2
    (fun (m, p) (a : expr) ->
      let p = rename s p in
      match p with
      | TFn (pts, pr) -> cls := !cls @ [ fn_arg env f p pts pr a ]
      | _ ->
          ignore (check env p a);
          if m = MInout then begin
            inout_place env a;
            (* inout 实参里的区域不能放宽（不变）：被调用的函数可能往里面写一个活得更短的引用 *)
            match Hashtbl.find_opt types a.id with
            | Some (Ty t) when t <> p ->
                err a.pos "inout 实参的类型要和参数类型完全相同：需要 %s，实际是 %s（区域不能放宽）"
                  (string_of_ty p) (string_of_ty t)
            | _ -> ()
          end)
    ps args;
  if has_fn then if !concrete then specialize x f args ps !cls else Hashtbl.replace skip_lower !cur_fn ();
  Ty (rename s r)

(* 结构体字面量。泛型结构体的类型实参取期望的类型，不知道时从字段的值推出来。 *)
and struct_lit env want (x : expr) n inits : t =
  let pos = x.pos in
  let m =
    match Hashtbl.find_opt gtypes n, want with
    | None, _ -> n
    | Some _, Some (TStruct w) when inst_of w = Some n -> w
    | Some (ps, _), _ ->
        let abs = instantiate n (List.map (fun p -> TParam (p, false)) ps) in
        let fs = match abs with TStruct a -> Hashtbl.find_opt env.structs a | _ -> None in
        let fs = match fs with Some fs -> fs | None -> err pos "未知结构体 %s" n in
        let s = ref [] in
        List.iter
          (fun (f : field) ->
            match List.assoc_opt f.fname inits with
            | Some e -> (match expr env e with Ty t -> unify s f.fty t | Never -> ())
            | None -> ())
          fs;
        let ts =
          List.map
            (fun p ->
              match List.assoc_opt p !s with
              | None -> err pos "无法推断 %s 的类型参数 %s（可以写类型标注，例如 let x: %s<…> = …）" n p n
              | Some t ->
                  data_arg pos t;
                  t)
            ps
        in
        (match instantiate n ts with TStruct m -> m | _ -> n)
  in
  let fs =
    match Hashtbl.find_opt env.structs m with
    | Some fs -> fs
    | None -> err pos "未知结构体 %s" n
  in
  List.iter
    (fun (f : field) ->
      match List.filter (fun (g, _) -> g = f.fname) inits with
      | [ (_, (e : expr)) ] -> ignore (check env f.fty e)
      | [] -> err pos "%s 缺少字段 %s" n f.fname
      | _ -> err pos "字段 %s 重复" f.fname)
    fs;
  List.iter
    (fun (g, _) ->
      if not (List.exists (fun (f : field) -> f.fname = g) fs) then
        err pos "%s 没有字段 %s" n g)
    inits;
  Ty (TStruct m)

(* arena |r| { … }：r 比所有外层区域都活得短。块的值的类型不能含 r（R3）。 *)
and arena env want pos r (b : block) : t =
  if List.mem_assoc r !regions then err pos "区域 %s 已经存在（内外层区域不能同名）" r;
  let saved = !regions in
  regions := (r, List.map fst saved) :: saved;
  let env' = { env with vars = (r, (TArena r, false)) :: env.vars } in
  let t = match want with Some w -> block env' ~want:w b | None -> block env' b in
  regions := saved;
  (match t with
  | Ty ty when mentions r ty && not !lax_regions ->
      let at = match b.tail with Some e -> e.pos | None -> pos in
      err at "块的值的类型 %s 含有区域 %s，不能带出这个区域（R3）" (string_of_ty ty) r
  | _ -> ());
  t

(* 枚举（含 Result）的 match（Lean M8）：必须穷尽，每个变体恰好一个分支；带载荷的变体绑定载荷。
   borrow：match let s { … }，s 必须是位置（Lean M8（二））。 *)
(* 带嵌套模式的 match（C55）：检查模式的类型；覆盖性（Lean M24b：每个代表值都有分支匹配，每个分支都是某个代表值
   第一个匹配的）；然后展开（Lean M24）：
     { [let #pN = s]  if test₀ { extract₀ } else if test₁ { extract₁ } … else { extract_last } }
   test 按借用（match let、==），extract 只对选中的分支按移动拆开（match let 时按借用）。返回展开后的表达式，
   调用的地方按它检查类型；map_body 再把 PMatch 换成它。 *)
and pmatch_ env (x : expr) borrow (s : expr) arms : expr =
  let pos = x.pos in
  let ty = match expr env s with Ty t -> t | Never -> err s.pos "不能 match 一个不会返回的值" in
  if borrow && root s = None then err s.pos "match let 后面必须是变量、字段或解引用（要借用的位置）";
  (* 借来的参数里的位置不能移走：和 match let 一样按借用拆开 *)
  let borrow =
    borrow || (simple_place s && match root s with Some v -> List.mem v !let_params | None -> false)
  in
  List.iter
    (fun ((q : pat), _) ->
      check_pat env ty q;
      let vs = pvars q in
      List.iter
        (fun v -> if List.length (List.filter (( = ) v) vs) > 1 then err q.ppos "变量 %s 在同一个模式里出现了两次" v)
        vs)
    arms;
  let keys = List.sort_uniq compare (List.concat_map (fun (q, _) -> pat_lits q) arms) in
  let depth = List.fold_left (fun m (q, _) -> max m (pat_depth q)) 0 arms in
  let rs =
    try reps env keys depth ty
    with Too_many -> err pos "match 的模式太复杂（要检查的情况超过 100000 种），请拆成几层 match"
  in
  (match List.find_opt (fun r -> first_arm arms r = None) rs with
  | Some r -> err pos "match 没有覆盖所有情况，例如 %s" (show_aval r)
  | None -> ());
  List.iteri
    (fun i ((q : pat), _) ->
      if not (List.exists (fun r -> first_arm arms r = Some i) rs) then
        err q.ppos "这个分支永远匹配不到（前面的分支已经覆盖了它能匹配的值）")
    arms;
  let mkp, pre =
    if simple_place s then ((fun () -> copy_place s), [])
    else
      let n = fresh_pvar () in
      ((fun () -> mkx pos (Var n)), [ { s = Let (false, n, None, s); spos = pos } ])
  in
  let rec chain = function
    | [] -> unreachable pos
    | [ (q, e) ] -> pextract env pos borrow ty mkp q e
    | (q, e) :: rest -> (
        let ex = pextract env pos borrow ty mkp q e in
        match ptest env pos ty mkp q with
        | None -> ex
        | Some t -> mkx pos (If (t, { stmts = []; tail = Some ex }, Some { stmts = []; tail = Some (chain rest) })))
  in
  let d = mkx pos (Block { stmts = pre; tail = Some (chain arms) }) in
  Hashtbl.replace pdesugar x.id d;
  d

and match_ env want pos borrow (s : expr) arms : t =
  match expr env s with
  | Never -> err s.pos "不能 match 一个不会返回的值"
  | Ty ty -> (
      match variants ty with
      | None -> err s.pos "%s 不是 Result 或枚举，不能 match" (string_of_ty ty)
      | Some vs ->
          if borrow && root s = None then
            err s.pos "match let 后面必须是变量、字段或解引用（要借用的位置）";
          enum_match env want pos ty vs arms)

and enum_match env want pos ty vs arms : t =
  List.iter
    (fun (v, _, (a : expr)) ->
      if v <> "_" && not (List.mem_assoc v vs) then err a.pos "%s 没有变体 %s" (string_of_ty ty) (vname v))
    arms;
  (* 通配分支 _（C46，Lean M23 的 covers）：最多一个，只能在最后，而且确实还有没写到的变体 *)
  (match List.filter (fun (v, _, _) -> v = "_") arms with
  | _ :: (_, _, (a : expr)) :: _ -> err a.pos "match 有两个 _ 分支"
  | [ (_, _, (a : expr)) ] ->
      (match List.rev arms with (v, _, _) :: _ when v = "_" -> () | _ -> err a.pos "_ 分支只能放在最后");
      if List.for_all (fun (v, _) -> List.exists (fun (u, _, _) -> u = v) arms) vs then
        err a.pos "所有变体都已经写到了，_ 分支是多余的"
  | [] -> ());
  let bodies =
    List.map
      (fun (v, pty) ->
        let w = vname v in
        match List.filter (fun (u, _, _) -> u = v) arms with
        | [ (_, x, (a : expr)) ] -> (
            match pty with
            | Some TUnit when x = "" || x = "_" ->
                (* () 载荷：分支可以写 Ok => …，也可以写 Ok(_) 或 Ok(u) *)
                (env, a)
            | Some t ->
                if x = "" then err a.pos "%s 带载荷，分支要写 %s(x) 或 %s(_)" w w w;
                ({ env with vars = (x, (t, false)) :: env.vars }, a)
            | None ->
                if x <> "" then err a.pos "%s 没有载荷，分支要写 %s => …" w w;
                (env, a))
        | [] -> (
            (* 没写到的变体用 _ 分支：载荷绑到看不见的变量 _#，分支结束时析构 *)
            match arm_for arms (v, pty) with
            | Some (x, a) when x <> "" -> ({ env with vars = (x, (Option.get pty, false)) :: env.vars }, a)
            | Some (_, a) -> (env, a)
            | None -> err pos "match 没有覆盖 %s（枚举的 match 必须穷尽）" w)
        | _ -> err pos "match 有两个 %s 分支" w)
      vs
  in
  match want, bodies with
  | Some w, _ ->
      List.iter (fun (env', a) -> ignore (check env' w a)) bodies;
      Ty w
  | None, (env', a) :: rest ->
      List.fold_left (fun acc (env', a) -> join pos "match " acc (expr env' a)) (expr env' a) rest
  | None, [] -> err pos "空枚举不能 match"

(* want：块的值的期望类型（块最后的表达式按它检查）。 *)
and block ?want env (b : block) : t =
  let rec go env = function
    | [] -> (
        match b.tail, want with
        | Some e, Some w -> check env w e
        | Some e, None -> expr env e
        | None, _ -> Ty TUnit)
    | (s : stmt) :: rest -> (
        let pos = s.spos in
        match s.s with
        | Decl (v, ty) ->
            check_ty env pos ty;
            (match ty with
            | TArena _ -> err pos "区域不能存进变量"
            | _ -> ());
            go { env with vars = (v, (ty, true)) :: env.vars } rest
        | Let (mut, v, ann, e) ->
            let ty =
              match ann with
              | Some a ->
                  check_ty env pos a;
                  ignore (check env a e);
                  a
              | None -> (
                  match expr env e with
                  | Ty t -> t
                  | Never -> err pos "%s 的初值不会返回，无法推断类型" v)
            in
            (match ty with TArena _ -> err pos "区域不能存进变量" | _ -> ());
            go { env with vars = (v, (ty, mut)) :: env.vars } rest
        | Assign (v, e) ->
            note ~write:true env.vars v;
            (match List.assoc_opt v env.vars with
            | Some (t, true) -> ignore (check env t e)
            | Some (_, false) -> err pos "%s 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）" v
            | None -> err pos "未定义的变量 %s" v);
            go env rest
        | SetField (a, f, e) ->
            (* 给字段赋值：a 是以可修改的变量为根的位置，路径上不经过引用；旧值由 own.ml 先析构 *)
            let rec path (x : expr) =
              match x.e with
              | Var _ -> ()
              | Field (b, _) -> path b
              | Deref b -> (
                  match Hashtbl.find_opt types b.id with
                  | Some (Ty (TBox _)) -> path b
                  | _ -> err x.pos "不能通过引用修改它指向的值")
              | Call (g, [ b; _ ]) when callee x g = "vec_get" -> path b
              | _ -> err x.pos "只能给变量（或它里面的位置）的字段赋值"
            in
            let fty =
              match expr env a with
              | Ty (TStruct n) when Hashtbl.mem enum_tbl n -> err a.pos "%s 是枚举，不能给字段 %s 赋值" n f
              | Ty (TStruct n) -> (
                  match List.find_opt (fun (g : field) -> g.fname = f) (Hashtbl.find env.structs n) with
                  | Some g -> g.fty
                  | None -> err pos "%s 没有字段 %s" n f)
              | Ty t -> err a.pos "%s 不是结构体，不能给字段 %s 赋值" (string_of_ty t) f
              | Never -> err a.pos "不能给一个不会返回的值的字段赋值"
            in
            path a;
            Option.iter (note ~write:true env.vars) (root a);
            (match root a with
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err a.pos "%s 不可修改，不能给它的字段赋值（需要 var 变量或 inout / sink 参数）" v)
            | None -> err a.pos "只能给变量（或它里面的位置）的字段赋值");
            if is_linear fty then err pos "字段 %s 是 linear 值，不能直接赋值（旧值没有被消费）" f;
            ignore (check env fty e);
            go env rest
        | SetPlace (a, e) ->
            (* for x in inout v 里的 x = e（C33）：a 是 vec_get(v, #j)。和给字段赋值一样：v 以可修改的变量为根，
               路径上不经过引用；旧值由 own.ml 先析构（M18 的 replace_reach） *)
            let rec path (x : expr) =
              match x.e with
              | Var _ -> ()
              | Field (b, _) -> path b
              | Deref b -> (
                  match Hashtbl.find_opt types b.id with
                  | Some (Ty (TBox _)) -> path b
                  | _ -> err x.pos "不能通过引用修改它指向的值")
              | Call (g, [ b; _ ]) when callee x g = "vec_get" -> path b
              | _ -> err x.pos "只能给变量里的位置赋值"
            in
            let t = match expr env a with Ty t -> t | Never -> err a.pos "不能给一个不会返回的值赋值" in
            path a;
            Option.iter (note ~write:true env.vars) (root a);
            (match root a with
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err pos "%s 不可修改，不能给它的元素赋值（需要 var 变量或 inout / sink 参数）" v)
            | None -> err pos "只能给变量里的位置赋值");
            if is_linear t then err pos "元素是 linear 值，不能直接赋值（旧值没有被消费）";
            ignore (check env t e);
            go env rest
        | While (c, body) ->
            expect c.pos TBool (expr env c);
            incr loop_depth;
            Fun.protect ~finally:(fun () -> decr loop_depth) (fun () -> expect pos TUnit (block env body));
            go env rest
        | (Break | Continue) as d ->
            (* 之后的语句走不到，和 return 一样记作 Never *)
            if !loop_depth = 0 then err pos "%s 只能用在循环里" (if d = Break then "break" else "continue");
            ignore (go env rest);
            Never
        | Return None ->
            expect pos env.ret (Ty TUnit);
            ignore (go env rest);
            Never
        | Return (Some e) ->
            ignore (check env env.ret e);
            ignore (go env rest);
            Never
        | Destruct (n, bs, e) ->
            (* 拆开结构体：每个字段都要写，移进各自的变量；linear 结构体只能 consume *)
            let m =
              match expr env e with
              | Ty (TStruct m) when (m = n || inst_of m = Some n) && Hashtbl.mem env.structs m -> m
              | Ty t when Ast.is_tuple n ->
                  err e.pos "这里要拆开的是 %s 个元素的元组，实际是 %s" (String.sub n 6 (String.length n - 6)) (string_of_ty t)
              | Ty t -> err e.pos "这里要拆开的是结构体 %s，实际是 %s" n (string_of_ty t)
              | Never -> err e.pos "不能拆开一个不会返回的值"
            in
            if Hashtbl.mem linear_tbl m then err pos "linear 结构体 %s 不能拆开（要用 consume）" n;
            let fs = Hashtbl.find env.structs m in
            List.iter
              (fun (f, _) -> if not (List.exists (fun (g : field) -> g.fname = f) fs) then err pos "%s 没有字段 %s" n f)
              bs;
            List.iter
              (fun (g : field) ->
                match List.filter (fun (f, _) -> f = g.fname) bs with
                | [ _ ] -> ()
                | [] -> err pos "拆开 %s 时缺少字段 %s（每个字段都要写）" n g.fname
                | _ -> err pos "字段 %s 重复" g.fname)
              fs;
            let vars =
              List.fold_left
                (fun vs (f, x) -> (x, ((List.find (fun (g : field) -> g.fname = f) fs).fty, false)) :: vs)
                env.vars bs
            in
            go { env with vars } rest
        | Errdefer e ->
            let v = errdefer env pos e in
            if List.mem v env.guards then err pos "%s 已经有一个生效的 errdefer，不能再登记" v;
            go { env with guards = v :: env.guards } rest
        | Expr e ->
            let t = expr env e in
            let r = go env rest in
            if t = Never then Never else r)
  in
  go env b.stmts

(* errdefer f(…)：实参只能是变量、字段或常量，并且恰好以 sink 传入一个 linear 变量，
   出错返回时由它消费。返回这个变量。 *)
and errdefer env pos (e : expr) =
  match e.e with
  | Call (f, _) when (match List.assoc_opt f env.vars with Some (TFn _, _) -> true | _ -> false) ->
      err pos "errdefer 暂不能调用函数参数 %s" f
  | Call (f, args) when f <> "box" ->
      ignore (expr env e);
      let f = callee e f in
      List.iter
        (fun (a : expr) ->
          match a.e with
          | Var _ | Field _ | Deref _ | Int _ | Float _ | Char _ | Bool _ | Unit -> ()
          | _ -> err a.pos "errdefer 的实参只能是变量、字段或常量")
        args;
      let modes = if f = "consume" then [ MSink ] else List.map fst (fst (Hashtbl.find sigs f)) in
      let targets =
        List.filter_map
          (fun (m, (a : expr)) ->
            match m, a.e with
            | MSink, Var v when is_linear (fst (List.assoc v env.vars)) -> Some v
            | _ -> None)
          (List.combine modes args)
      in
      (match targets with
      | [ v ] ->
          Hashtbl.replace errdefer_targets e.id v;
          v
      | _ -> err pos "errdefer 的调用必须以 sink 方式传入恰好一个 linear 变量（出错时由它消费）")
  | _ -> err pos "errdefer 后面必须是一个函数调用"

let mode_name = function MLet -> "let" | MInout -> "inout" | MSink -> "sink"

(* 整个程序的准备：结构体、枚举（含泛型定义）、函数签名、字段和载荷。返回检查函数体用的环境。 *)
let prepare (p : program) =
  let fns = Hashtbl.create 16 in
  Hashtbl.reset struct_tbl;
  Hashtbl.reset linear_tbl;
  Hashtbl.reset enum_tbl;
  Hashtbl.reset gtypes;
  Hashtbl.reset inst_tbl;
  Hashtbl.reset cov_memo;
  Hashtbl.reset gparams;
  Hashtbl.reset gfns;
  Hashtbl.reset call_inst;
  Hashtbl.reset requested;
  Hashtbl.reset traits;
  Hashtbl.reset methods;
  Hashtbl.reset impls;
  Hashtbl.reset gbounds;
  Hashtbl.reset defaults;
  Hashtbl.reset minst;
  Hashtbl.reset aliases;
  (* 内建的 trait ToStr（C56）：fn to_str(x: Self) -> Str，字符串插值 "{x}" 用它；I64、F64、Char、Bool、Str 已经实现 *)
  Hashtbl.add traits "ToStr" [ "to_str" ];
  Hashtbl.replace methods "to_str" "ToStr";
  Hashtbl.replace fns "to_str" ([ (MLet, TParam ("Self", true)) ], TStr);
  Hashtbl.replace gparams "to_str" [ ("Self", true) ];
  Hashtbl.replace gbounds "to_str" [ ("Self", "ToStr") ];
  List.iter
    (fun t -> Hashtbl.add impls "ToStr" { head = t; itps = []; ibounds = []; defined = [ "to_str" ] })
    [ TI64; TF64; TChar; TBool; TStr ];
  Hashtbl.reset vec_calls;
  Hashtbl.reset method_res;
  Hashtbl.reset skip_lower;
  Hashtbl.reset rewrites;
  Hashtbl.reset packs;
  Hashtbl.reset fcalls;
  Hashtbl.reset pdesugar;
  Hashtbl.reset fn_items;
  Queue.clear pending_items;
  lambda_frame := None;
  (* 内建的 Option<T>：vec_pop 返回它，写法和用户的枚举一样（.Some(x) / .None） *)
  Hashtbl.add gtypes "Option"
    ( [ "T" ],
      Enum
        {
          name = "Option";
          tparams = [ ("T", false) ];
          variants = [ ("Some", Some (TStruct "T")); ("None", None) ];
          ipos = { line = 1; col = 1 };
        } );
  Queue.clear pending;
  let defined n = Hashtbl.mem struct_tbl n || Hashtbl.mem enum_tbl n || Hashtbl.mem gtypes n in
  (* what 不为空：结构体、枚举的类型参数，不能标 linear *)
  let tparams_ok what ipos tps =
    List.iteri
      (fun i (t, lin) ->
        if List.mem_assoc t (List.filteri (fun j _ -> j < i) tps) then err ipos "类型参数 %s 重复" t;
        if lin && what <> "" then err ipos "%s的类型参数 %s 不能标 linear（只有函数的类型参数可以）" what t)
      tps
  in
  (* 先登记所有名字，字段和载荷的类型等名字都登记之后再解析 *)
  List.iter
    (function
      | Struct { name; tparams; fields; linear; ipos } as it ->
          if defined name then err ipos "类型 %s 重复定义" name;
          tparams_ok "结构体" ipos tparams;
          if tparams <> [] then Hashtbl.add gtypes name (List.map fst tparams, it)
          else begin
            Hashtbl.add struct_tbl name fields;
            if linear then Hashtbl.replace linear_tbl name ()
          end
      | Enum { name; tparams; variants; ipos } as it ->
          if defined name then err ipos "类型 %s 重复定义" name;
          tparams_ok "枚举" ipos tparams;
          if variants = [] then err ipos "枚举 %s 至少要有一个变体" name;
          List.iteri
            (fun i (v, _) ->
              if List.exists (fun (w, _) -> w = v) (List.filteri (fun j _ -> j < i) variants) then
                err ipos "枚举 %s 的变体 %s 重复" name v)
            variants;
          if tparams <> [] then Hashtbl.add gtypes name (List.map fst tparams, it)
          else Hashtbl.add enum_tbl name variants
      | Fn { name; ipos; _ } ->
          if Hashtbl.mem fns name || List.mem_assoc name builtins || List.mem name special then
            err ipos "函数 %s 重复定义" name;
          Hashtbl.add fns name ([], TUnit)
      | Trait { name; methods = ms; ipos } ->
          if Hashtbl.mem traits name then err ipos "trait %s 重复定义" name;
          List.iteri
            (fun i (m : msig) ->
              if List.exists (fun (n : msig) -> n.mname = m.mname) (List.filteri (fun j _ -> j < i) ms) then
                err m.mpos "trait %s 的方法 %s 重复" name m.mname)
            ms;
          List.iter (fun (m : msig) -> if m.mbody <> None then Hashtbl.replace defaults m.mname ()) ms;
          Hashtbl.add traits name (List.map (fun (m : msig) -> m.mname) ms)
      | Impl _ -> ())
    p;
  (* 结构体、枚举不能不经过 Box 包含自己（大小是无限的）；经过 Box 的递归可以（Lean M11）。
     泛型类型的实参也算包含（保守：实参只放在 Box 里时也拒绝）。 *)
  let rec visit path n =
    if List.mem n path then
      err { line = 1; col = 1 } "类型 %s 不经过 Box 包含自己，大小是无限的（递归要经过 Box，例如 next: Box<List>）" n;
    let rec inner = function
      | TStruct m -> visit (n :: path) m
      | TApp ("Vec", _) -> ()
      | TApp (m, ts) -> visit (n :: path) m; List.iter inner ts
      | TBox _ | TVec _ -> ()   (* Box、Vec 是指针，经过它们的递归大小有限 *)
      | TResult (a, b) -> inner a; inner b
      | _ -> ()
    in
    let raw = function
      | Struct { fields; _ } -> List.map (fun (f : field) -> f.fty) fields
      | Enum { variants; _ } -> List.filter_map snd variants
      | Fn _ | Trait _ | Impl _ -> []
    in
    match Hashtbl.find_opt struct_tbl n, Hashtbl.find_opt enum_tbl n, Hashtbl.find_opt gtypes n with
    | Some fs, _, _ -> List.iter (fun (f : field) -> inner f.fty) fs
    | _, Some vs, _ -> List.iter (fun (_, t) -> Option.iter inner t) vs
    | _, _, Some (_, it) -> List.iter inner (raw it)
    | None, None, None -> ()
  in
  Hashtbl.iter (fun n _ -> visit [] n) struct_tbl;
  Hashtbl.iter (fun n _ -> visit [] n) enum_tbl;
  Hashtbl.iter (fun n _ -> visit [] n) gtypes;
  (* 解析非泛型结构体、枚举的字段和载荷（Option<I64> 这样的写法换成实例） *)
  List.iter
    (function
      | Struct { name; tparams = []; fields; ipos; _ } ->
          Hashtbl.replace struct_tbl name
            (List.map (fun (f : field) -> { f with fty = resolve [] ipos f.fty }) fields)
      | Enum { name; tparams = []; variants; ipos; _ } ->
          Hashtbl.replace enum_tbl name (List.map (fun (v, t) -> (v, Option.map (resolve [] ipos) t)) variants)
      | Struct _ | Enum _ | Fn _ | Trait _ | Impl _ -> ())
    p;
  (* 函数签名：泛型函数的类型参数解析成 TParam；trait 的方法是带类型参数 Self 的泛型函数；
     impl 里的方法登记成 方法<类型> *)
  List.iter
    (function
      | Struct { tparams; ipos; _ } | Enum { tparams; ipos; _ } ->
          List.iter (fun (t, _) -> if defined t then err ipos "类型参数 %s 和已有的类型同名" t) tparams
      | Fn { name; tparams; bounds; params; ret; ipos; _ } ->
          tparams_ok "" ipos tparams;
          List.iter
            (fun (t, _) ->
              if defined t then err ipos "类型参数 %s 和已有的类型同名" t;
              if t = "Self" then err ipos "Self 不能用作类型参数的名字")
            tparams;
          List.iter (fun (_, tr) -> if not (Hashtbl.mem traits tr) then err ipos "未知 trait %s" tr) bounds;
          if bounds <> [] then Hashtbl.replace gbounds name bounds;
          let sc = List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams in
          let ps = List.map (fun (m, _, t) -> (m, resolve sc ipos t)) params in
          let ret = resolve sc ipos ret in
          if tparams <> [] then begin
            if List.exists (fun (_, t) -> region_names t <> []) ps || region_names ret <> [] then
              err ipos "泛型函数的签名里暂不能直接写区域或引用（类型实参可以是引用）";
            Hashtbl.replace gparams name tparams
          end;
          Hashtbl.replace fns name (ps, ret)
      | Trait { name; methods = ms; _ } ->
          (* Self 可以是任何类型（包括 linear 的），调用处的效果只由参数约定决定（Lean M10） *)
          let sc = [ ("Self", TParam ("Self", true)) ] in
          List.iter
            (fun (m : msig) ->
              if Hashtbl.mem fns m.mname || List.mem_assoc m.mname builtins || List.mem m.mname special
                 || Hashtbl.mem methods m.mname
              then err m.mpos "方法名 %s 和已有的函数或方法同名（暂不支持重载）" m.mname;
              let ps = List.map (fun (md, _, t) -> (md, resolve sc m.mpos t)) m.mparams in
              if List.exists (fun (_, t) -> is_fn t) ps then err m.mpos "trait 的方法暂不能有 fn(…) 参数";
              let ret = resolve sc m.mpos m.mret in
              if List.exists (fun (_, t) -> region_names t <> []) ps || region_names ret <> [] then
                err m.mpos "trait 的方法暂不支持区域参数和引用";
              Hashtbl.replace fns m.mname (ps, ret);
              Hashtbl.replace gparams m.mname [ ("Self", true) ];
              Hashtbl.replace gbounds m.mname [ ("Self", name) ];
              Hashtbl.replace methods m.mname name)
            ms
      | Impl { trait = tr; tparams; bounds; self; fns = ms; ipos } ->
          if not (Hashtbl.mem traits tr) then err ipos "未知 trait %s" tr;
          tparams_ok "" ipos tparams;
          List.iter
            (fun (t, _) ->
              if defined t then err ipos "类型参数 %s 和已有的类型同名" t;
              if t = "Self" then err ipos "Self 不能用作类型参数的名字")
            tparams;
          List.iter (fun (_, b) -> if not (Hashtbl.mem traits b) then err ipos "未知 trait %s" b) bounds;
          let tsc = List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams in
          let st = resolve tsc ipos self in
          type_arg ipos st;
          if region_names st <> [] then err ipos "暂不支持为含引用的类型 %s 实现 trait" (string_of_ty st);
          (match st with
          | TArena _ -> err ipos "不能为 Arena 实现 trait"
          | TFn _ | TOFn _ -> err ipos "不能为函数类型实现 trait"
          | TParam (t, _) -> err ipos "不能为类型参数 %s 本身实现 trait（impl<%s> … for %s 会和所有的 impl 重叠）" t t t
          | _ -> ());
          (* M25 的 WF：impl 的类型参数都要出现在头部里（实例的类型实参由匹配得到） *)
          List.iter
            (fun (t, _) ->
              if not (List.mem t (params_of st)) then
                err ipos "impl 的类型参数 %s 没有出现在 %s 里" t (string_of_ty st))
            tparams;
          (* M25 的 Coherent：同一个 trait 的两个 impl 不能有共同的实例 *)
          List.iter
            (fun i ->
              if compat i.head st then
                if params_of i.head = [] && params_of st = [] then
                  err ipos "%s 已经实现过 %s" (string_of_ty st) tr
                else
                  err ipos "impl %s for %s 和已有的 impl %s for %s 重叠（有的类型两个都能匹配）" tr (string_of_ty st) tr
                    (string_of_ty i.head))
            (Hashtbl.find_all impls tr);
          let defined = List.filter_map (function Fn f -> Some f.name | _ -> None) ms in
          Hashtbl.add impls tr { head = st; itps = List.map fst tparams; ibounds = bounds; defined };
          List.iter
            (function
              | Fn { name; tparams = ftps; params; ret; ipos; _ } ->
                  if ftps <> [] then err ipos "impl 里的方法暂不能有类型参数";
                  if List.exists (fun (_, _, t) -> is_fn t) params then err ipos "impl 里的方法暂不能有 fn(…) 参数";
                  let sc = ("Self", st) :: tsc in
                  let key = inst_name name [ st ] in
                  Hashtbl.replace fns key
                    (List.map (fun (md, _, t) -> (md, resolve sc ipos t)) params, resolve sc ipos ret);
                  (* 泛型 impl 的方法是泛型函数 方法<头部>，类型参数和约束是 impl 的 *)
                  if tparams <> [] then begin
                    Hashtbl.replace gparams key tparams;
                    if bounds <> [] then Hashtbl.replace gbounds key bounds
                  end
              | Struct _ | Enum _ | Trait _ | Impl _ -> ())
            ms)
    p;
  Hashtbl.reset sigs;
  Hashtbl.iter (Hashtbl.replace sigs) fns;
  List.iter (fun (f, s) -> Hashtbl.replace sigs f s) (builtins @ vec_sigs);
  let env0 = { structs = struct_tbl; fns; ret = TUnit; vars = []; guards = []; tbounds = []; clos = [] } in
  (* 字段和载荷的规则；泛型定义按类型参数是抽象类型的实例检查 *)
  let abstract name tparams =
    if tparams = [] then name
    else match instantiate name (List.map (fun (t, _) -> TParam (t, false)) tparams) with TStruct n -> n | _ -> name
  in
  List.iter
    (function
      | Struct { name; tparams; linear; ipos; _ } ->
          List.iter
            (fun (f : field) ->
              if region_names f.fty <> [] then
                err ipos "结构体字段 %s 不能含引用或区域（区域类型还没有实现）" f.fname;
              check_ty env0 ipos f.fty;
              if is_linear f.fty && not linear then
                err ipos "%s 的字段 %s 是 linear 的，%s 也必须标 linear" name f.fname name)
            (Hashtbl.find struct_tbl (abstract name tparams))
      | Enum { name; tparams; ipos; _ } ->
          List.iter
            (fun (v, t) ->
              match t with
              | None -> ()
              | Some t ->
                  if region_names t <> [] then
                    err ipos "%s 的变体 %s 的载荷不能含引用或区域（区域类型还没有实现）" name v;
                  check_ty env0 ipos t;
                  if t = TUnit then err ipos "%s 的变体 %s：没有载荷请直接写 %s" name v v;
                  if is_linear t then err ipos "%s 的变体 %s：载荷暂不能是 linear 值" name v)
            (Hashtbl.find enum_tbl (abstract name tparams))
      | Impl { trait = tr; tparams; self; fns = ms; ipos; _ } ->
          (* impl 和 trait 一致：方法一样，每个方法的参数约定和类型相同（Self 换成实现的类型，Lean M10 的 Conforms） *)
          let st = resolve (List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams) ipos self in
          check_ty env0 ipos st;
          let what = Printf.sprintf "impl %s for %s" tr (string_of_ty st) in
          let names = Hashtbl.find traits tr in
          let got = List.filter_map (function Fn f -> Some (f.name, f.ipos) | _ -> None) ms in
          List.iteri
            (fun i (n, at) ->
              if List.mem_assoc n (List.filteri (fun j _ -> j < i) got) then err at "%s 里的方法 %s 重复" what n;
              if not (List.mem n names) then err at "trait %s 没有方法 %s" tr n)
            got;
          List.iter
            (fun n -> if not (List.mem_assoc n got || Hashtbl.mem defaults n) then err ipos "%s 缺少方法 %s" what n)
            names;
          let s = [ ("Self", st) ] in
          List.iter
            (fun (n, at) ->
              let tps, tret = Hashtbl.find fns n in
              let ips, iret = Hashtbl.find fns (inst_name n [ st ]) in
              if List.length tps <> List.length ips then
                err at "%s 的方法 %s 有 %d 个参数，trait 里声明的是 %d 个" what n (List.length ips) (List.length tps);
              List.iteri
                (fun i ((m1, t1), (m2, t2)) ->
                  let t1 = subst s t1 in
                  if m1 <> m2 then
                    err at "%s 的方法 %s 的第 %d 个参数约定是 %s，trait 里声明的是 %s" what n (i + 1) (mode_name m2)
                      (mode_name m1);
                  if t1 <> t2 then
                    err at "%s 的方法 %s 的第 %d 个参数类型是 %s，应为 %s" what n (i + 1) (string_of_ty t2)
                      (string_of_ty t1))
                (List.combine tps ips);
              if subst s tret <> iret then
                err at "%s 的方法 %s 返回 %s，应为 %s" what n (string_of_ty iret) (string_of_ty (subst s tret)))
            got
      | Trait { methods = ms; _ } ->
          List.iter
            (fun (m : msig) ->
              let ps, ret = Hashtbl.find fns m.mname in
              List.iter (fun (_, t) -> check_ty env0 m.mpos t) ps;
              check_ty env0 m.mpos ret)
            ms
      | Fn _ -> ())
    p;
  env0

(* 把函数里写出的类型换成解析后的：签名取 prepare 解析好的，函数体里的 let 标注、var 声明按类型参数解析。
   泛型函数另外记下来，实例化时复制。 *)
let resolve_item = function
  | Fn f ->
      let ps, ret = Hashtbl.find sigs f.name in
      let sc = List.map (fun (t, lin) -> (t, TParam (t, lin))) f.tparams in
      let it =
        Fn
          {
            f with
            params = List.map2 (fun (m, x, _) (_, t) -> (m, x, t)) f.params ps;
            ret;
            body = map_body ~fresh:false (resolve sc) f.body;
          }
      in
      if f.tparams <> [] then Hashtbl.replace gfns f.name it;
      it
  | it -> it

(* 整个程序的条目逐个解析：impl 换成它的方法（普通函数，名字是 方法<类型>，Self 换成实现的类型）；
   泛型 impl 的方法是泛型函数，类型参数和约束是 impl 的（C56）。
   trait 的签名已经登记过，不再留在程序里；默认方法是泛型函数 方法，类型参数 Self、约束 Self: trait（C56）。 *)
let resolve_items = function
  | Fn _ as it -> [ resolve_item it ]
  | Impl { tparams; bounds; self; fns; ipos; _ } ->
      let tsc = List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams in
      let st = resolve tsc ipos self in
      List.filter_map
        (function
          | Fn f ->
              let name = inst_name f.name [ st ] in
              let ps, ret = Hashtbl.find sigs name in
              let it =
                Fn
                  {
                    f with
                    name;
                    tparams;
                    bounds;
                    params = List.map2 (fun (m, x, _) (_, t) -> (m, x, t)) f.params ps;
                    ret;
                    body = map_body ~fresh:false (resolve (("Self", st) :: tsc)) f.body;
                  }
              in
              if tparams <> [] then Hashtbl.replace gfns name it;
              Some it
          | Struct _ | Enum _ | Trait _ | Impl _ -> None)
        fns
  | Trait { name = tr; methods = ms; _ } ->
      List.filter_map
        (fun (m : msig) ->
          Option.map
            (fun body ->
              resolve_item
                (Fn
                   {
                     name = m.mname;
                     tparams = [ ("Self", true) ];
                     bounds = [ ("Self", tr) ];
                     params = m.mparams;
                     ret = m.mret;
                     body;
                     ipos = m.mpos;
                   }))
            m.mbody)
        ms
  | (Struct _ | Enum _) as it -> [ it ]

(* 检查一个函数。特化（C40）：clos 是 fn(…) 参数绑定的闭包，fvars 是这些参数（已经从参数里去掉，函数体里仍按名字用）。
   rord：闭包捕获的区域参数之间的先后关系（M22）；其余区域参数之间没有顺序（D3）。 *)
let check_fn ?(clos = []) ?(fvars = []) ?(rord = []) env0 = function
  | Fn { name; tparams; params; ret; body; ipos; bounds } ->
      cur_fn := name;
      concrete := tparams = [] && not (List.exists (fun (_, _, t) -> is_fn t) params);
      (* 区域参数：Arena 参数和参数类型里出现的区域名，彼此之间没有顺序（D3） *)
      regions :=
        List.sort_uniq compare (List.concat_map (fun (_, _, t) -> region_names t) params)
        |> List.map (fun r -> (r, Option.value (List.assoc_opt r rord) ~default:[]));
      List.iter
        (fun (m, x, t) ->
          check_ty ~param:true env0 ipos t;
          (match t with
          | TArena _ when m <> MLet -> err ipos "区域参数 %s 只能按 let 传入" x
          | TFn _ when m <> MLet -> err ipos "函数类型的参数 %s 只能按 let 传入（不写约定）" x
          | _ -> ()))
        params;
      check_ty env0 ipos ret;
      let vars = List.rev_map (fun (m, x, t) -> (x, (t, m <> MLet))) params in
      let_params := List.filter_map (fun (m, x, _) -> if m = MLet then Some x else None) params;
      let vars = List.map (fun (y, t) -> (y, (t, false))) fvars @ vars in
      let env = { env0 with ret; vars; tbounds = bounds; clos } in
      expect ipos ret (block env ~want:ret body);
      regions := []
  | Struct _ | Enum _ | Trait _ | Impl _ -> ()

let check_main env0 =
  match Hashtbl.find_opt env0.fns "main" with
  | Some ([], TUnit) when not (is_generic "main") -> ()
  | Some _ -> err { line = 1; col = 1 } "main 必须是 fn main()，没有参数、没有返回值、没有类型参数"
  | None -> err { line = 1; col = 1 } "缺少 fn main()"

(* fn to_str(x: P) -> Str 是 impl ToStr for P { … } 的简写（C49 的写法；C56 起可以给多种类型各写一个），
   fn to_str<T: ToStr>(v: Vec<T>) -> Str 是泛型 impl 的简写。
   模块里的 m.to_str(x: P) -> Str 照常是函数，另外为 P 实现 ToStr（调用它），"{x}" 也就能用了。 *)
let sugar_to_str (p : program) =
  let suffix = ".to_str" in
  let qualified n =
    let k = String.length n and l = String.length suffix in
    k > l && String.sub n (k - l) l = suffix
  in
  List.concat_map
    (function
      | Fn ({ name = "to_str"; tparams; bounds; params; ret; ipos; _ } as f) -> (
          match params, ret with
          | [ (MLet, _, self) ], TStr ->
              [ Impl { trait = "ToStr"; tparams; bounds; self; fns = [ Fn { f with tparams = []; bounds = [] } ]; ipos } ]
          | _ -> err ipos "to_str 是 trait ToStr 的方法，要写成 fn to_str(x: T) -> Str")
      | Fn ({ name; tparams = []; params = [ (MLet, x, self) ]; ret = TStr; ipos; _ } as f) as it when qualified name ->
          let call = { e = Call (name, [ { e = Var x; pos = ipos; id = fresh_id () } ]); pos = ipos; id = fresh_id () } in
          let body = { stmts = []; tail = Some call } in
          [ it; Impl { trait = "ToStr"; tparams = []; bounds = []; self; fns = [ Fn { f with name = "to_str"; body } ]; ipos } ]
      | it -> [ it ])
    p

(* 检查整个程序。返回解析过类型的程序（impl 换成它的方法），后面接上用到的泛型函数实例（单态化），每个实例照常检查。
   方法调用在检查之后换成解析出的函数名；实例是从检查过的泛型函数复制的，复制时已经换好。
   闭包（C40）：提升出的闭包和特化也接在后面、照常检查；调用的改写在全部检查完之后换上。
   带 fn(…) 参数的函数本身不输出（只输出它的特化），函数体里有闭包的泛型函数不在定义处降级。 *)
let program (p : program) : program =
  let p = sugar_to_str p in
  let env0 = prepare p in
  let p = List.concat_map resolve_items p in
  List.iter (function Fn f as it -> Hashtbl.replace fn_items f.name it | _ -> ()) p;
  (* C28：函数之间互不影响，每个函数报它的第一个错误；有错误时不再生成泛型实例（免得同一个错误报好几次） *)
  let errs = ref [] in
  let guard g x =
    try g x with Error (pos, msg) ->
      regions := [];
      errs := (pos, msg) :: !errs
  in
  List.iter (guard (check_fn env0)) p;
  guard check_main env0;
  if !errs <> [] then raise (Errors (List.rev !errs));
  let rec drain acc =
    if List.length acc >= 1000 then
      err { line = 1; col = 1 } "泛型函数的实例超过 1000 个（可能是多态递归）";
    match Queue.take_opt pending with
    | Some r ->
        let it = instance r in
        guard (check_fn env0) it;
        drain (it :: acc)
    | None -> (
        match Queue.take_opt pending_items with
        | Some (it, clos, fvars, rord) ->
            guard (check_fn ~clos ~fvars ~rord env0) it;
            drain (it :: acc)
        | None -> List.rev acc)
  in
  let insts = drain [] in
  if !errs <> [] then raise (Errors (List.rev !errs));
  List.filter_map
    (function
      | Fn f when List.exists (fun (_, _, t) -> is_fn t) f.params -> None
      | Fn f when f.tparams <> [] && Hashtbl.mem skip_lower f.name -> None
      | Fn f -> Some (Fn { f with body = map_body ~fresh:false (fun _ t -> t) f.body })
      | it -> Some it)
    (p @ insts)

(* 对照测试（--batch）：程序里的函数逐个检查，每个连同它引出的泛型实例、提升出的闭包和特化一起，
   返回要降级的条目（规则和 program 相同）。出错时清空队列再抛出，不影响下一个函数。
   调用前要用 batch_items 登记所有函数（特化时按名字找函数）。 *)
let batch_items (p : program) =
  let p = List.map resolve_item p in
  List.iter (function Fn f as it -> Hashtbl.replace fn_items f.name it | _ -> ()) p;
  p

let check_batch env0 it =
  try
    check_fn env0 it;
    let rec drain acc =
      match Queue.take_opt pending with
      | Some r ->
          let i = instance r in
          check_fn env0 i;
          drain (i :: acc)
      | None -> (
          match Queue.take_opt pending_items with
          | Some (i, clos, fvars, rord) ->
              check_fn ~clos ~fvars ~rord env0 i;
              drain (i :: acc)
          | None -> List.rev acc)
    in
    let insts = drain [] in
    List.filter_map
      (function
        | Fn f when List.exists (fun (_, _, t) -> is_fn t) f.params -> None
        | Fn f when f.tparams <> [] && Hashtbl.mem skip_lower f.name -> None
        | Fn f -> Some (Fn { f with body = map_body ~fresh:false (fun _ t -> t) f.body })
        | it -> Some it)
      (it :: insts)
  with e ->
    Queue.clear pending;
    Queue.clear pending_items;
    regions := [];
    raise e
