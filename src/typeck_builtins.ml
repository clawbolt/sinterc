(* 内建函数的签名和参数约定，位置表达式。哪个方法对应哪个内建函数写在核心部分里（std/core 的 intrinsic fn）。 *)
open Ast
open Typeck_env

(* 内建函数按值接收可复制的参数，记作 sink；字符串按 let（只读借用）或 inout 传入。
   核心部分的 intrinsic fn 要写成同样的签名（typeck.ml 检查）。 *)
let builtins =
  [
    "print", ([ (MLet, TStr) ], TUnit);
    (* 不换行的输出。print_str 原样写出字符串的字节；print_byte 写一个原始字节（0 到 255，否则 panic）。
       输出经过 C 的缓冲，read_line 之前、panic 和程序结束时都会刷新 *)
    "print_str", ([ (MLet, TStr) ], TUnit);
    "print_byte", ([ (MSink, TI64) ], TUnit);
    (* 写到标准错误。eprint 末尾加换行，eprint_str 不加；写之前先刷新标准输出 *)
    "eprint", ([ (MLet, TStr) ], TUnit);
    "eprint_str", ([ (MLet, TStr) ], TUnit);
    "str_len", ([ (MLet, TStr) ], TI64);
    "str_eq", ([ (MLet, TStr); (MLet, TStr) ], TBool);
    "str_concat", ([ (MLet, TStr); (MLet, TStr) ], TStr);
    "str_push", ([ (MInout, TStr); (MLet, TStr) ], TUnit);
    "str_clone", ([ (MLet, TStr) ], TStr);
    "str_from_i64", ([ (MSink, TI64) ], TStr);
    "str_from_bool", ([ (MSink, TBool) ], TStr);
    (* F64。str_from_f64 是能还原的最短写法（整数值带 .0）；to_f64 把 I64 换成 F64，
       to_i64 向零取整（NaN 或超出 I64 范围时 panic）；str_to_f64 解析（格式不对时 .None）。
       f64_* 是核心部分 F64 的 sqrt 等方法用的内建函数 *)
    "str_from_f64", ([ (MSink, TF64) ], TStr);
    "str_to_f64", ([ (MLet, TStr) ], TApp ("Option", [ TF64 ]));
    "to_f64", ([ (MSink, TI64) ], TF64);
    "to_i64", ([ (MSink, TF64) ], TI64);
    "f64_sqrt", ([ (MSink, TF64) ], TF64);
    "f64_floor", ([ (MSink, TF64) ], TF64);
    "f64_ceil", ([ (MSink, TF64) ], TF64);
    "f64_round", ([ (MSink, TF64) ], TF64);
    "f64_is_nan", ([ (MSink, TF64) ], TBool);
    (* 字符（Lean `Library.Utf8`）。char_from 在不是合法码点时返回 .None；str_chars 按 `Library.Utf8` 的 decode1 逐个解码
       （不合法的字节解成 U+FFFD），结果是新的 Vec<Char>；str_char_count 是字符的个数 *)
    "char_code", ([ (MSink, TChar) ], TI64);
    "char_from", ([ (MSink, TI64) ], TApp ("Option", [ TChar ]));
    "str_from_char", ([ (MSink, TChar) ], TStr);
    "str_push_char", ([ (MInout, TStr); (MSink, TChar) ], TUnit);
    "str_chars", ([ (MLet, TStr) ], TVec TChar);
    "str_char_count", ([ (MLet, TStr) ], TI64);
    (* 读一行（读到末尾时 .None，去掉行尾的换行）、解析整数（格式不对或超出范围时 .None）、
       第 i 个字节、[a, b) 的子串（新的 Str）；下标越界时 panic *)
    "read_line", ([], TApp ("Option", [ TStr ]));
    "str_to_i64", ([ (MLet, TStr) ], TApp ("Option", [ TI64 ]));
    "str_byte", ([ (MLet, TStr); (MSink, TI64) ], TI64);
    "str_slice", ([ (MLet, TStr); (MSink, TI64); (MSink, TI64) ], TStr);
    "choose", ([], TBool);       (* 对照测试用：按运行时给定的选择序列返回真假 *)
    (* 文件与参数（Lean `Library.FileIO`）：命令行参数（不含程序名）；读整个文件；把文件换成 s（先写临时文件再 rename，失败时原文件不变）。
       出错时 Err(说明)，返回的字符串都是新分配的 *)
    "args", ([], TVec TStr);
    "read_file", ([ (MLet, TStr) ], TResult (TStr, TStr));
    "write_file", ([ (MLet, TStr); (MLet, TStr) ], TResult (TUnit, TStr));
    (* 字节串 Bytes（任意字节）。get、set、push 的字节是 0 到 255 的 I64（push、set 超出时 panic），
       下标越界时 panic；str_to_bytes 复制字符串的字节；str_from_utf8 只在合法的 UTF-8 时得到 .Some（Lean `Library.Utf8Check`）；
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
    (* std.os 用的内建函数（名字带 os_，不占用户的名字）。和文件读写一样只读外部的状态、返回新的值（Lean `Library.FileIO` 的形状）：
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

(* 字符串切片的内建函数（Lean `Strings.Slice`）：text(a, s) 把 s 放进区域 a，得到整段的切片；view_… 是切片的方法。
   结果里的区域跟着参数走，所以和 alloc 一样单独检查类型（expr）；这里只给降级用的参数约定，区域写 _ *)
let view_sigs =
  let v = TRef (TView, "_") in
  [
    "text", ([ (MLet, TArena "_"); (MSink, TStr) ], v);
    (* s.view()：借自 s 的整段切片（返回借用），区域是 s 的名字 *)
    "str_view", ([ (MLet, TStr) ], v);
    "view_len", ([ (MLet, v) ], TI64);
    "view_byte", ([ (MLet, v); (MSink, TI64) ], TI64);
    "view_sub", ([ (MLet, v); (MSink, TI64); (MSink, TI64) ], v);
    "view_to_str", ([ (MLet, v) ], TStr);
    "view_eq", ([ (MLet, v); (MLet, TStr) ], TBool);
    "view_eq_view", ([ (MLet, v); (MLet, v) ], TBool);
    "view_find", ([ (MLet, v); (MLet, TStr); (MSink, TI64) ], TI64);
    "view_print_str", ([ (MLet, v) ], TUnit);
    (* 流式读入：读到的字节放进区域 a 里新的缓冲区（Lean `Strings.Slice` 的 fresh_valid），得到它的切片，不经过 Str *)
    "read_file_in", ([ (MLet, TArena "_"); (MLet, TStr) ], TResult (v, TStr));
    "read_stdin_in", ([ (MLet, TArena "_") ], v);
    "read_chunk", ([ (MLet, TArena "_"); (MSink, TI64) ], v);
  ]

(* 特殊处理的内建函数：参数类型不固定。 *)
let special =
  [ "box"; "unbox"; "consume"; "alloc"; "vec_new"; "vec_push"; "vec_pop"; "vec_len"; "vec_get"; "vec_swap"; "panic"; "os_exit"; "ref_of" ]
  @ List.map fst view_sigs

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

