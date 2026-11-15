(* 源文件和位置。 *)

(* file：文件的序号（0 是主文件），line、col 从 1 开始。字段按这个顺序排，比较位置时先比文件。 *)
type pos = { file : int; line : int; col : int }

(* 编译器合成的、不对应源码的位置（例如 drop flag 这样的临时变量）。 *)
val nopos : pos

(* 主文件的开头：不属于某一行的错误（“缺少 fn main()”）和内建的定义用它。 *)
val start : pos

(* 读进来的文件名，下标是 pos.file。modules.ml 读文件时登记。 *)
val files : string array ref

(* 正在读的文件的序号：词法、语法分析产生的位置都属于它。modules.ml 读每个文件之前设好。 *)
val current : int ref

val of_lexing : Lexing.position -> pos

(* 位置所在的文件名和行号。主文件用 main（命令行上写的路径）。 *)
val locate : string -> pos -> string * int
