(* 源文件和位置。一个程序由几个文件组成：0 是主文件，之后是引入的模块和标准库，按 modules.ml 读到的顺序编号。
   位置记下文件的序号和文件里的行、列，报错时再用 files 换回文件名。 *)

type pos = { file : int; line : int; col : int }

let nopos = { file = 0; line = 0; col = 0 }
let start = { file = 0; line = 1; col = 1 }

let files : string array ref = ref [||]
let current = ref 0

let of_lexing (q : Lexing.position) = { file = !current; line = q.pos_lnum; col = q.pos_cnum - q.pos_bol + 1 }

let locate main (p : pos) =
  if p.file > 0 && p.file < Array.length !files then (!files.(p.file), p.line) else (main, p.line)
