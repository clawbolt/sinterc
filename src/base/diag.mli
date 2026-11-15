(* 诊断：编译器报给用户的错误。 *)

(* 一条信息：key 是代码里写的中文格式串，args 是按顺序填进去的参数（已经是文字）。 *)
type message = { key : string; args : string list }

(* 一个错误：位置和信息。 *)
type t = { pos : Source.pos; message : message }

(* 阶段内部遇到第一个错误就抛出。 *)
exception Error of t

(* 一次报多个错误：类型检查按函数收集，每个函数各自的第一个错误，按出现的顺序。 *)
exception Errors of t list

(* error pos "格式 %s" x：记下格式串和参数，抛 Error。格式里只能用 %s、%d、%c。 *)
val error : Source.pos -> ('a, unit, string, 'b) format4 -> 'a

(* message "格式 %s" x：只做信息不抛出，留到之后再报（例如 MIR 的 Live 检查）。 *)
val message : ('a, unit, string, message) format4 -> 'a

val raise_at : Source.pos -> message -> 'a

(* 中文的信息：把参数填回格式串。英文见 Messages.render。 *)
val chinese : message -> string

(* fill 格式串 参数：把 %s、%d、%c 依次换成参数，%% 换成 %。 *)
val fill : string -> string list -> string
