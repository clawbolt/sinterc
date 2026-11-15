(* 把诊断显示给用户：“文件:行:列: 错误：…”，下面是出错的那一行，再下一行用 ^ 标出列：
     ok.sn:4:11: 错误：s 已被移动，不能再使用
       4 |     print(s.0)
         |           ^
   源码按文件读一次缓存起来；标准库的模块（std/m.sn）用编进编译器的源码。读不到或行号超出时只打印第一行。
   列号按字节数，^ 前的空白按显示宽度算：ASCII 占 1 格，其他字符（中文等）占 2 格，制表符照抄。 *)
open Sinter

let sources : (string, string array option) Hashtbl.t = Hashtbl.create 4

let source_lines path =
  match Hashtbl.find_opt sources path with
  | Some ls -> ls
  | None ->
      let text =
        let std = "std/" in
        if String.length path > String.length std && String.sub path 0 (String.length std) = std then
          List.assoc_opt (Filename.chop_suffix (Filename.basename path) ".sn") Stdlib_src.files
        else
          try
            let ic = open_in_bin path in
            let s = really_input_string ic (in_channel_length ic) in
            close_in ic;
            Some s
          with Sys_error _ -> None
      in
      let ls = Option.map (fun s -> Array.of_list (String.split_on_char '\n' s)) text in
      Hashtbl.replace sources path ls;
      ls

(* 和整个程序有关、不指向某一行的错误：不显示源码 *)
let whole_program (m : Diag.message) =
  List.exists
    (fun p -> String.length m.key >= String.length p && String.sub m.key 0 (String.length p) = p)
    [ "缺少 fn main()"; "main 必须是"; "泛型函数的实例超过"; "泛型类型的实例超过" ]

let caret_pad line col =
  let b = Buffer.create 16 in
  let n = min (col - 1) (String.length line) in
  String.iteri
    (fun i c ->
      if i < n then
        if c = '\t' then Buffer.add_char b '\t'
        else if Char.code c < 0x80 then Buffer.add_char b ' '
        else if Char.code c >= 0xC0 then Buffer.add_string b "  ")   (* 多字节字符的首字节 *)
    line;
  Buffer.contents b

let snippet path line col =
  match source_lines path with
  | Some ls when line >= 1 && line <= Array.length ls ->
      let text = ls.(line - 1) in
      let text = if String.ends_with ~suffix:"\r" text then String.sub text 0 (String.length text - 1) else text in
      let num = string_of_int line in
      let gutter = String.make (String.length num + 1) ' ' in
      Printf.eprintf " %s | %s\n%s | %s^\n" num text gutter (caret_pad text col)
  | _ -> ()

(* 核心部分的名字在编译器里带前缀 core.（Modules.core），报错时按用户写的样子显示：core.Ord 显示成 Ord。 *)
let unprefix_core msg =
  let p = Modules.core ^ "." in
  let n = String.length p and b = Buffer.create (String.length msg) in
  let ident c = match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' -> true | _ -> false in
  let i = ref 0 in
  while !i < String.length msg do
    if !i + n <= String.length msg && String.sub msg !i n = p && (!i = 0 || not (ident msg.[!i - 1])) then i := !i + n
    else begin
      Buffer.add_char b msg.[!i];
      incr i
    end
  done;
  Buffer.contents b

(* 位置可能在引入的模块里：换回那个文件的名字和行号。file 是命令行上的主文件。 *)
let report file (e : Diag.t) =
  let file, line = Source.locate file e.pos in
  Printf.eprintf "%s:%d:%d: %s%s\n" file line e.pos.col (Messages.pick "错误：" "error: ")
    (unprefix_core (Messages.render e.message));
  if not (whole_program e.message) then snippet file line e.pos.col

let fail file e =
  report file e;
  exit 1

(* 一次报多个错误：按位置排序（先按文件），同一个位置按中文的信息，最多显示 20 个。 *)
let max_errors = 20

let fail_many file errs =
  let order (e : Diag.t) = (e.pos, Diag.chinese e.message) in
  let errs = List.sort_uniq (fun a b -> compare (order a) (order b)) errs in
  List.iteri (fun i e -> if i < max_errors then report file e) errs;
  let n = List.length errs in
  if n > max_errors then
    prerr_endline
      (Messages.pick
         (Printf.sprintf "（还有 %d 个错误没有显示）" (n - max_errors))
         (Printf.sprintf "(%d more errors not shown)" (n - max_errors)));
  exit 1

let cannot_open f =
  prerr_endline (Messages.pick ("错误：打不开文件 " ^ f) ("error: cannot open file " ^ f));
  exit 1
