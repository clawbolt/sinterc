(* 用法：main.exe 源文件 [-o 输出.c]
   不给 -o 时把 C 代码写到标准输出。出错时打印“文件:行:列: 错误：…”，退出码 1。 *)
open Sinter

let fail file (pos : Ast.pos) msg =
  Printf.eprintf "%s:%d:%d: 错误：%s\n" file pos.line pos.col msg;
  exit 1

let () =
  let file, out =
    match Array.to_list Sys.argv with
    | [ _; f ] -> (f, None)
    | [ _; f; "-o"; o ] -> (f, Some o)
    | _ ->
        prerr_endline "用法：main.exe 源文件 [-o 输出.c]";
        exit 2
  in
  let ic = open_in_bin file in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf file;
  let prog =
    try Parser.program Lexer.token lexbuf with
    | Lexer.Error (pos, msg) -> fail file pos msg
    | Parser.Error ->
        let p = Lexing.lexeme_start_p lexbuf in
        fail file { line = p.pos_lnum; col = p.pos_cnum - p.pos_bol + 1 }
          (Printf.sprintf "语法错误（在 '%s' 附近）" (Lexing.lexeme lexbuf))
  in
  close_in ic;
  (try Typeck.program prog with Typeck.Error (pos, msg) -> fail file pos msg);
  let c = Emit_c.program prog in
  match out with
  | None -> print_string c
  | Some o ->
      let oc = open_out_bin o in
      output_string oc c;
      close_out oc
