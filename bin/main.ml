(* 用法：main.exe 源文件 [-o 输出] [--emit=core] [--unchecked]
   默认生成 C；--emit=core 生成给 Lean 复核的 CFG（附录 F 的 C5）。
   --unchecked 只和 --emit=core 一起用：跳过所有权与投影检查，照样导出，看 Lean 是否独立地拒绝。
   不给 -o 时写到标准输出。出错时打印“文件:行:列: 错误：…”，退出码 1。 *)
open Sinter

let fail file (pos : Ast.pos) msg =
  Printf.eprintf "%s:%d:%d: 错误：%s\n" file pos.line pos.col msg;
  exit 1

let usage () =
  prerr_endline "用法：main.exe 源文件 [-o 输出] [--emit=core] [--unchecked]";
  exit 2

let () =
  let file = ref None and out = ref None and core = ref false and unchecked = ref false in
  let rec args = function
    | [] -> ()
    | "-o" :: o :: rest -> out := Some o; args rest
    | "--emit=core" :: rest -> core := true; args rest
    | "--emit=c" :: rest -> core := false; args rest
    | "--unchecked" :: rest -> unchecked := true; args rest
    | f :: rest when !file = None && String.length f > 0 && f.[0] <> '-' -> file := Some f; args rest
    | _ -> usage ()
  in
  args (List.tl (Array.to_list Sys.argv));
  let file = match !file with Some f -> f | None -> usage () in
  if !unchecked && not !core then usage ();
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
  (* 类型检查、降级、所有权检查、投影与排他性检查都可能报错 *)
  let fs =
    try
      Typeck.program prog;
      let check = not !unchecked in
      List.map
        (fun f ->
          let f = Own.elaborate ~check f in
          if check then Borrow.check f else f)
        (Mir.lower prog)
    with Typeck.Error (pos, msg) -> fail file pos msg
  in
  let text = if !core then Emit_core.program ~expect_ok:(not !unchecked) fs else Emit_c.program prog fs in
  match !out with
  | None -> print_string text
  | Some o ->
      let oc = open_out_bin o in
      output_string oc text;
      close_out oc
