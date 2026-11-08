(* 用法：
     sinterc run 源文件.sn            编译并运行，退出码就是程序的退出码
     sinterc build 源文件.sn [-o 程序]  编译成可执行文件（默认和源文件同名，去掉 .sn）
     sinterc 源文件.sn [-o 输出.c]     只生成 C（不给 -o 时写到标准输出）
     sinterc --version / --help
   build 和 run 调用系统的 C 编译器：环境变量 CC，默认 cc。
   开发用的选项：--emit=core 生成给 Lean 复核的 CFG（附录 F 的 C5）；
   --unchecked 只和 --emit=core 一起用：跳过所有权与投影检查、区域的先后和 R3 检查，照样导出，看 Lean 是否独立地拒绝；
   --batch：对照测试，见下面。
   出错时打印“文件:行:列: 错误：…”，退出码 1。 *)
open Sinter

let version = "0.1.0"

let fail file (pos : Ast.pos) msg =
  Printf.eprintf "%s:%d:%d: 错误：%s\n" file pos.line pos.col msg;
  exit 1

let help =
  "Sinter 编译器 sinterc " ^ version ^ "\n\n\
   用法：\n\
  \  sinterc run 源文件.sn             编译并运行\n\
  \  sinterc build 源文件.sn [-o 程序]  编译成可执行文件\n\
  \  sinterc 源文件.sn [-o 输出.c]      只生成 C\n\
  \  sinterc --version | --help\n\n\
   build 和 run 用环境变量 CC 指定的 C 编译器（默认 cc）。\n"

let usage () =
  prerr_string help;
  exit 2

(* 把生成的 C 交给系统的 C 编译器。 *)
let cc c_file exe =
  let cc = Option.value (Sys.getenv_opt "CC") ~default:"cc" in
  let cmd = Printf.sprintf "%s -std=gnu11 -O1 -w %s -o %s" cc (Filename.quote c_file) (Filename.quote exe) in
  if Sys.command cmd <> 0 then begin
    Printf.eprintf "错误：C 编译器（%s）没有成功，可以用环境变量 CC 换一个（需要 GCC 或 Clang）\n" cc;
    exit 1
  end

type mode = Emit | Build | Run

let () =
  let file = ref None and out = ref None and core = ref false and unchecked = ref false
  and batch = ref false in
  let argv = List.tl (Array.to_list Sys.argv) in
  let mode, argv =
    match argv with
    | "run" :: rest -> (Run, rest)
    | "build" :: rest -> (Build, rest)
    | ("--version" | "-v") :: _ -> print_endline ("sinterc " ^ version); exit 0
    | ("--help" | "-h") :: _ -> print_string help; exit 0
    | _ -> (Emit, argv)
  in
  let rec args = function
    | [] -> ()
    | "-o" :: o :: rest -> out := Some o; args rest
    | "--emit=core" :: rest -> core := true; args rest
    | "--emit=c" :: rest -> core := false; args rest
    | "--unchecked" :: rest -> unchecked := true; args rest
    | "--batch" :: rest -> batch := true; args rest
    | f :: rest when !file = None && String.length f > 0 && f.[0] <> '-' -> file := Some f; args rest
    | _ -> usage ()
  in
  args argv;
  let file = match !file with Some f -> f | None -> usage () in
  if !unchecked && not !core then usage ();
  if mode <> Emit && (!core || !batch) then usage ();
  let ic =
    try open_in_bin file with Sys_error _ ->
      Printf.eprintf "错误：打不开文件 %s\n" file;
      exit 1
  in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf file;
  let prog =
    try Parser.program Lexer.token lexbuf with
    | Lexer.Error (pos, msg) | Ast.Syntax (pos, msg) -> fail file pos msg
    | Parser.Error ->
        let p = Lexing.lexeme_start_p lexbuf in
        fail file { line = p.pos_lnum; col = p.pos_cnum - p.pos_bol + 1 }
          (Printf.sprintf "语法错误（在 '%s' 附近）" (Lexing.lexeme lexbuf))
  in
  close_in ic;
  if !batch then begin
    (* 对照测试：名字形如 p123_y / p123_n 的函数逐个检查，y / n 是 Lean（acceptsG）的结论。
       要求 Lean 接受的 sinterc 都接受；sinterc 接受的生成 C，运行时检查（附录 F 的 C5）。 *)
    let env0 = try Typeck.prepare prog with Typeck.Error (pos, msg) -> fail file pos msg in
    let lean_says name =
      let n = String.length name in
      if n > 3 && name.[0] = 'p' && name.[n - 2] = '_' then Some (name.[n - 1] = 'y') else None
    in
    let compile it =
      let it = Typeck.resolve_item it in
      Typeck.check_fn env0 it;
      List.map (fun f -> Borrow.check (Own.elaborate f)) (Mir.lower [ it ])
    in
    let both = ref 0 and neither = ref 0 and only_lean = ref [] and only_sn = ref 0
    and internal = ref [] in
    let fs =
      List.concat_map
        (fun it ->
          match it with
          | Ast.Fn { name; ipos; _ } -> (
              match lean_says name with
              | None -> ( try compile it with Typeck.Error (pos, msg) -> fail file pos msg)
              | Some lean -> (
                  match compile it with
                  | fs ->
                      if lean then incr both else incr only_sn;
                      fs
                  | exception Typeck.Error (pos, msg) ->
                      if lean then
                        only_lean := Printf.sprintf "%s（%d:%d: %s）" name pos.line pos.col msg :: !only_lean
                      else incr neither;
                      []
                  | exception e ->
                      internal :=
                        Printf.sprintf "%s（第 %d 行：%s）" name ipos.line (Printexc.to_string e) :: !internal;
                      []))
          | Ast.Struct _ | Ast.Enum _ | Ast.Trait _ | Ast.Impl _ -> [])
        prog
    in
    let total = !both + !neither + List.length !only_lean + !only_sn + List.length !internal in
    Printf.printf
      "对照：程序 %d 个；两边都接受 %d，两边都拒绝 %d，Lean 拒绝而 sinterc 接受 %d（允许：sinterc 会自动析构），Lean 接受而 sinterc 拒绝 %d，sinterc 内部错误 %d\n"
      total !both !neither !only_sn (List.length !only_lean) (List.length !internal);
    let show title l =
      if l <> [] then begin
        Printf.printf "%s，前 10 个：\n" title;
        List.iteri (fun i s -> if i < 10 then Printf.printf "  %s\n" s) (List.rev l)
      end
    in
    show "Lean 接受而 sinterc 拒绝" !only_lean;
    show "sinterc 内部错误" !internal;
    let names =
      List.filter_map (fun (f : Mir.func) -> if lean_says f.name <> None then Some f.name else None) fs
    in
    let text = Emit_c.program ~batch:names prog fs in
    (match !out with
    | None -> print_string text
    | Some o ->
        let oc = open_out_bin o in
        output_string oc text;
        close_out oc);
    exit (if !only_lean = [] && !internal = [] then 0 else 1)
  end;
  (* 类型检查、降级、所有权检查、投影与排他性检查都可能报错。
     泛型函数本身也降级并检查（定义处检查，Lean M9），但只输出它的实例。 *)
  let prog, fs =
    try
      Typeck.lax_regions := !unchecked;
      let prog = Typeck.program prog in
      let check = not !unchecked in
      let fs =
        List.map
          (fun f ->
            let f = Own.elaborate ~check f in
            if check then Borrow.check f else f)
          (Mir.lower prog)
      in
      (prog, List.filter (fun (f : Mir.func) -> not (Typeck.is_generic f.name)) fs)
    with Typeck.Error (pos, msg) -> fail file pos msg
  in
  let text = if !core then Emit_core.program ~expect_ok:(not !unchecked) fs else Emit_c.program prog fs in
  let write o =
    let oc = open_out_bin o in
    output_string oc text;
    close_out oc
  in
  match mode, !out with
  | Emit, None -> print_string text
  | Emit, Some o -> write o
  | Build, o ->
      let exe =
        match o with
        | Some o -> o
        | None -> if Filename.check_suffix file ".sn" then Filename.chop_suffix file ".sn" else file ^ ".out"
      in
      let c = Filename.temp_file "sinter" ".c" in
      write c;
      cc c exe;
      Sys.remove c
  | Run, _ ->
      let c = Filename.temp_file "sinter" ".c" and exe = Filename.temp_file "sinter" ".exe" in
      write c;
      cc c exe;
      Sys.remove c;
      flush_all ();
      let code = Sys.command (Filename.quote exe) in
      Sys.remove exe;
      exit code
