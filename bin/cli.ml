(* 命令行：语言、子命令和选项。用法见 main.ml 开头和 help。 *)
open Sinter

let version = "0.2.0-pre"

let help () =
  Messages.pick
    ("Sinter 编译器 sinterc " ^ version ^ "\n\n\
      用法：\n\
     \  sinterc run [--release] 源文件.sn [参数…]     编译并运行\n\
     \  sinterc build [--release] 源文件.sn [-o 程序]  编译成可执行文件\n\
     \  sinterc 源文件.sn [-o 输出.c]      只生成 C\n\
     \  sinterc fmt [--check | -w] 文件…  格式化（默认打印结果，-w 写回，--check 只检查）\n\
     \  sinterc --version | --help\n\n\
      build 和 run 用环境变量 CC 指定的 C 编译器（默认 cc）。\n\
      --release：用 -O2 编译，不做退出时的泄漏检查（默认 -O1，有泄漏时退出码是 102）。\n\
      --lang=en 或 SINTER_LANG=en：英文的错误信息。\n")
    ("Sinter compiler sinterc " ^ version ^ "\n\n\
      Usage:\n\
     \  sinterc run [--release] FILE.sn [ARGS…]     compile and run\n\
     \  sinterc build [--release] FILE.sn [-o EXE]  compile to an executable\n\
     \  sinterc FILE.sn [-o OUT.c]      emit C only\n\
     \  sinterc fmt [--check | -w] FILE…  format (print; -w writes back; --check only checks)\n\
     \  sinterc --version | --help\n\n\
      build and run use the C compiler named by $CC (default: cc).\n\
      --release: compile with -O2 and skip the leak check at exit (default: -O1, exit code 102 on a leak).\n\
      --lang=zh or SINTER_LANG=zh: error messages in Chinese.\n")

let usage () =
  prerr_string (help ());
  exit 2

type mode = Emit | Build | Run

type options = {
  mode : mode;
  file : string;
  out : string option;  (* -o *)
  core : bool;  (* --emit=core：输出给 Lean 复核的 CFG，不是 C *)
  unchecked : bool;  (* 只和 --emit=core 一起用 *)
  batch : bool;  (* 对照测试，见 batch.ml *)
  release : bool;
  prog_args : string list;  (* run：源文件后面的都是给程序的参数（args() 读到的） *)
}

type command = Fmt of string list | Compile of options

(* --lang 可以写在任何位置（run 的程序参数里也算），先取出来。 *)
let set_lang argv =
  (match Sys.getenv_opt "SINTER_LANG" with
  | Some ("en" | "EN") -> Messages.lang := Messages.En
  | _ -> ());
  List.filter
    (fun a ->
      match a with
      | "--lang=en" -> Messages.lang := Messages.En; false
      | "--lang=zh" -> Messages.lang := Messages.Zh; false
      | _ -> true)
    argv

let parse argv =
  let argv = set_lang argv in
  let mode, argv =
    match argv with
    | "run" :: rest -> (Some Run, rest)
    | "build" :: rest -> (Some Build, rest)
    | "fmt" :: rest -> (None, rest)
    | ("--version" | "-v") :: _ -> print_endline ("sinterc " ^ version); exit 0
    | ("--help" | "-h") :: _ -> print_string (help ()); exit 0
    | _ -> (Some Emit, argv)
  in
  match mode with
  | None -> Fmt argv
  | Some mode ->
      let file = ref None and out = ref None and core = ref false and unchecked = ref false
      and batch = ref false and release = ref false and prog_args = ref [] in
      let rec args = function
        | [] -> ()
        | rest when mode = Run && !file <> None -> prog_args := rest
        | "-o" :: o :: rest -> out := Some o; args rest
        | "--emit=core" :: rest -> core := true; args rest
        | "--emit=c" :: rest -> core := false; args rest
        | "--unchecked" :: rest -> unchecked := true; args rest
        | "--batch" :: rest -> batch := true; args rest
        | "--release" :: rest -> release := true; args rest
        | f :: rest when !file = None && String.length f > 0 && f.[0] <> '-' -> file := Some f; args rest
        | _ -> usage ()
      in
      args argv;
      let file = match !file with Some f -> f | None -> usage () in
      if !unchecked && not !core then usage ();
      if mode <> Emit && (!core || !batch) then usage ();
      (* 只生成 C 时没有 C 编译这一步，--release 没有意义（自己编译时加 -DSN_RELEASE） *)
      if mode = Emit && !release then usage ();
      Compile
        {
          mode;
          file;
          out = !out;
          core = !core;
          unchecked = !unchecked;
          batch = !batch;
          release = !release;
          prog_args = !prog_args;
        }
