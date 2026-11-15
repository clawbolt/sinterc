(* 调用系统的 C 编译器，以及 build、run 用到的文件操作。 *)
open Sinter

let write path text =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

(* 把生成的 C 交给系统的 C 编译器（环境变量 CC，默认 cc）。
   release：-O2，并定义 SN_RELEASE（不统计分配次数、不做退出时的泄漏检查）。 *)
let compile ~release c_file exe =
  let cc = Option.value (Sys.getenv_opt "CC") ~default:"cc" in
  let opt = if release then "-O2 -DSN_RELEASE" else "-O1" in
  let cmd = Printf.sprintf "%s -std=gnu11 %s -w %s -o %s -lm -pthread" cc opt (Filename.quote c_file) (Filename.quote exe) in
  if Sys.command cmd <> 0 then begin
    prerr_endline
      (Messages.pick
         (Printf.sprintf "错误：C 编译器（%s）没有成功，可以用环境变量 CC 换一个（需要 GCC 或 Clang）" cc)
         (Printf.sprintf "error: the C compiler (%s) failed; set CC to another one (GCC or Clang is required)" cc));
    exit 1
  end

(* sinterc build：可执行文件默认和源文件同名，去掉 .sn。 *)
let build ~release ~out file text =
  let exe =
    match out with
    | Some o -> o
    | None -> if Filename.check_suffix file ".sn" then Filename.chop_suffix file ".sn" else file ^ ".out"
  in
  let c = Filename.temp_file "sinter" ".c" in
  write c text;
  compile ~release c exe;
  Sys.remove c

(* sinterc run：编译到临时文件、运行，退出码就是程序的退出码。 *)
let run ~release ~args text =
  let c = Filename.temp_file "sinter" ".c" and exe = Filename.temp_file "sinter" ".exe" in
  write c text;
  compile ~release c exe;
  Sys.remove c;
  flush_all ();
  let code = Sys.command (String.concat " " (List.map Filename.quote (exe :: args))) in
  Sys.remove exe;
  exit code
