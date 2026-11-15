(* 用法：
     sinterc run 源文件.sn [参数…]    编译并运行，后面的参数交给程序（args()），退出码就是程序的退出码
     sinterc build 源文件.sn [-o 程序]  编译成可执行文件（默认和源文件同名，去掉 .sn）
     sinterc 源文件.sn [-o 输出.c]     只生成 C（不给 -o 时写到标准输出）
     sinterc --version / --help
   build 和 run 调用系统的 C 编译器：环境变量 CC，默认 cc。
   --lang=en（或环境变量 SINTER_LANG=en）：错误信息和运行时的 panic 用英文，默认中文。
   开发用的选项：--emit=core 生成给 Lean 复核的 CFG（附录 F 的 `Checking.Validate`）；
   --unchecked 只和 --emit=core 一起用：跳过所有权与投影检查、区域的先后和 R3 检查，照样导出，看 Lean 是否独立地拒绝；
   --batch：对照测试，见 batch.ml。
   出错时打印“文件:行:列: 错误：…”和出错的那一行，退出码 1（见 report.ml）。

   这个目录：cli.ml 命令行，pipeline.ml 编译的各个阶段，report.ml 报错，cc.ml 调用 C 编译器，
   fmt_cmd.ml 是 sinterc fmt，batch.ml 是 --batch。 *)
open Cli

let () =
  match parse (List.tl (Array.to_list Sys.argv)) with
  | Fmt argv -> Fmt_cmd.main argv
  | Compile o -> (
      let prog = Pipeline.load o.file in
      if o.batch then Batch.run ~out:o.out o.file prog;
      let prog, fs = Pipeline.check ~unchecked:o.unchecked o.file prog in
      let text = Pipeline.emit ~core:o.core ~unchecked:o.unchecked prog fs in
      match o.mode, o.out with
      | Emit, None -> print_string text
      | Emit, Some out -> Cc.write out text
      | Build, out -> Cc.build ~release:o.release ~out o.file text
      | Run, _ -> Cc.run ~release:o.release ~args:o.prog_args text)
