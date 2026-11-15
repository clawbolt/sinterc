(* C 后端：从 MIR 生成整个 C 文件。输入是 MIR、定义（Defs）和要写 C 辅助函数的东西（Tast.generated）；不依赖 Typeck。
   分成四个文件，按生成的 C 里的顺序：
   - c_names.ml：C 名字和 C 类型；
   - c_types.ml：类型定义、析构函数、Vec 宏、运行时库；
   - c_func.ml：每个 MIR 函数；
   - c_closures.ml：闭包、任务、Fn 调用。
   这里把它们按顺序拼起来，再接上入口。 *)
open Ast

(* 和程序无关的 C 运行时在 runtime/sinter.h，构建时编进编译器（src/dune）。 *)
let prelude = Runtime_src.prelude

(* batch：对照测试的函数名。给了的话不调用 u_main，而是每个函数按所有长度不超过 3 的选择序列各跑一遍，
   每次跑完检查有没有没释放的 Box 或区域内存块。 *)
let program ?batch d (g : Tast.generated) (p : program) (fs : Mir.func list) : string =
  let b = Buffer.create 4096 in
  let add s = Buffer.add_string b s in
  add prelude;
  add "\n";
  add (C_types.type_defs d g p fs);
  List.iter (fun f -> add (C_func.signature d f ^ ";\n")) fs;
  add (C_closures.closures d g);
  List.iter (C_func.func d g b) fs;
  (* 入口是 runtime/sinter.h 的 sn_main、sn_batch *)
  (match batch with
  | None -> add "int main(int argc, char **argv) { return sn_main(argc, argv, u_main); }\n"
  | Some names ->
      let rec seqs n = if n = 0 then [ "" ] else "" :: List.concat_map (fun s -> [ "1" ^ s; "0" ^ s ]) (seqs (n - 1)) in
      let seqs = List.sort_uniq compare (seqs 3) in
      let strs l = String.concat ", " (List.map (Printf.sprintf "\"%s\"") l) in
      add (Printf.sprintf "static const char *const sn_seqs[] = { %s };\n" (strs seqs));
      List.iter (fun n -> add (Printf.sprintf "static void t_%s(void) { (void)u_%s(); }\n" n n)) names;
      add (Printf.sprintf "static const sn_test sn_tests[] = { %s };\n" (String.concat ", " (List.map (( ^ ) "t_") names)));
      add (Printf.sprintf "static const char *const sn_names[] = { %s };\n" (strs names));
      add
        (Printf.sprintf "int main(void) { return sn_batch(sn_tests, sn_names, %d, sn_seqs, %d); }\n" (List.length names)
           (List.length seqs)));
  Buffer.contents b
