(* sinterc --batch：和 Lean 对照（diff.sh 用）。
   名字形如 p123_y / p123_n 的函数逐个检查，y / n 是 Lean（acceptsG）的结论。
   要求 Lean 接受的 sinterc 都接受；sinterc 接受的生成 C，运行时检查（附录 F 的 `Checking.Validate`）。
   先打印计数，再输出 C；有 Lean 接受而 sinterc 拒绝的、或者 sinterc 内部错误时退出码是 1。 *)
open Sinter

let lean_says name =
  let n = String.length name in
  if n > 3 && name.[0] = 'p' && name.[n - 2] = '_' then Some (name.[n - 1] = 'y') else None

let run ~out file (prog : Resolved.t) =
  let env0 = try Typeck.prepare prog with Diag.Error e -> Report.fail file e in
  let d = Typeck.defs in
  (* 类型检查的错误是异常，后面几步的错误是结果；这里统一成异常，下面按 Lean 的结论分类 *)
  let compile it =
    List.map
      (fun fd -> match Mir_check.func d fd with Ok f -> f | Error e -> raise (Diag.Error e))
      (Typeck.check_batch env0 it)
  in
  let prog = Typeck.batch_items prog.Resolved.items in
  let both = ref 0 and neither = ref 0 and only_lean = ref [] and only_sn = ref 0
  and internal = ref [] in
  let fs =
    List.concat_map
      (fun it ->
        match it with
        | Ast.Fn { name; ipos; _ } -> (
            match lean_says name with
            | None -> ( try compile it with Diag.Error e -> Report.fail file e)
            | Some lean -> (
                match compile it with
                | fs ->
                    if lean then incr both else incr only_sn;
                    fs
                | exception Diag.Error e ->
                    if lean then
                      only_lean := Printf.sprintf "%s（%d:%d: %s）" name e.pos.line e.pos.col (Diag.chinese e.message) :: !only_lean
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
  let fs = List.filter (fun (f : Mir.func) -> not (Typeck.is_generic f.name)) fs in
  let text = Emit_c.program ~batch:names d (Typeck.generated ()) (Typeck.type_names prog) fs in
  (match out with
  | None -> print_string text
  | Some o -> Cc.write o text);
  exit (if !only_lean = [] && !internal = [] then 0 else 1)
