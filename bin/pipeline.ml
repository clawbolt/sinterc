(* 编译一个程序：读入、类型检查、降级、所有权与投影检查，最后生成 C 或给 Lean 的 CFG。
   每一步出错都在这里报出并退出（退出码 1）。 *)
open Sinter

(* 读主文件和它用 use 引入的模块，合成一个程序 *)
let load file =
  try Resolve.load file with
  | Sys_error _ -> Report.cannot_open file
  | Diag.Error e -> Report.fail file e

(* 核心部分（预导入）的函数都经过了检查，但只输出用到的：从其他函数出发沿着调用（包括闭包包装里的函数）能走到的。
   其他函数照旧全部输出。 *)
let used_core (fs : (Mir.func * bool) list) =
  let by = Hashtbl.create 64 and seen = Hashtbl.create 64 in
  List.iter (fun ((f : Mir.func), _) -> Hashtbl.replace by f.name f) fs;
  let rec visit (f : Mir.func) =
    if not (Hashtbl.mem seen f.name) then begin
      Hashtbl.replace seen f.name ();
      Array.iter
        (fun (b : Mir.block) ->
          List.iter (function Mir.Assign (_, Mir.Call (n, _)) | Mir.Eval (Mir.Call (n, _)) -> callee n | _ -> ()) b.stmts)
        f.blocks
    end
  and callee n =
    Option.iter visit (Hashtbl.find_opt by n);
    match Hashtbl.find_opt Check.packs n with
    | Some ((c : Check.closure), _) -> Option.iter visit (Hashtbl.find_opt by c.cname)
    | None -> ()
  in
  List.iter (fun (f, from_core) -> if not from_core then visit f) fs;
  List.filter_map (fun ((f : Mir.func), _) -> if Hashtbl.mem seen f.name then Some f else None) fs

(* 类型检查、降级、所有权检查、投影与排他性检查都可能报错。
   泛型函数本身也降级并检查（定义处检查，Lean `Generics.Check`），但只输出它的实例。
   类型检查有错误时报出所有函数的类型错误就停下；都通过时，每个函数各自降级、检查，报出所有函数的所有权错误。
   unchecked：跳过所有权与投影检查、区域的先后和 R3 检查（只给 --emit=core 用）。 *)
let check ~unchecked file prog =
  try
    Check.lax_regions := unchecked;
    let prog = Check.program prog in
    let check = not unchecked in
    let d = Check.defs in
    let errs = ref [] in
    let fs =
      List.concat_map
        (fun (fd : Tast.fn_def) ->
          match Mir_check.func ~check d fd with
          | Ok f -> [ (f, fd.from_core) ]
          | Error e ->
              errs := e :: !errs;
              [])
        prog.Tast.fns
    in
    if !errs <> [] then Report.fail_many file !errs;
    (prog, List.filter (fun (f : Mir.func) -> not (Check.is_generic f.name)) (used_core fs))
  with
  | Diag.Error e -> Report.fail file e
  | Diag.Errors errs -> Report.fail_many file errs

(* 生成的文字：给 Lean 的 CFG，或者 C（运行时的信息按 --lang 换成英文） *)
let emit ~core ~unchecked (prog : Tast.program) fs =
  let d = Check.defs in
  if core then Emit_core.program ~expect_ok:(not unchecked) d fs
  else Messages.runtime (Emit_c.program d (Check.generated ()) prog.Tast.types fs)
