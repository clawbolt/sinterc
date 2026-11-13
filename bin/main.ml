(* 用法：
     sinterc run 源文件.sn [参数…]    编译并运行，后面的参数交给程序（args()），退出码就是程序的退出码
     sinterc build 源文件.sn [-o 程序]  编译成可执行文件（默认和源文件同名，去掉 .sn）
     sinterc 源文件.sn [-o 输出.c]     只生成 C（不给 -o 时写到标准输出）
     sinterc --version / --help
   build 和 run 调用系统的 C 编译器：环境变量 CC，默认 cc。
   --lang=en（或环境变量 SINTER_LANG=en）：错误信息和运行时的 panic 用英文，默认中文。
   开发用的选项：--emit=core 生成给 Lean 复核的 CFG（附录 F 的 `Checking.Validate`）；
   --unchecked 只和 --emit=core 一起用：跳过所有权与投影检查、区域的先后和 R3 检查，照样导出，看 Lean 是否独立地拒绝；
   --batch：对照测试，见下面。
   出错时打印“文件:行:列: 错误：…”和出错的那一行，退出码 1。 *)
open Sinter

let version = "0.2.0-pre"

(* 报错时显示出错的那一行，下一行用 ^ 标出列：
     ok.sn:4:11: 错误：s 已被移动，不能再使用
       4 |     print(s.0)
         |           ^
   源码按文件读一次缓存起来；标准库的模块（std/m.sn）用编进编译器的源码。读不到或行号超出时只打印第一行。
   列号按字节数，^ 前的空白按显示宽度算：ASCII 占 1 格，其他字符（中文等）占 2 格，制表符照抄。 *)
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
let whole_program msg =
  List.exists
    (fun p -> String.length msg >= String.length p && String.sub msg 0 (String.length p) = p)
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

(* 位置可能在引入的模块里：换回那个文件的名字和行号。 *)
let report file (pos : Ast.pos) msg =
  let msg = unprefix_core msg in
  let file, line = Ast.locate file pos in
  Printf.eprintf "%s:%d:%d: %s%s\n" file line pos.col (I18n.pick "错误：" "error: ") (I18n.msg msg);
  if not (whole_program msg) then snippet file line pos.col

let fail file pos msg =
  report file pos msg;
  exit 1

(* 一次报多个错误：按位置排序（行号里含文件的序号，所以先按文件），最多显示 20 个。 *)
let max_errors = 20

let fail_many file errs =
  let errs = List.sort_uniq (fun ((p : Ast.pos), m) ((q : Ast.pos), n) -> compare (p.line, p.col, m) (q.line, q.col, n)) errs in
  List.iteri (fun i (pos, msg) -> if i < max_errors then report file pos msg) errs;
  let n = List.length errs in
  if n > max_errors then
    prerr_endline
      (I18n.pick
         (Printf.sprintf "（还有 %d 个错误没有显示）" (n - max_errors))
         (Printf.sprintf "(%d more errors not shown)" (n - max_errors)));
  exit 1

let help () =
  I18n.pick
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

(* 把生成的 C 交给系统的 C 编译器。release：-O2，并定义 SN_RELEASE（不统计分配次数、不做退出时的泄漏检查）。 *)
let cc ~release c_file exe =
  let cc = Option.value (Sys.getenv_opt "CC") ~default:"cc" in
  let opt = if release then "-O2 -DSN_RELEASE" else "-O1" in
  let cmd = Printf.sprintf "%s -std=gnu11 %s -w %s -o %s -lm -pthread" cc opt (Filename.quote c_file) (Filename.quote exe) in
  if Sys.command cmd <> 0 then begin
    prerr_endline
      (I18n.pick
         (Printf.sprintf "错误：C 编译器（%s）没有成功，可以用环境变量 CC 换一个（需要 GCC 或 Clang）" cc)
         (Printf.sprintf "error: the C compiler (%s) failed; set CC to another one (GCC or Clang is required)" cc));
    exit 1
  end

(* sinterc fmt [--check | -w] 文件…：默认把格式化的结果打印出来；-w 写回文件；
   --check 只列出需要格式化的文件，有的话退出码是 1。 *)
let fmt_main argv =
  let check = List.mem "--check" argv and write = List.mem "-w" argv in
  let files = List.filter (fun a -> a <> "--check" && a <> "-w") argv in
  if files = [] || (check && write) || List.exists (fun f -> String.length f > 0 && f.[0] = '-') files then usage ();
  let changed = ref 0 in
  List.iter
    (fun f ->
      let src =
        try
          let ic = open_in_bin f in
          let s = really_input_string ic (in_channel_length ic) in
          close_in ic;
          s
        with Sys_error _ ->
          prerr_endline (I18n.pick ("错误：打不开文件 " ^ f) ("error: cannot open file " ^ f));
          exit 1
      in
      if not (Fmt.check src) then begin
        prerr_endline
          (I18n.pick
             ("内部错误：格式化 " ^ f ^ " 会改变记号或者不稳定，没有改动文件")
             ("internal error: formatting " ^ f ^ " would change its tokens or is not stable; the file was left unchanged"));
        exit 3
      end;
      let out = Fmt.format src in
      if check then begin
        if out <> src then begin
          incr changed;
          print_endline f
        end
      end
      else if write then begin
        if out <> src then begin
          let oc = open_out_bin f in
          output_string oc out;
          close_out oc
        end
      end
      else print_string out)
    files;
  if check && !changed > 0 then exit 1;
  exit 0

type mode = Emit | Build | Run

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
    match Hashtbl.find_opt Typeck.packs n with
    | Some ((c : Typeck.closure), _) -> Option.iter visit (Hashtbl.find_opt by c.cname)
    | None -> ()
  in
  List.iter (fun (f, from_core) -> if not from_core then visit f) fs;
  List.filter_map (fun ((f : Mir.func), _) -> if Hashtbl.mem seen f.name then Some f else None) fs

let () =
  let file = ref None and out = ref None and core = ref false and unchecked = ref false
  and batch = ref false and release = ref false in
  (match Sys.getenv_opt "SINTER_LANG" with
  | Some ("en" | "EN") -> I18n.lang := I18n.En
  | _ -> ());
  let argv =
    List.filter
      (fun a ->
        match a with
        | "--lang=en" -> I18n.lang := I18n.En; false
        | "--lang=zh" -> I18n.lang := I18n.Zh; false
        | _ -> true)
      (List.tl (Array.to_list Sys.argv))
  in
  let mode, argv =
    match argv with
    | "run" :: rest -> (Run, rest)
    | "build" :: rest -> (Build, rest)
    | "fmt" :: rest -> fmt_main rest
    | ("--version" | "-v") :: _ -> print_endline ("sinterc " ^ version); exit 0
    | ("--help" | "-h") :: _ -> print_string (help ()); exit 0
    | _ -> (Emit, argv)
  in
  (* run：源文件后面的都是给程序的参数（args() 读到的） *)
  let prog_args = ref [] in
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
  (* 读主文件和它用 use 引入的模块，合成一个程序 *)
  let prog =
    try Modules.load file with
    | Sys_error _ ->
        prerr_endline (I18n.pick ("错误：打不开文件 " ^ file) ("error: cannot open file " ^ file));
        exit 1
    | Lexer.Error (pos, msg) | Ast.Syntax (pos, msg) -> fail file pos msg
  in
  if !batch then begin
    (* 对照测试：名字形如 p123_y / p123_n 的函数逐个检查，y / n 是 Lean（acceptsG）的结论。
       要求 Lean 接受的 sinterc 都接受；sinterc 接受的生成 C，运行时检查（附录 F 的 `Checking.Validate`）。 *)
    let env0 = try Typeck.prepare prog with Typeck.Error (pos, msg) -> fail file pos msg in
    let lean_says name =
      let n = String.length name in
      if n > 3 && name.[0] = 'p' && name.[n - 2] = '_' then Some (name.[n - 1] = 'y') else None
    in
    let compile it =
      List.concat_map
        (fun it -> List.map (fun f -> Borrow.check (Own.elaborate f)) (Mir.lower [ it ]))
        (Typeck.check_batch env0 it)
    in
    let prog = Typeck.batch_items prog in
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
    let fs = List.filter (fun (f : Mir.func) -> not (Typeck.is_generic f.name)) fs in
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
     泛型函数本身也降级并检查（定义处检查，Lean `Generics.Check`），但只输出它的实例。
     类型检查有错误时报出所有函数的类型错误就停下；都通过时，每个函数各自降级、检查，报出所有函数的所有权错误 *)
  let prog, fs =
    try
      Typeck.lax_regions := !unchecked;
      let prog = Typeck.program prog in
      let check = not !unchecked in
      let errs = ref [] in
      let fs =
        List.concat_map
          (fun it ->
            let from_core = match it with Ast.Fn { ipos; _ } -> Modules.in_core ipos | _ -> false in
            try
              List.map
                (fun f ->
                  let f = Own.elaborate ~check f in
                  ((if check then Borrow.check f else f), from_core))
                (Mir.lower [ it ])
            with Typeck.Error (pos, msg) ->
              errs := (pos, msg) :: !errs;
              [])
          prog
      in
      if !errs <> [] then fail_many file !errs;
      (prog, List.filter (fun (f : Mir.func) -> not (Typeck.is_generic f.name)) (used_core fs))
    with
    | Typeck.Error (pos, msg) -> fail file pos msg
    | Typeck.Errors errs -> fail_many file errs
  in
  let text = if !core then Emit_core.program ~expect_ok:(not !unchecked) fs else I18n.runtime (Emit_c.program prog fs) in
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
      cc ~release:!release c exe;
      Sys.remove c
  | Run, _ ->
      let c = Filename.temp_file "sinter" ".c" and exe = Filename.temp_file "sinter" ".exe" in
      write c;
      cc ~release:!release c exe;
      Sys.remove c;
      flush_all ();
      let code = Sys.command (String.concat " " (List.map Filename.quote (exe :: !prog_args))) in
      Sys.remove exe;
      exit code
