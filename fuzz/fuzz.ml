(* 随机测试（T5）：sinterc fmt 和整个编译器。用法：fuzz 根目录 轮数 种子。
   1. fmt，空白：真实的源文件（示例、标准库）随机改空白——记号之间的空格数、缩进、行尾空白、空行数，
      但不改“有没有空白”和注释前面的空白——格式化的结果必须和原文件格式化的结果完全一样（fmt 只看记号和换行）；
   2. fmt，随机记号行：从真实文件里取出的完整记号随机拼成若干行（随机的括号、注释、字符串），
      Fmt.check 必须成立：格式化前后记号相同，再格式化一次不变；
   3. 编译：示例程序随机删掉、复制、交换或替换一个记号，走完整个编译器（解析、类型检查、降级、所有权、投影与排他性、
      生成 C）。报出语法、类型、所有权错误都可以；抛出别的异常（Not_found、Failure、断言……）就是编译器的错误。
   不一致的输入写到 根目录/_batch/fuzz/ 下。种子相同时结果相同。 *)
open Sinter

let read path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let files_in dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir |> Array.to_list |> List.filter (fun f -> Filename.check_suffix f ".sn") |> List.sort compare
    |> List.map (Filename.concat dir)
  else []

let pick a = a.(Random.int (Array.length a))

(* ---- 1. 空白 ---- *)

let spaces () = String.init (1 + Random.int 3) (fun _ -> if Random.int 4 = 0 then '\t' else ' ')

let perturb src =
  let b = Buffer.create (String.length src * 2) in
  if Random.bool () then Buffer.add_string b "\n\n";
  List.iter
    (fun line ->
      match Fmt.tokens_of_line line with
      | [] ->
          (* 空行：变成 1 到 3 个空行，可以带空白 *)
          for _ = 0 to Random.int 3 do
            if Random.bool () then Buffer.add_string b (spaces ());
            Buffer.add_char b '\n'
          done
      | ts ->
          Buffer.add_string b (String.make (Random.int 13) ' ');
          List.iter
            (fun (t : Fmt.tok) ->
              if t.gap > 0 then
                Buffer.add_string b (if Fmt.is_comment t.text then String.make t.gap ' ' else spaces ());
              Buffer.add_string b t.text)
            ts;
          if Random.int 3 = 0 then Buffer.add_string b (spaces ());
          Buffer.add_char b '\n')
    (Fmt.split_lines src);
  if Random.bool () then Buffer.add_string b "\n \n";
  Buffer.contents b

(* ---- 2. 随机记号行 ---- *)

let soup pool =
  let b = Buffer.create 256 in
  for _ = 1 to 1 + Random.int 12 do
    Buffer.add_string b (String.make (Random.int 9) ' ');
    for i = 1 to Random.int 10 do
      let t = pick pool in
      if not (Fmt.is_comment t) then begin
        if i > 1 && Random.int 3 > 0 then Buffer.add_string b (spaces ());
        Buffer.add_string b t
      end
    done;
    if Random.int 6 = 0 then Buffer.add_string b " // 注释";
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

(* ---- 3. 编译 ---- *)

(* 一个记号：要么是一行，要么是行里的一个记号（保留缩进和原来的空白） *)
let mutate pool src =
  let lines = Array.of_list (List.map Fmt.tokens_of_line (Fmt.split_lines src)) in
  let nonempty = List.filter (fun i -> lines.(i) <> []) (List.init (Array.length lines) Fun.id) in
  if nonempty <> [] then begin
    let i = List.nth nonempty (Random.int (List.length nonempty)) in
    let ts = Array.of_list lines.(i) in
    let k = Random.int (Array.length ts) in
    let tok text gap = { Fmt.text; gap } in
    let l = Array.to_list ts in
    let replaced =
      match Random.int 4 with
      | 0 -> List.filteri (fun j _ -> j <> k) l
      | 1 -> List.concat (List.mapi (fun j (t : Fmt.tok) -> if j = k then [ t; { t with gap = 1 } ] else [ t ]) l)
      | 2 when k + 1 < Array.length ts ->
          List.mapi
            (fun j (t : Fmt.tok) ->
              if j = k then { (ts.(k + 1)) with gap = t.gap } else if j = k + 1 then { (ts.(k)) with gap = t.gap } else t)
            l
      | _ -> List.mapi (fun j (t : Fmt.tok) -> if j = k then tok (pick pool) (max t.gap 1) else t) l
    in
    lines.(i) <- replaced
  end;
  let line ts =
    String.concat ""
      (List.mapi (fun j (t : Fmt.tok) -> (if j = 0 then "" else if t.gap > 0 then " " else "") ^ t.text) ts)
  in
  String.concat "\n" (Array.to_list (Array.map line lines)) ^ "\n"

type outcome = Accepted | Rejected | Internal of string

let compile path =
  try
    let prog = Modules.load path in
    let prog = Typeck.program prog in
    let fs = List.concat_map (fun it -> List.map (fun f -> Borrow.check (Own.elaborate f)) (Mir.lower [ it ])) prog in
    let fs = List.filter (fun (f : Mir.func) -> not (Typeck.is_generic f.name)) fs in
    ignore (Emit_c.program prog fs);
    Accepted
  with
  | Lexer.Error _ | Ast.Syntax _ | Typeck.Error _ | Typeck.Errors _ -> Rejected
  | Stack_overflow -> Internal "Stack_overflow"
  | e -> Internal (Printexc.to_string e)

let () =
  let root = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let rounds = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 20 in
  let seed = if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3) else 1 in
  Random.init seed;
  let out = Filename.concat root "_batch/fuzz" in
  (try Sys.mkdir (Filename.concat root "_batch") 0o755 with Sys_error _ -> ());
  (try Sys.mkdir out 0o755 with Sys_error _ -> ());
  let progs = files_in (Filename.concat root "examples/ok") @ files_in (Filename.concat root "examples/tour") in
  let sources = progs @ files_in (Filename.concat root "examples/bad") @ files_in (Filename.concat root "std") in
  let texts = List.map (fun f -> (f, read f)) sources in
  (* 记号池：所有文件里的完整记号（不要单独的引号，它们不是记号） *)
  let pool =
    List.concat_map
      (fun (_, s) -> List.concat_map (fun l -> List.map (fun (t : Fmt.tok) -> t.text) (Fmt.tokens_of_line l)) (Fmt.split_lines s))
      texts
    |> List.filter (fun t -> t <> "'" && t <> "\"")
    |> List.sort_uniq compare |> Array.of_list
  in
  let bad = ref 0 in
  let save kind i s =
    incr bad;
    let p = Filename.concat out (Printf.sprintf "%s_%d.sn" kind i) in
    write p s;
    Printf.printf "  不一致：%s\n" p
  in
  (* 1 *)
  let n1 = ref 0 in
  List.iter
    (fun (f, s) ->
      let want = Fmt.format s in
      for _ = 1 to rounds do
        incr n1;
        let p = perturb s in
        if Fmt.format p <> want || not (Fmt.check p) then save ("ws_" ^ Filename.remove_extension (Filename.basename f)) !n1 p
      done)
    texts;
  Printf.printf "种子 %d\nfmt 空白：%d 个文件 × %d 次，共 %d 个\n%!" seed (List.length texts) rounds !n1;
  (* 2 *)
  let n2 = rounds * 200 in
  for i = 1 to n2 do
    let s = soup pool in
    if not (Fmt.check s) then save "soup" i s
  done;
  Printf.printf "fmt 随机记号行：%d 段\n%!" n2;
  (* 3 *)
  let n3 = ref 0 and acc = ref 0 and rej = ref 0 and internal = ref 0 in
  List.iter
    (fun f ->
      let s = read f in
      let tmp = Filename.concat (Filename.dirname f) ".fuzz_tmp.sn" in
      for _ = 1 to rounds do
        incr n3;
        let m = mutate pool s in
        write tmp m;
        (match compile tmp with
        | Accepted -> incr acc
        | Rejected -> incr rej
        | Internal e ->
            incr internal;
            Printf.printf "  内部错误：%s\n" e;
            save ("compile_" ^ Filename.remove_extension (Filename.basename f)) !n3 m);
        (try Sys.remove tmp with Sys_error _ -> ())
      done)
    progs;
  Printf.printf "编译：%d 个变体，接受 %d，报错 %d，内部错误 %d\n" !n3 !acc !rej !internal;
  if !bad = 0 then print_endline "随机测试全部通过" else (Printf.printf "有 %d 个不一致\n" !bad; exit 1)
