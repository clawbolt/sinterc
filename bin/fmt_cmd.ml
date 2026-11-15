(* sinterc fmt [--check | -w] 文件…：默认把格式化的结果打印出来；-w 写回文件；
   --check 只列出需要格式化的文件，有的话退出码是 1。格式化本身见 src/fmt.ml。 *)
open Sinter

let main argv =
  let check = List.mem "--check" argv and write = List.mem "-w" argv in
  let files = List.filter (fun a -> a <> "--check" && a <> "-w") argv in
  if files = [] || (check && write) || List.exists (fun f -> String.length f > 0 && f.[0] = '-') files then Cli.usage ();
  let changed = ref 0 in
  List.iter
    (fun f ->
      let src =
        try
          let ic = open_in_bin f in
          let s = really_input_string ic (in_channel_length ic) in
          close_in ic;
          s
        with Sys_error _ -> Report.cannot_open f
      in
      if not (Fmt.check src) then begin
        prerr_endline
          (Messages.pick
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
        if out <> src then Cc.write f out
      end
      else print_string out)
    files;
  if check && !changed > 0 then exit 1;
  exit 0
