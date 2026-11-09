(* 把标准库的源码 std/*.sn 编进编译器（C35）：生成 stdlib_src.ml，里面是 (模块名, 源码) 的表。
   用法：embed 文件…，结果写到标准输出。 *)
let read path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  let files = List.sort compare (List.tl (Array.to_list Sys.argv)) in
  print_endline "(* 由 gen/embed.ml 从 std/*.sn 生成，不要手工修改。 *)";
  print_endline "let files = [";
  List.iter
    (fun path ->
      let name = Filename.remove_extension (Filename.basename path) in
      Printf.printf "  (%S, %S);\n" name (read path))
    files;
  print_endline "]"
