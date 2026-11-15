(* test/messages.t 用：检查 src/base/messages.ml 的英文表和代码里的报错对得上。
   用法：check_messages.exe 源文件…（不要传 messages.ml 本身）
   - 表里的每个键都要是这些文件里的某个字符串字面量。找不到说明改了代码里的中文却没改表，那条信息的英文就没了。
   - 列出报错调用（err、err_at、Diag.error、Diag.message 后面紧跟的格式串）里没有英文的。 *)
open Sinter

let read path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* s.[i] 是左引号：读一个 OCaml 字符串字面量，返回它的值和右引号之后的位置。 *)
let literal s i =
  let n = String.length s and b = Buffer.create 64 in
  let rec go j =
    if j >= n then (Buffer.contents b, n)
    else
      match s.[j] with
      | '"' -> (Buffer.contents b, j + 1)
      | '\\' when j + 1 < n -> (
          match s.[j + 1] with
          | 'n' -> Buffer.add_char b '\n'; go (j + 2)
          | 't' -> Buffer.add_char b '\t'; go (j + 2)
          | 'r' -> Buffer.add_char b '\r'; go (j + 2)
          | 'b' -> Buffer.add_char b '\b'; go (j + 2)
          | '\n' ->
              (* 续行：下一行开头的空白不算 *)
              let k = ref (j + 2) in
              while !k < n && (s.[!k] = ' ' || s.[!k] = '\t') do incr k done;
              go !k
          | '0' .. '9' when j + 3 < n ->
              Buffer.add_char b (Char.chr (int_of_string (String.sub s (j + 1) 3)));
              go (j + 4)
          | 'x' when j + 3 < n ->
              Buffer.add_char b (Char.chr (int_of_string ("0x" ^ String.sub s (j + 2) 2)));
              go (j + 4)
          | c -> Buffer.add_char b c; go (j + 2))
      | c -> Buffer.add_char b c; go (j + 1)
  in
  go (i + 1)

(* 字符字面量（'"'、'\\'、'\n'）里的引号不是字符串的开头：返回字符字面量之后的位置，不是字符字面量时返回 None。 *)
let char_literal s i =
  let n = String.length s in
  if i + 2 < n && s.[i + 1] <> '\\' && s.[i + 2] = '\'' then Some (i + 3)
  else if i + 3 < n && s.[i + 1] = '\\' && s.[i + 3] = '\'' then Some (i + 4)
  else None

(* 文件里所有的字符串字面量 *)
let literals s =
  let n = String.length s and acc = ref [] and i = ref 0 in
  while !i < n do
    match s.[!i] with
    | '"' ->
        let v, j = literal s !i in
        acc := v :: !acc;
        i := j
    | '\'' -> ( match char_literal s !i with Some j -> i := j | None -> incr i)
    | _ -> incr i
  done;
  List.rev !acc

let callers = [ "err "; "err_at "; "Diag.error "; "Diag.message " ]

let ident c = match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' | '.' -> true | _ -> false

let starts_at s i lit = i + String.length lit <= String.length s && String.sub s i (String.length lit) = lit

(* 调用名后面到格式串之前只能是位置参数（x.pos、(p $startpos) 这样），最多换一次行；
   碰到别的字符（例如定义里的 =）就不是报错调用。 *)
let format_after s j =
  let n = String.length s in
  let rec go j lines =
    if j >= n then None
    else
      match s.[j] with
      | '"' -> Some (fst (literal s j))
      | '\n' -> if lines = 0 then go (j + 1) 1 else None
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '\'' | ' ' | '(' | ')' | '$' -> go (j + 1) lines
      | _ -> None
  in
  go j 0

(* 文件里报错调用的格式串，按出现的顺序 *)
let calls s =
  let n = String.length s and acc = ref [] in
  for i = 0 to n - 1 do
    if i = 0 || not (ident s.[i - 1]) then
      List.iter
        (fun c -> if starts_at s i c then Option.iter (fun f -> acc := f :: !acc) (format_after s (i + String.length c)))
        callers
  done;
  List.rev !acc

let () =
  let texts = List.map read (List.tl (Array.to_list Sys.argv)) in
  let seen = Hashtbl.create 1024 in
  List.iter (fun s -> List.iter (fun l -> Hashtbl.replace seen l ()) (literals s)) texts;
  let keys = List.map fst Messages.table in
  let stale = List.filter (fun k -> not (Hashtbl.mem seen k)) keys in
  let missing = ref [] in
  List.iter
    (fun f -> if not (Hashtbl.mem Messages.english f || List.mem f !missing) then missing := f :: !missing)
    (List.concat_map calls texts);
  let missing = List.rev !missing in
  if stale = [] then Printf.printf "英文表 %d 条，键都能在代码里找到\n" (List.length keys)
  else begin
    Printf.printf "英文表 %d 条，%d 个键在代码里找不到：\n" (List.length keys) (List.length stale);
    List.iter (Printf.printf "  %s\n") stale
  end;
  Printf.printf "没有英文的报错 %d 条：\n" (List.length missing);
  List.iter (Printf.printf "  %s\n") missing
