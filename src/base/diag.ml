(* 诊断：编译器报给用户的错误。词法、语法、模块、类型检查、降级、所有权和投影检查都抛这里的异常，
   由 bin/main.ml 统一渲染。

   信息的键就是代码里写的中文格式串（例如 "%s 已被移动，不能再使用"），参数单独记下。
   中文按格式串填回参数；英文由 messages.ml 按键查表、按编号填参数，不需要从整句中文里猜参数在哪。 *)

type message = { key : string; args : string list }
type t = { pos : Source.pos; message : message }

exception Error of t
exception Errors of t list

(* 参数的两头加上标记，渲染一次就能按标记取回每个参数。格式里只有 %s、%d、%c 三种转换。 *)
let open_mark = '\001'
let close_mark = '\002'

let is_conversion c = c = 's' || c = 'd' || c = 'c'

let mark key =
  let n = String.length key and b = Buffer.create (String.length key + 16) and i = ref 0 in
  while !i < n do
    if key.[!i] = '%' && !i + 1 < n then begin
      let c = key.[!i + 1] in
      if is_conversion c then begin
        Buffer.add_char b open_mark;
        Buffer.add_char b '%';
        Buffer.add_char b c;
        Buffer.add_char b close_mark
      end
      else begin
        Buffer.add_char b '%';
        Buffer.add_char b c
      end;
      i := !i + 2
    end
    else begin
      Buffer.add_char b key.[!i];
      incr i
    end
  done;
  Buffer.contents b

let args_of s =
  let rec go i acc =
    match String.index_from_opt s i open_mark with
    | None -> List.rev acc
    | Some j ->
        let k = String.index_from s j close_mark in
        go (k + 1) (String.sub s (j + 1) (k - j - 1) :: acc)
  in
  go 0 []

let capture k fmt =
  let key = string_of_format fmt in
  Printf.ksprintf (fun s -> k { key; args = args_of s }) (Scanf.format_from_string (mark key) fmt)

let message fmt = capture Fun.id fmt
let error pos fmt = capture (fun message -> raise (Error { pos; message })) fmt
let raise_at pos message = raise (Error { pos; message })

let fill key args =
  let n = String.length key and b = Buffer.create 64 and i = ref 0 and rest = ref args in
  while !i < n do
    if key.[!i] = '%' && !i + 1 < n then begin
      if is_conversion key.[!i + 1] then begin
        match !rest with
        | a :: more ->
            Buffer.add_string b a;
            rest := more
        | [] -> ()
      end
      else Buffer.add_char b key.[!i + 1];
      i := !i + 2
    end
    else begin
      Buffer.add_char b key.[!i];
      incr i
    end
  done;
  Buffer.contents b

let chinese m = fill m.key m.args
