(* sinterc fmt：按记号格式化。不改变记号、不改变在哪里换行，只改空白：
   - 每行按没闭合的括号重新缩进（每层 4 个空格；一行里新开的几个括号只算一层；行首的 } ) 先退出）；
   - 行内：( 后面、, ) 前面没有空格，, 后面一个空格；= == != <= >= => -> && || 两边各一个空格；
     { 前面一个空格，{ 后面、} 前面一个空格（{} 例外）；其余记号之间原来有空白就留一个，没有就不加；
   - 行尾注释前面至少一个空格（原来更多时保留，方便对齐）；
   - 去掉行尾空白，连续的空行合成一个（不额外加空行，和 rustfmt、gofmt 一样），去掉文件开头和结尾的空行，
     文件以一个换行结束。
   字符串（包括插值）和字符字面量原样保留。格式化前后的记号序列必须相同（check 检查），
   再格式化一次结果不变。 *)

type tok = { text : string; gap : int (* 前面的空白宽度；行首是 -1 *) }

let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_ident c = is_ident_start c || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'

let utf8_len c =
  let b = Char.code c in
  if b < 0x80 then 1 else if b < 0xE0 then 2 else if b < 0xF0 then 3 else 4

(* 字符串从 i（左引号）开始，返回右引号后面的位置。插值的 { … } 里可以有嵌套的字符串。 *)
let rec scan_str s i =
  let n = String.length s in
  let rec go j =
    if j >= n then n
    else
      match s.[j] with
      | '\\' -> go (j + 2)
      | '"' -> j + 1
      | '{' when j + 1 < n && s.[j + 1] = '{' -> go (j + 2)
      | '{' -> go (hole (j + 1) 0)
      | _ -> go (j + 1)
  and hole j d =
    if j >= n then n
    else
      match s.[j] with
      | '"' -> hole (scan_str s j) d
      | '\'' -> hole (scan_char s j) d
      | '{' -> hole (j + 1) (d + 1)
      | '}' -> if d = 0 then j + 1 else hole (j + 1) (d - 1)
      | _ -> hole (j + 1) d
  in
  go (i + 1)

(* 字符字面量 'a'、'中'、'\n'；不是合法的字符字面量时只取一个 ' *)
and scan_char s i =
  let n = String.length s in
  if i + 3 < n && s.[i + 1] = '\\' && s.[i + 3] = '\'' then i + 4
  else if i + 1 < n then
    let k = i + 1 + utf8_len s.[i + 1] in
    if k < n && s.[k] = '\'' then k + 1 else i + 1
  else i + 1

let ops2 = [ "->"; "=>"; "=="; "!="; "<="; ">="; "&&"; "||"; ".." ]

(* 一行切成记号；// 注释是一个记号（到行尾） *)
let tokens_of_line (s : string) : tok list =
  let n = String.length s in
  let rec go i gap acc =
    if i >= n then List.rev acc
    else
      let c = s.[i] in
      if c = ' ' || c = '\t' || c = '\r' then go (i + 1) (gap + 1) acc
      else
        let j =
          if c = '/' && i + 1 < n && s.[i + 1] = '/' then n
          else if c = '"' then scan_str s i
          else if c = '\'' then scan_char s i
          else if is_ident_start c then begin
            let j = ref i in
            while !j < n && is_ident s.[!j] do incr j done;
            !j
          end
          else if is_digit c then begin
            let j = ref i in
            while !j < n && (is_ident s.[!j] || (s.[!j] = '.' && !j + 1 < n && is_digit s.[!j + 1])) do incr j done;
            !j
          end
          else if i + 1 < n && List.mem (String.sub s i 2) ops2 then i + 2
          else i + utf8_len c
        in
        let j = min j n in
        let text = String.sub s i (j - i) in
        let text = if c = '/' then String.trim text else text in
        go j 0 ({ text; gap = (if acc = [] then -1 else gap) } :: acc)
  in
  go 0 0 []

let spaced = [ "="; "=="; "!="; "<="; ">="; "=>"; "->"; "&&"; "||" ]
let is_comment t = String.length t >= 2 && String.sub t 0 2 = "//"

(* a、b 之间的空白 *)
let between (a : string) (b : tok) =
  let t = b.text in
  if is_comment t then String.make (max 1 b.gap) ' '
  else if t = "," || t = ")" || t = ":" then ""
  else if a = "(" then ""
  else if a = "," || a = ":" then " "
  else if a = "{" && t = "}" then ""
  else if a = "{" || t = "}" || t = "{" then " "
  else if List.mem a spaced || List.mem t spaced then " "
  else if b.gap > 0 then " "
  else ""

let format_line depth (ts : tok list) =
  match ts with
  | [] -> ""
  | first :: rest ->
      let b = Buffer.create 80 in
      Buffer.add_string b (String.make (4 * depth) ' ');
      Buffer.add_string b first.text;
      let prev = ref first.text in
      List.iter
        (fun t ->
          Buffer.add_string b (between !prev t);
          Buffer.add_string b t.text;
          prev := t.text)
        rest;
      Buffer.contents b

let split_lines s =
  let ls = String.split_on_char '\n' s in
  (* 末尾的换行不算多出一行 *)
  match List.rev ls with "" :: r -> List.rev r | _ -> ls

(* 缩进：没闭合的括号放在一个栈里。一行里新开、到行尾还没闭合的括号只算一层（最外面那个），
   所以 f(v, |x| { 换行 … 换行 }) 里的闭包体只缩进一层。行首连续的 } ) 先出栈，再算这一行的缩进。 *)
let format (src : string) : string =
  let lines = List.map tokens_of_line (split_lines src) in
  let stack : int ref list ref = ref [] in
  let depth () = List.fold_left (fun a w -> a + !w) 0 !stack in
  let pop () = match !stack with _ :: r -> stack := r | [] -> () in
  let out = Buffer.create (String.length src) in
  let blank = ref false and started = ref false in
  List.iter
    (fun ts ->
      if ts = [] then (if !started then blank := true)
      else begin
        let is_open t = t.text = "{" || t.text = "(" and is_close t = t.text = "}" || t.text = ")" in
        let rec leading = function t :: r when is_close t -> pop (); leading r | r -> r in
        let rest = leading ts in
        let d = depth () in
        let mine = ref [] in
        List.iter
          (fun t ->
            if is_open t then begin
              let w = ref 0 in
              stack := w :: !stack;
              mine := w :: !mine
            end
            else if is_close t then begin
              (match !stack with w :: _ -> mine := List.filter (fun x -> x != w) !mine | [] -> ());
              pop ()
            end)
          rest;
        (* !mine 里最后一个是这一行最先打开的 *)
        (match List.rev !mine with w :: _ -> w := 1 | [] -> ());
        if !blank then Buffer.add_char out '\n';
        blank := false;
        started := true;
        Buffer.add_string out (format_line d ts);
        Buffer.add_char out '\n'
      end)
    lines;
  Buffer.contents out

(* 只看记号（不看空白）：格式化前后必须一样 *)
let token_texts s = List.concat_map (fun ts -> List.map (fun t -> t.text) ts) (List.map tokens_of_line (split_lines s))

let check src = token_texts (format src) = token_texts src && format (format src) = format src
