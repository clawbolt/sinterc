(* 把程序改写成新的标准库的写法，分两步。第一步（--modules）是标准库重新划分：
   - 旧模块的函数换成方法：str.trim(s) → s.trim()，vec.sort(v) → v.sort()，hashmap.insert(m, k, v) → m.insert(k, v)；
     换了地方的换成新模块的函数：os.exists(p) → fs.exists(p)，view.line_reader() → io.lines()；
   - 打印：print → println，print_str → print，eprint → eprintln，eprint_str → eprint，
     print_i64(x)、print_bool、print_f64、print_char → println("{x}")；
   - use 行跟着改：去掉不再存在、不再用到的模块，加上新用到的。
   按字符扫描，注释原样保留，字符串插值里的代码照样改写；调用的参数按括号配对切开，先改写参数里面的。
   接收者不是一个名字、字面量或调用链时加括号：math.abs(a - b) → (a - b).abs()。
   .sn 整个文件是一个程序；.t 里改每个 heredoc（cat > x.sn <<'EOF'）和错误信息里引用的源码行；.md 里改 ``` 代码块（sh 的除外）。
   默认是第二步：标准库以外不能直接调用内建函数，vec_len(v) → v.len()、str_from_i64(n) → n.to_str()、vec_get(v, i) → v[i]，
   读写外部的换成 std.io、std.fs、std.env、std.process 的函数（read_line() → io.read_line()），缺的 use 行补上。
   程序自己定义了同名函数时不改。每一步只能跑一次：--modules 改过的 print 再改一次就变成了 println。
   --types 是第三步，构造换成类型函数：vec_new() → Vec.new()，bytes_new() → Bytes.new()，map.new() → map.Map.new()，
   hashmap.new() → hashmap.HashMap.new()，bigint.zero()、from_i64、parse → bigint.BigInt.zero() 等。这一步可以重复跑，标准库里也能用。
   用法：migrate [-w] [--modules | --types] 文件…。不带 -w 时打印结果，带 -w 时写回并列出改了的文件。 *)

let is_id_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_id c = is_id_start c || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'

let id_end s i =
  let n = String.length s in
  let j = ref i in
  while !j < n && is_id s.[!j] do incr j done;
  !j

let line_end s i = match String.index_from_opt s i '\n' with Some j -> j | None -> String.length s

(* 从 i（左引号）开始的字符串之后的位置；插值 { … } 里可以有嵌套的字符串 *)
let rec skip_str s i =
  let n = String.length s in
  let rec go j =
    if j >= n then n
    else
      match s.[j] with
      | '\\' -> go (j + 2)
      | '"' -> j + 1
      | '{' when j + 1 < n && s.[j + 1] = '{' -> go (j + 2)
      | '{' -> go (skip_hole s (j + 1))
      | _ -> go (j + 1)
  in
  go (i + 1)

(* 插值里从 i 开始，返回对应的 } 后面的位置 *)
and skip_hole s i =
  let n = String.length s in
  let rec go j d =
    if j >= n then n
    else
      match s.[j] with
      | '"' -> go (skip_str s j) d
      | '\'' -> go (skip_char s j) d
      | '{' -> go (j + 1) (d + 1)
      | '}' -> if d = 0 then j + 1 else go (j + 1) (d - 1)
      | _ -> go (j + 1) d
  in
  go i 0

(* 字符字面量 'a'、'中'、'\n' 之后的位置；不是字符字面量时只跳过 ' *)
and skip_char s i =
  let n = String.length s in
  if i + 3 < n && s.[i + 1] = '\\' && s.[i + 3] = '\'' then i + 4
  else begin
    let k = ref (i + 1) in
    if !k < n then begin
      incr k;
      while !k < n && Char.code s.[!k] land 0xC0 = 0x80 do incr k done
    end;
    if !k < n && s.[!k] = '\'' then !k + 1 else i + 1
  end

(* s.[l] 是 (：按顶层的逗号切开参数（原样，带空白），返回参数和 ) 后面的位置；括号不配对时是 None。
   闭包 |a, b| 里的逗号也会切开，所以只单独用第一个参数，其余的按原样用逗号接回去。 *)
let split_args s l =
  let n = String.length s in
  let args = ref [] and start = ref (l + 1) in
  let rec go j d =
    if j >= n then None
    else
      match s.[j] with
      | '"' -> go (skip_str s j) d
      | '\'' -> go (skip_char s j) d
      | '/' when j + 1 < n && s.[j + 1] = '/' -> go (line_end s j) d
      | '(' | '[' | '{' -> go (j + 1) (d + 1)
      | ')' | ']' | '}' when d > 0 -> go (j + 1) (d - 1)
      | ')' ->
          args := String.sub s !start (j - !start) :: !args;
          Some (List.rev !args, j + 1)
      | ']' | '}' -> None
      | ',' when d = 0 ->
          args := String.sub s !start (j - !start) :: !args;
          start := j + 1;
          go (j + 1) d
      | _ -> go (j + 1) d
  in
  go (l + 1) 0

(* 能直接接 .f(…) 的表达式：名字、字符串或字符字面量、括号开头，顶层只有名字、. 和配对的 ( ) [ ] *)
let simple t =
  let n = String.length t in
  let rec go j d =
    if j >= n then d = 0
    else
      match t.[j] with
      | '"' -> go (skip_str t j) d
      | '\'' -> go (skip_char t j) d
      | '(' | '[' -> go (j + 1) (d + 1)
      | ')' | ']' -> d > 0 && go (j + 1) (d - 1)
      | _ when d > 0 -> go (j + 1) d
      | c -> (is_id c || c = '.') && go (j + 1) d
  in
  n > 0 && (is_id_start t.[0] || t.[0] = '"' || t.[0] = '\'' || t.[0] = '(') && go 0 0

let recv a =
  let t = String.trim a in
  if simple t then t else "(" ^ t ^ ")"

let is_num t = String.length t > 0 && (is_digit t.[0] || (t.[0] = '-' && String.length t > 1 && is_digit t.[1]))

let lstrip s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && s.[!i] = ' ' do incr i done;
  String.sub s !i (n - !i)

type rule =
  | Method of string            (* M.f(a, …) → a.g(…) *)
  | Swap of string              (* 同 Method；第一个参数是数字时和第二个交换（min、max） *)
  | Global of string            (* M.f(…) → g(…)（预导入的函数） *)
  | Moved of string * string    (* M.f(…) → N.g(…) *)
  | Keep

let rule m f =
  match (m, f) with
  | ("str" | "char" | "option" | "result"), _ -> Method f
  | "vec", "sum_f64" -> Method "sum"
  | "vec", "range" -> Keep
  | "vec", _ -> Method f
  | "math", ("min" | "max") -> Swap f
  | "math", "fmin" -> Swap "min"
  | "math", "fmax" -> Swap "max"
  | "math", "fabs" -> Method "abs"
  | "math", "min_of" -> Global "min"
  | "math", "max_of" -> Global "max"
  | "math", ("pi" | "e") -> Keep
  | "math", _ -> Method f
  | "view", "to_i64" -> Method "parse_i64"
  | "view", "line_reader" -> Moved ("io", "lines")
  | "view", "next_block" -> Moved ("io", "next_block_in")
  | "view", _ -> Method f
  | "os", "env" -> Moved ("env", "get")
  | "os", "exit" -> Moved ("env", "exit")
  | "os", ("run" | "output") -> Moved ("process", f)
  | "os", _ -> Moved ("fs", f)
  | "io", ("read_lines" | "write_lines") -> Moved ("fs", f)
  | "treemap", "new" -> Moved ("map", "new")
  | (("hashmap" | "map"), "new") | ("bigint", ("zero" | "from_i64" | "parse")) -> Keep
  | ("treemap" | "hashmap" | "map" | "bigint"), _ -> Method f
  | _ -> Keep

let type_rule m t =
  match (m, t) with
  | "treemap", "TreeMap" -> ("map", "Map")
  | "view", "LineReader" -> ("io", "LineReader")
  | _ -> (m, t)

let printers = [ "print"; "print_str"; "eprint"; "eprint_str"; "print_i64"; "print_bool"; "print_f64"; "print_char" ]

(* 第二步：标准库以外不能直接调用内建函数。改成方法，或者 std.io、std.fs、std.env、std.process 的函数 *)
type brule =
  | BMeth of string            (* f(a, …) → a.g(…) *)
  | BIndex                     (* vec_get(v, i) → v[i] *)
  | BFind                      (* view_find(v, p, i) → v.find_from(p, i).unwrap_or(-1) *)
  | BMoved of string * string  (* f(…) → m.g(…) *)

let builtin_rule f =
  let starts p = String.starts_with ~prefix:p f in
  let strip p = String.sub f (String.length p) (String.length f - String.length p) in
  match f with
  | "vec_get" -> Some BIndex
  | "view_find" -> Some BFind
  | "vec_len" | "vec_push" | "vec_pop" | "vec_swap" -> Some (BMeth (strip "vec_"))
  | "str_to_i64" -> Some (BMeth "parse_i64")
  | "str_to_f64" -> Some (BMeth "parse_f64")
  | "str_from_i64" | "str_from_bool" | "str_from_f64" | "str_from_char" -> Some (BMeth "to_str")
  | "str_from_utf8" -> Some (BMeth "decode")
  | "str_len" | "str_eq" | "str_concat" | "str_push" | "str_clone" | "str_byte" | "str_slice" | "str_chars"
  | "str_char_count" | "str_push_char" | "str_to_bytes" ->
      Some (BMeth (strip "str_"))
  | "to_f64" | "to_i64" -> Some (BMeth f)
  | "f64_sqrt" | "f64_floor" | "f64_ceil" | "f64_round" | "f64_is_nan" -> Some (BMeth (strip "f64_"))
  | "char_code" -> Some (BMeth "code")
  | "char_from" -> Some (BMeth "to_char")
  | "bytes_len" | "bytes_get" | "bytes_set" | "bytes_push" | "bytes_append" | "bytes_slice" | "bytes_clone" | "bytes_eq" ->
      Some (BMeth (strip "bytes_"))
  | "view_len" | "view_byte" | "view_sub" | "view_to_str" | "view_eq" | "view_eq_view" | "view_print_str" ->
      Some (BMeth (strip "view_"))
  | "read_line" -> Some (BMoved ("io", "read_line"))
  | "read_stdin_in" -> Some (BMoved ("io", "read_all_in"))
  | "read_chunk" -> Some (BMoved ("io", "read_chunk"))
  | "print_byte" -> Some (BMoved ("io", "write_byte"))
  | "print_bytes" -> Some (BMoved ("io", "write_bytes"))
  | "args" -> Some (BMoved ("env", "args"))
  | "os_env" -> Some (BMoved ("env", "get"))
  | "os_exit" -> Some (BMoved ("env", "exit"))
  | "os_run" | "os_output" -> Some (BMoved ("process", strip "os_"))
  | "read_file" -> Some (BMoved ("fs", "read"))
  | "write_file" -> Some (BMoved ("fs", "write"))
  | "read_file_in" -> Some (BMoved ("fs", "read_in"))
  | "read_bytes" | "write_bytes" -> Some (BMoved ("fs", f))
  | _ when starts "os_" -> Some (BMoved ("fs", strip "os_"))
  | _ -> None

let builtin_call f r args =
  let a = String.concat "," args in
  match (r, args) with
  | BMoved (m, g), _ -> m ^ "." ^ g ^ "(" ^ a ^ ")"
  | BIndex, [ v; i ] -> recv v ^ "[" ^ String.trim i ^ "]"
  | BFind, v :: rest -> recv v ^ ".find_from(" ^ lstrip (String.concat "," rest) ^ ").unwrap_or(-1)"
  | BMeth g, v :: rest when String.trim v <> "" -> recv v ^ "." ^ g ^ "(" ^ lstrip (String.concat "," rest) ^ ")"
  | _ -> f ^ "(" ^ a ^ ")"

(* 改写到哪一步：--modules 是第一步，标准库重新划分（模块的函数换成方法、打印改名）；默认是第二步（内建函数） *)
let modules = ref false

(* --types：只把构造换成类型函数 *)
let types = ref false

let type_fn m f =
  match (m, f) with
  | ("map", "new") -> Some "Map"
  | ("hashmap", "new") -> Some "HashMap"
  | ("bigint", ("zero" | "from_i64" | "parse")) -> Some "BigInt"
  | _ -> None

type ctx = { uses : string list; (* use std.X 引入的 X *) defs : string list (* 程序自己定义的打印函数 *) }

let module_call m f args =
  let call g = g ^ "(" ^ String.concat "," args ^ ")" in
  let meth g =
    match args with
    | a :: rest when String.trim a <> "" -> recv a ^ "." ^ g ^ "(" ^ lstrip (String.concat "," rest) ^ ")"
    | _ -> call (m ^ "." ^ f)
  in
  match rule m f with
  | Keep -> call (m ^ "." ^ f)
  | Global g -> call g
  | Moved (n, g) -> call (n ^ "." ^ g)
  | Method g -> meth g
  | Swap g -> (
      match args with
      | [ a; c ] when is_num (String.trim a) && simple (String.trim c) ->
          String.trim c ^ "." ^ g ^ "(" ^ String.trim a ^ ")"
      | _ -> meth g)

let printer_call w args =
  let a = String.concat "," args in
  match w with
  | "print" -> "println(" ^ a ^ ")"
  | "print_str" -> "print(" ^ a ^ ")"
  | "eprint" -> "eprintln(" ^ a ^ ")"
  | "eprint_str" -> "eprint(" ^ a ^ ")"
  | _ ->
      let t = String.trim a in
      if String.exists (fun c -> c = '\n' || c = '"' || c = '{' || c = '}') t then "println(" ^ recv t ^ ".to_str())"
      else "println(\"{" ^ t ^ "}\")"

let rec rw ctx s =
  let n = String.length s in
  let b = Buffer.create (n + 16) in
  let rec go i =
    if i < n then begin
      let c = s.[i] in
      if c = '/' && i + 1 < n && s.[i + 1] = '/' then begin
        let j = line_end s i in
        Buffer.add_substring b s i (j - i);
        go j
      end
      else if c = '"' then go (copy_str ctx b s i)
      else if c = '\'' then begin
        let j = skip_char s i in
        Buffer.add_substring b s i (j - i);
        go j
      end
      else if is_id_start c && (i = 0 || not (is_id s.[i - 1])) then go (ident ctx b s i)
      else begin
        Buffer.add_char b c;
        go (i + 1)
      end
    end
  in
  go 0;
  Buffer.contents b

(* 字符串原样复制，插值 { … } 里的代码改写 *)
and copy_str ctx b s i =
  let n = String.length s in
  Buffer.add_char b '"';
  let rec go j =
    if j >= n then n
    else
      match s.[j] with
      | '\\' ->
          Buffer.add_substring b s j (min 2 (n - j));
          go (j + 2)
      | '"' ->
          Buffer.add_char b '"';
          j + 1
      | '{' when j + 1 < n && s.[j + 1] = '{' ->
          Buffer.add_string b "{{";
          go (j + 2)
      | '{' ->
          let k = skip_hole s (j + 1) in
          let closed = k - 1 > j && s.[k - 1] = '}' in
          let stop = if closed then k - 1 else k in
          Buffer.add_char b '{';
          Buffer.add_string b (rw ctx (String.sub s (j + 1) (stop - j - 1)));
          if closed then Buffer.add_char b '}';
          go k
      | c ->
          Buffer.add_char b c;
          go (j + 1)
  in
  go (i + 1)

(* 从名字开头的 i 开始：M.f(…)、M.T、打印函数的调用，返回处理到的位置 *)
and ident ctx b s i =
  let n = String.length s in
  let j = id_end s i in
  let w = String.sub s i (j - i) in
  let copy_to l =
    Buffer.add_string b (String.sub s i (l - i));
    l
  in
  if i > 0 && s.[i - 1] = '.' then copy_to j
  else if !types then begin
    let k = j + 1 in
    let l = if k < n && s.[j] = '.' then id_end s k else k in
    match w with
    | ("vec_new" | "bytes_new") when j < n && s.[j] = '(' && not (List.mem w ctx.defs) ->
        Buffer.add_string b (if w = "vec_new" then "Vec.new" else "Bytes.new");
        j
    | _ when l > k && l < n && s.[l] = '(' && List.mem w ctx.uses && type_fn w (String.sub s k (l - k)) <> None ->
        let f = String.sub s k (l - k) in
        Buffer.add_string b (w ^ "." ^ Option.get (type_fn w f) ^ "." ^ f);
        l
    | _ -> copy_to j
  end
  else if !modules && j + 1 < n && s.[j] = '.' && is_id_start s.[j + 1] && List.mem w ctx.uses then begin
    let k = j + 1 in
    let l = id_end s k in
    let f = String.sub s k (l - k) in
    if f.[0] >= 'A' && f.[0] <= 'Z' then begin
      let m, t = type_rule w f in
      Buffer.add_string b (m ^ "." ^ t);
      l
    end
    else if l < n && s.[l] = '(' then (
      match split_args s l with
      | None -> copy_to l
      | Some (args, e) ->
          Buffer.add_string b (module_call w f (List.map (rw ctx) args));
          e)
    else copy_to l
  end
  else if !modules && j < n && s.[j] = '(' && List.mem w printers && not (List.mem w ctx.defs) then (
    match split_args s j with
    | None -> copy_to j
    | Some (args, e) ->
        Buffer.add_string b (printer_call w (List.map (rw ctx) args));
        e)
  else if (not !modules) && j < n && s.[j] = '(' && builtin_rule w <> None && not (List.mem w ctx.defs) then (
    match split_args s j with
    | None -> copy_to j
    | Some (args, e) ->
        Buffer.add_string b (builtin_call w (Option.get (builtin_rule w)) (List.map (rw ctx) args));
        e)
  else copy_to j

(* use 行："use std.X"，返回缩进和 X *)
let use_line l =
  let t = String.trim l and p = "use std." in
  let k = String.length p in
  if String.length t > k && String.sub t 0 k = p then begin
    let m = String.sub t k (String.length t - k) in
    if String.for_all is_id m then Some (String.sub l 0 (String.index l 'u'), m) else None
  end
  else None

(* 模块 m 在代码里出现（写成 m.x）吗 *)
let mentions s m =
  let n = String.length s and k = String.length m in
  let rec go i =
    if i + k >= n then false
    else if
      String.sub s i k = m
      && s.[i + k] = '.'
      && (i = 0 || not (is_id s.[i - 1] || s.[i - 1] = '.'))
    then true
    else go (i + 1)
  in
  go 0

(* 不再存在的模块 *)
let removed = [ "str"; "char"; "option"; "result"; "view"; "os"; "treemap" ]

(* 用到了才留下的模块：旧模块的函数搬到了它们里面，或者它们里面的函数大多变成了方法 *)
let optional = [ "vec"; "math"; "io"; "fs"; "env"; "process"; "map" ]

let extras = function
  | "view" -> [ "io" ]
  | "os" -> [ "fs"; "env"; "process" ]
  | "treemap" -> [ "map" ]
  | "io" -> [ "fs" ]
  | _ -> []

let uses_of text = List.filter_map (fun l -> Option.map snd (use_line l)) (String.split_on_char '\n' text)

let contains s sub =
  let n = String.length s and k = String.length sub in
  let rec go i = i + k <= n && (String.sub s i k = sub || go (i + 1)) in
  go 0

(* 程序里 fn 定义的名字：和内建函数、打印函数同名时调用的是它们，不改写 *)
let fn_names text =
  let n = String.length text in
  let rec go i acc =
    match if i >= n then None else Some (String.index_from_opt text i 'f') with
    | None | Some None -> acc
    | Some (Some k) ->
        if k + 3 < n && text.[k + 1] = 'n' && text.[k + 2] = ' ' && (k = 0 || not (is_id text.[k - 1])) then begin
          let e = id_end text (k + 3) in
          go e (String.sub text (k + 3) (e - k - 3) :: acc)
        end
        else go (k + 1) acc
  in
  go 0 []

(* 用到了 std.io、std.fs、std.env、std.process 却没有 use 的，加上：放在已有的 use 后面，没有 use 时放在开头的注释后面 *)
let add_uses lines =
  let body = String.concat "\n" (List.filter (fun l -> use_line l = None) lines) in
  let have = List.filter_map (fun l -> Option.map snd (use_line l)) lines in
  (* 变量叫 fs 时 fs.push(…) 不是模块 *)
  let binds m =
    List.exists (fun p -> contains body p)
      [ "let " ^ m ^ " "; "var " ^ m ^ " "; "let " ^ m ^ ":"; "var " ^ m ^ ":"; "(" ^ m ^ ": "; ", " ^ m ^ ": "; "for " ^ m ^ " " ]
  in
  let need = List.filter (fun m -> mentions body m && (not (List.mem m have)) && not (binds m)) [ "io"; "fs"; "env"; "process" ] in
  if need = [] then lines
  else begin
    let news = List.map (fun m -> "use std." ^ m) need in
    let last = ref (-1) in
    List.iteri (fun i l -> if use_line l <> None then last := i) lines;
    if !last >= 0 then List.concat (List.mapi (fun i l -> if i = !last then l :: news else [ l ]) lines)
    else begin
      let rec split acc = function
        | l :: rest when starts_comment l || String.trim l = "" -> split (l :: acc) rest
        | rest -> (List.rev acc, rest)
      and starts_comment l = String.length (String.trim l) >= 2 && String.sub (String.trim l) 0 2 = "//" in
      let head, rest = split [] lines in
      head @ news @ (if rest = [] then [] else [ "" ]) @ rest
    end
  end

(* 一个完整的程序 *)
let unit text =
  let defs = fn_names text in
  let ctx = { uses = uses_of text; defs } in
  let out = rw ctx text in
  let lines = String.split_on_char '\n' out in
  if !types then out
  else if not !modules then String.concat "\n" (add_uses lines)
  else
  let body = String.concat "\n" (List.filter (fun l -> use_line l = None) lines) in
  let emitted = ref [] in
  let fixed =
    List.concat_map
      (fun l ->
        match use_line l with
        | None -> [ l ]
        | Some (ind, m) ->
            let cands = (if List.mem m removed then [] else [ m ]) @ extras m in
            List.filter_map
              (fun c ->
                if List.mem c !emitted then None
                else if (List.mem c optional && not (mentions body c)) || List.mem c removed then None
                else begin
                  emitted := c :: !emitted;
                  Some (if c = m then l else ind ^ "use std." ^ c)
                end)
              cands)
      lines
  in
  String.concat "\n" fixed

let starts s p = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* 错误信息里引用的源码行："  12 |     代码" *)
let excerpt l =
  let n = String.length l in
  let i = ref 0 in
  while !i < n && l.[!i] = ' ' do incr i done;
  let d = !i in
  while !i < n && is_digit l.[!i] do incr i done;
  if !i > d && !i + 2 < n && String.sub l !i 3 = " | " then Some (!i + 3) else None

let cram text =
  let lines = Array.of_list (String.split_on_char '\n' text) in
  let n = Array.length lines in
  let out = ref [] in
  let all = ref [] in
  (* 先收集所有程序的 use，错误信息里的源码行按它们改写 *)
  Array.iter (fun l -> if starts l "  > " then Option.iter (fun (_, m) -> all := m :: !all) (use_line (String.sub l 4 (String.length l - 4)))) lines;
  let ctx = { uses = !all; defs = [] } in
  let i = ref 0 in
  while !i < n do
    let l = lines.(!i) in
    incr i;
    out := l :: !out;
    if starts l "  $ " && contains l "<<" then begin
      let k = String.rindex l '<' + 1 in
      let tag = String.trim (String.sub l k (String.length l - k)) in
      let tag = String.concat "" (String.split_on_char '\'' (String.concat "" (String.split_on_char '"' tag))) in
      let body = ref [] in
      while !i < n && lines.(!i) <> "  > " ^ tag && starts lines.(!i) "  >" do
        let b = lines.(!i) in
        body := (if String.length b <= 4 then "" else String.sub b 4 (String.length b - 4)) :: !body;
        incr i
      done;
      let prog = unit (String.concat "\n" (List.rev !body)) in
      List.iter (fun p -> out := ("  > " ^ p) :: !out) (String.split_on_char '\n' prog)
    end
    else if not (starts l "  $ " || starts l "  > ") then
      match excerpt l with
      | Some k when starts l "  " -> (
          out := List.tl !out;
          out := (String.sub l 0 k ^ rw ctx (String.sub l k (String.length l - k))) :: !out)
      | _ -> ()
  done;
  String.concat "\n" (List.rev !out)

let markdown text =
  let lines = String.split_on_char '\n' text in
  let out = ref [] and block = ref None in
  List.iter
    (fun l ->
      let t = String.trim l in
      match !block with
      | None ->
          out := l :: !out;
          if starts t "```" then begin
            let info = String.trim (String.sub t 3 (String.length t - 3)) in
            if not (List.mem info [ "sh"; "bash"; "console"; "text" ]) then block := Some []
          end
      | Some b ->
          if starts t "```" then begin
            out := l :: List.rev_append (String.split_on_char '\n' (unit (String.concat "\n" (List.rev b)))) !out;
            block := None
          end
          else block := Some (l :: b))
    lines;
  (match !block with Some b -> out := List.rev_append (List.rev b) !out | None -> ());
  String.concat "\n" (List.rev !out)

let () =
  let argv = List.tl (Array.to_list Sys.argv) in
  let write = List.mem "-w" argv in
  modules := List.mem "--modules" argv;
  types := List.mem "--types" argv;
  let files = List.filter (fun a -> a <> "-w" && a <> "--modules" && a <> "--types") argv in
  if files = [] then begin
    prerr_endline "用法：migrate [-w] [--modules | --types] 文件…";
    exit 2
  end;
  List.iter
    (fun f ->
      let ic = open_in_bin f in
      let src = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let out =
        if Filename.check_suffix f ".t" then cram src
        else if Filename.check_suffix f ".md" then markdown src
        else unit src
      in
      if write then begin
        if out <> src then begin
          let oc = open_out_bin f in
          output_string oc out;
          close_out oc;
          print_endline f
        end
      end
      else print_string out)
    files
