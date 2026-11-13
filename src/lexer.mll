{
open Parser

exception Error of Ast.pos * string

(* 圆括号里的换行不算语句分隔。花括号里重新算：f(m, |x| { a \n b }) 里闭包体的换行分隔语句，
   saved 记下外层的圆括号层数，遇到 } 时恢复。 *)
let depth = ref 0
let saved : int list ref = ref []

let pos lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  { Ast.line = p.pos_lnum; col = p.pos_cnum - p.pos_bol + 1 }

let keywords = [
  "fn", FN; "struct", STRUCT; "let", LET; "var", VAR; "if", IF; "else", ELSE;
  "while", WHILE; "return", RETURN; "true", TRUE; "false", FALSE;
  "inout", INOUT; "sink", SINK; "linear", LINEAR; "errdefer", ERRDEFER; "match", MATCH;
  "arena", ARENA; "in", IN; "enum", ENUM; "trait", TRAIT; "impl", IMPL; "for", FOR;
  "break", BREAK; "continue", CONTINUE; "use", USE; "pub", PUB; "scope", SCOPE;
]

(* 字符串插值："x = {x}" 拆成记号 #fmt("x = ", (x).to_str())，由 parser 展开。
   pending：一次拆出来的其余记号（带各自的位置），next 依次交给 parser。 *)
let pending : (token * Lexing.position * Lexing.position) list ref = ref []
let token_ref : (Lexing.lexbuf -> token) ref = ref (fun _ -> EOF)

let pos_of (q : Lexing.position) = { Ast.line = q.pos_lnum; col = q.pos_cnum - q.pos_bol + 1 }

let flush buf parts =
  if Buffer.length buf = 0 then parts
  else begin
    let s = Buffer.contents buf in
    Buffer.clear buf;
    `Lit s :: parts
  end

(* { 后面的表达式：照常读记号，直到同一层的 }。不能跨行（字符串本身不能跨行） *)
let hole (hp : Lexing.position) lexbuf =
  let bad () = raise (Error (pos_of hp, "字符串里的 { 没有对应的 }（要写 { 本身请用 {{）")) in
  saved := !depth :: !saved;
  depth := 0;
  let rec go lvl acc =
    let ((t, a, _) as x) =
      match !pending with
      | x :: r -> pending := r; x
      | [] -> let t = !token_ref lexbuf in (t, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
    in
    if a.Lexing.pos_lnum <> hp.Lexing.pos_lnum then bad ();
    match t with
    | EOF | NL -> bad ()
    | RBRACE when lvl = 0 ->
        if acc = [] then raise (Error (pos_of hp, "字符串里的 {} 中间要写一个表达式"));
        List.rev acc
    | LBRACE -> go (lvl + 1) (x :: acc)
    | RBRACE -> go (lvl - 1) (x :: acc)
    | _ -> go lvl (x :: acc)
  in
  go 0 []

(* 一个插值字符串的记号：#fmt(片段, (表达式).to_str(), …)。第一个记号直接返回，其余放进 pending *)
let interp start fin parts =
  let at q t = (t, q, q) in
  let piece = function
    | `Lit s -> [ at start (STRING s) ]
    | `Code (hp, ts) ->
        (at hp LPAREN :: ts) @ [ at hp RPAREN; at hp DOT; at hp (IDENT "to_str"); at hp LPAREN; at hp RPAREN ]
  in
  let rec join = function
    | [] -> []
    | [ p ] -> piece p
    | p :: r -> piece p @ (at start COMMA :: join r)
  in
  pending := (at start LPAREN :: join parts) @ [ (RPAREN, fin, fin) ];
  IDENT "#fmt"
}

let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let alnum = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
  | [' ' '\t' '\r']+      { token lexbuf }
  | "//" [^ '\n']*        { token lexbuf }
  | '\n'                  { Lexing.new_line lexbuf;
                            if !depth > 0 then token lexbuf else NL }
  (* 浮点数：1.5、2.0e-3、1e9。紧跟在 . 后面时（t.0.1 取嵌套元组的元素）只读整数部分，
     . 和后面的数字留给下一个记号 *)
  | (digit+ '.' digit+ (['e' 'E'] ['+' '-']? digit+)? | digit+ ['e' 'E'] ['+' '-']? digit+) as s
      { let st = lexbuf.Lexing.lex_start_pos in
        if st > 0 && Bytes.get lexbuf.Lexing.lex_buffer (st - 1) = '.' && String.contains s '.' then begin
          let k = String.index s '.' in
          lexbuf.Lexing.lex_curr_pos <- st + k;
          lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with pos_cnum = lexbuf.Lexing.lex_start_p.pos_cnum + k };
          INT (Int64.of_string (String.sub s 0 k))
        end
        else match float_of_string_opt s with
          | Some f when Float.is_finite f -> FLOAT f
          | _ -> raise (Error (pos lexbuf, "浮点数超出 F64 范围：" ^ s)) }
  | digit+ as n           { match Int64.of_string_opt n with
                            | Some v -> INT v
                            | None -> raise (Error (pos lexbuf, "整数超出 I64 范围：" ^ n)) }
  | lower alnum* as s     { match List.assoc_opt s keywords with
                            | Some k -> k
                            | None -> IDENT s }
  | upper alnum* as s     { match s with "Box" -> BOX | "Result" -> RESULT | _ -> UIDENT s }
  (* 另一个模块里的类型或 trait：geom.Point，中间没有空格。字段名都是小写，不会和它混淆 *)
  | (lower alnum* '.' upper alnum*) as s { UIDENT s }
  | "->"  { ARROW }   | "=>"  { FATARROW }
  | '?'   { QUESTION }
  | "=="  { EQEQ }  | "!="  { NE }
  | "<="  { LE }    | ">="  { GE }
  | "&&"  { ANDAND } | "||" { OROR }
  | '&'   { AMP }    | '|'   { BAR }
  | '<'   { LT }    | '>'   { GT }
  | '='   { EQ }
  | '+'   { PLUS }  | '-'   { MINUS }
  | '*'   { STAR }  | '/'   { SLASH } | '%' { PERCENT }
  | '!'   { BANG }
  | '('   { incr depth; LPAREN }
  | ')'   { decr depth; RPAREN }
  | '['   { incr depth; LBRACKET }
  | ']'   { decr depth; RBRACKET }
  | '{'   { saved := !depth :: !saved; depth := 0; LBRACE }
  | '}'   { (match !saved with d :: ds -> depth := d; saved := ds | [] -> ()); RBRACE }
  | ','   { COMMA }  | ':'  { COLON }
  | ';'   { NL }
  | ".."  { DOTDOT }
  | '.'   { DOT }
  | '"'   { (* 读完整个字符串后把起点改回左引号，报错位置才指向字面量的开头 *)
            let start = lexbuf.Lexing.lex_start_p and p = pos lexbuf in
            let parts = str p (Buffer.create 16) [] lexbuf in
            lexbuf.Lexing.lex_start_p <- start;
            match parts with
            | [] -> STRING ""
            | [ `Lit s ] -> STRING s
            | _ -> interp start lexbuf.Lexing.lex_curr_p parts }
  (* 字符字面量：'a'、'中'、'😀'，转义 '\n'、'\t'、'\\'、'\''、'\0'。引号里正好一个字符 *)
  | "'\\n'" { CHAR 10 }   | "'\\t'" { CHAR 9 }   | "'\\\\'" { CHAR 92 }
  | "'\\''" { CHAR 39 }   | "'\\0'" { CHAR 0 }
  | '\'' ([^ '\\' '\'' '\n'] ['\128'-'\191']* as s) '\''
      { let d = String.get_utf_8_uchar s 0 in
        if Uchar.utf_decode_is_valid d && Uchar.utf_decode_length d = String.length s then
          CHAR (Uchar.to_int (Uchar.utf_decode_uchar d))
        else raise (Error (pos lexbuf, "字符字面量的引号里要正好是一个字符")) }
  | '\'' { raise (Error (pos lexbuf, "字符字面量的引号里要正好是一个字符（转义只有 \\n、\\t、\\\\、\\'、\\0）")) }
  | eof   { EOF }
  | _ as c { raise (Error (pos lexbuf, Printf.sprintf "无法识别的字符 '%c'" c)) }

(* 字符串字面量：转义有换行、回车、制表符、反斜杠和双引号；不能跨行。 *)
(* 字符串插值：{ 表达式 } 是插值，{{ 和 }} 是 { 和 } 本身。 *)
and str p buf parts = parse
  | '"'         { List.rev (flush buf parts) }
  | "{{"        { Buffer.add_char buf '{'; str p buf parts lexbuf }
  | "}}"        { Buffer.add_char buf '}'; str p buf parts lexbuf }
  | '{'         { let hp = lexbuf.Lexing.lex_start_p in
                  let parts = flush buf parts in
                  let ts = hole hp lexbuf in
                  str p buf (`Code (hp, ts) :: parts) lexbuf }
  | '}'         { raise (Error (pos lexbuf, "字符串里的 } 要写成 }}")) }
  | "\\n"       { Buffer.add_char buf '\n'; str p buf parts lexbuf }
  | "\\t"       { Buffer.add_char buf '\t'; str p buf parts lexbuf }
  | "\\r"       { Buffer.add_char buf '\r'; str p buf parts lexbuf }   (* Windows 换行 \r\n *)
  | "\\\\"      { Buffer.add_char buf '\\'; str p buf parts lexbuf }
  | "\\\""      { Buffer.add_char buf '"'; str p buf parts lexbuf }
  | '\\' _      { raise (Error (pos lexbuf, "字符串里只能用 \\n、\\r、\\t、\\\\、\\\" 这几种转义")) }
  | '\n' | eof  { raise (Error (p, "字符串没有结束（缺少右边的 \"，字符串不能跨行）")) }
  | _ as c      { Buffer.add_char buf c; str p buf parts lexbuf }

{
let () = token_ref := token

(* parser 取记号：先交出插值拆出来的记号（同时设好位置），没有了再读源码 *)
let next lexbuf =
  match !pending with
  | (t, a, b) :: r -> pending := r; lexbuf.Lexing.lex_start_p <- a; lexbuf.Lexing.lex_curr_p <- b; t
  | [] -> token lexbuf
}
