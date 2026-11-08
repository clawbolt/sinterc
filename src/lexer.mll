{
open Parser

exception Error of Ast.pos * string

(* 圆括号里的换行不算语句分隔。 *)
let depth = ref 0

let pos lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  { Ast.line = p.pos_lnum; col = p.pos_cnum - p.pos_bol + 1 }

let keywords = [
  "fn", FN; "struct", STRUCT; "let", LET; "var", VAR; "if", IF; "else", ELSE;
  "while", WHILE; "return", RETURN; "true", TRUE; "false", FALSE;
  "inout", INOUT; "sink", SINK; "linear", LINEAR; "errdefer", ERRDEFER; "match", MATCH;
  "arena", ARENA; "in", IN; "enum", ENUM; "trait", TRAIT; "impl", IMPL; "for", FOR;
  "break", BREAK; "continue", CONTINUE;
]
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
  | digit+ as n           { match Int64.of_string_opt n with
                            | Some v -> INT v
                            | None -> raise (Error (pos lexbuf, "整数超出 I64 范围：" ^ n)) }
  | lower alnum* as s     { match List.assoc_opt s keywords with
                            | Some k -> k
                            | None -> IDENT s }
  | upper alnum* as s     { match s with "Box" -> BOX | "Result" -> RESULT | _ -> UIDENT s }
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
  | '{'   { LBRACE } | '}'  { RBRACE }
  | ','   { COMMA }  | ':'  { COLON }
  | ';'   { NL }
  | ".."  { DOTDOT }
  | '.'   { DOT }
  | '"'   { (* 读完整个字符串后把起点改回左引号，报错位置才指向字面量的开头 *)
            let start = lexbuf.Lexing.lex_start_p and p = pos lexbuf in
            let s = str p (Buffer.create 16) lexbuf in
            lexbuf.Lexing.lex_start_p <- start;
            STRING s }
  | eof   { EOF }
  | _ as c { raise (Error (pos lexbuf, Printf.sprintf "无法识别的字符 '%c'" c)) }

(* 字符串字面量（C26）：转义只有换行、制表符、反斜杠和双引号四种；不能跨行。 *)
and str p buf = parse
  | '"'         { Buffer.contents buf }
  | "\\n"       { Buffer.add_char buf '\n'; str p buf lexbuf }
  | "\\t"       { Buffer.add_char buf '\t'; str p buf lexbuf }
  | "\\\\"      { Buffer.add_char buf '\\'; str p buf lexbuf }
  | "\\\""      { Buffer.add_char buf '"'; str p buf lexbuf }
  | '\\' _      { raise (Error (pos lexbuf, "字符串里只能用 \\n、\\t、\\\\、\\\" 这几种转义")) }
  | '\n' | eof  { raise (Error (p, "字符串没有结束（缺少右边的 \"，字符串不能跨行）")) }
  | _ as c      { Buffer.add_char buf c; str p buf lexbuf }
