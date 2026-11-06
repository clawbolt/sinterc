{
open Parser

exception Error of Ast.pos * string

(* 圆括号里的换行不算语句分隔。 *)
let depth = ref 0

let pos lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  { Ast.line = p.pos_lnum; col = p.pos_cnum - p.pos_bol + 1 }

let keywords = [
  "fn", FN; "struct", STRUCT; "let", LET; "if", IF; "else", ELSE;
  "while", WHILE; "return", RETURN; "true", TRUE; "false", FALSE;
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
  | upper alnum* as s     { UIDENT s }
  | "->"  { ARROW }
  | "=="  { EQEQ }  | "!="  { NE }
  | "<="  { LE }    | ">="  { GE }
  | "&&"  { ANDAND } | "||" { OROR }
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
  | '.'   { DOT }
  | eof   { EOF }
  | _ as c { raise (Error (pos lexbuf, Printf.sprintf "无法识别的字符 '%c'" c)) }
