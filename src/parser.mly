%{
open Ast

let p (pos : Lexing.position) = { line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 }

let mk e pos = { e; pos = p pos; id = fresh_id () }
let mks s pos = { s; spos = p pos }

(* 没有分号：块的最后一条若是表达式语句，就是块的值。 *)
let mk_block ss =
  match List.rev ss with
  | { s = Expr e; _ } :: rest -> { stmts = List.rev rest; tail = Some e }
  | _ -> { stmts = ss; tail = None }

(* 参数 a: Arena 引入名为 a 的区域。 *)
let arena_param n = function TArena _ -> TArena n | t -> t

let ty_of_name = function
  | "I64" -> TI64
  | "Bool" -> TBool
  | "Arena" -> TArena ""        (* 名字由参数名补上 *)
  | n -> TStruct n
%}

%token <int64> INT
%token <string> IDENT UIDENT
%token FN STRUCT LET VAR IF ELSE WHILE RETURN TRUE FALSE BOX INOUT SINK
%token LINEAR ERRDEFER MATCH RESULT FATARROW QUESTION ARENA IN AMP BAR ENUM
%token ARROW EQEQ NE LE GE LT GT ANDAND OROR EQ
%token PLUS MINUS STAR SLASH PERCENT BANG
%token LPAREN RPAREN LBRACE RBRACE COMMA COLON DOT NL EOF

%left OROR
%left ANDAND
%nonassoc EQEQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UNARY
%left DOT QUESTION

%start <Ast.program> program
%%

program:
  | nls is = items EOF { is }

items:
  | { [] }
  | i = item nls is = items { i :: is }

nls:
  | { () }
  | NL nls { () }

item:
  | STRUCT n = UIDENT tp = tparams LBRACE nls fs = fields RBRACE
    { Struct { name = n; tparams = tp; fields = fs; linear = false; ipos = p $startpos } }
  | LINEAR STRUCT n = UIDENT tp = tparams LBRACE nls fs = fields RBRACE
    { Struct { name = n; tparams = tp; fields = fs; linear = true; ipos = p $startpos } }
  | ENUM n = UIDENT tp = tparams LBRACE nls vs = variants RBRACE
    { Enum { name = n; tparams = tp; variants = vs; ipos = p $startpos } }
  | FN n = IDENT tp = tparams LPAREN ps = separated_list(COMMA, param) RPAREN r = ret b = block
    { Fn { name = n; tparams = tp; params = ps; ret = r; body = b; ipos = p $startpos } }

(* 类型参数：<T, U: linear> *)
tparams:
  | { [] }
  | LT ts = separated_nonempty_list(COMMA, tparam) GT { ts }

tparam:
  | n = UIDENT { (n, false) }
  | n = UIDENT COLON LINEAR { (n, true) }

fields:
  | { [] }
  | f = field nls { [f] }
  | f = field COMMA nls fs = fields { f :: fs }

field:
  | n = IDENT COLON t = ty { { fname = n; fty = t } }

(* 变体：A、A(T)，或者 A(x: T)（名字只作说明） *)
variants:
  | { [] }
  | v = variant nls { [v] }
  | v = variant COMMA nls vs = variants { v :: vs }

variant:
  | n = UIDENT { (n, None) }
  | n = UIDENT LPAREN t = ty RPAREN { (n, Some t) }
  | n = UIDENT LPAREN IDENT COLON t = ty RPAREN { (n, Some t) }

param:
  | n = IDENT COLON t = ty { (MLet, n, arena_param n t) }
  | LET n = IDENT COLON t = ty { (MLet, n, arena_param n t) }
  | INOUT n = IDENT COLON t = ty { (MInout, n, arena_param n t) }
  | SINK n = IDENT COLON t = ty { (MSink, n, arena_param n t) }

ty:
  | n = UIDENT { ty_of_name n }
  | n = UIDENT LT ts = separated_nonempty_list(COMMA, ty) GT { TApp (n, ts) }
  | LPAREN RPAREN { TUnit }
  | BOX LT t = ty GT { TBox t }
  | AMP t = ty IN r = IDENT { TRef (t, r) }
  | RESULT LT t = ty COMMA e = ty GT { TResult (t, e) }

ret:
  | { TUnit }
  | ARROW t = ty { t }

block:
  | LBRACE nls ss = stmts RBRACE { mk_block ss }

stmts:
  | { [] }
  | s = stmt { [s] }
  | s = stmt NL nls ss = stmts { s :: ss }

stmt:
  | LET x = IDENT t = option(preceded(COLON, ty)) EQ e = expr { mks (Let (false, x, t, e)) $startpos }
  | VAR x = IDENT t = option(preceded(COLON, ty)) EQ e = expr { mks (Let (true, x, t, e)) $startpos }
  | VAR x = IDENT COLON t = ty { mks (Decl (x, t)) $startpos }
  | x = IDENT EQ e = expr { mks (Assign (x, e)) $startpos }
  | WHILE c = expr b = block { mks (While (c, b)) $startpos }
  | RETURN e = option(expr) { mks (Return e) $startpos }
  | ERRDEFER e = expr { mks (Errdefer e) $startpos }
  | e = expr { mks (Expr e) $startpos }

expr:
  | n = INT { mk (Int n) $startpos }
  | TRUE { mk (Bool true) $startpos }
  | FALSE { mk (Bool false) $startpos }
  | LPAREN RPAREN { mk Unit $startpos }
  | x = IDENT { mk (Var x) $startpos }
  | LPAREN e = expr RPAREN { e }
  | ARENA BAR r = IDENT BAR b = block { mk (Arena (r, b)) $startpos }
  | f = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN { mk (Call (f, args)) $startpos }
  | n = UIDENT LBRACE nls fs = inits RBRACE { mk (StructLit (n, fs)) $startpos }
  | e = expr DOT f = IDENT { mk (Field (e, f)) $startpos }
  | e = expr QUESTION { mk (Try e) $startpos($2) }
  | n = UIDENT LPAREN a = expr RPAREN
    { match n with
      | "Ok" | "Err" -> mk (Variant (n, Some a)) $startpos
      | _ -> mk (Call (n, [ a ])) $startpos }
  | MATCH s = expr LBRACE nls a = arms RBRACE { mk (Match (false, s, a)) $startpos }
  | MATCH LET s = expr LBRACE nls a = arms RBRACE { mk (Match (true, s, a)) $startpos }
  | DOT n = UIDENT { mk (Variant (n, None)) $startpos }
  | DOT n = UIDENT LPAREN a = expr RPAREN { mk (Variant (n, Some a)) $startpos }
  | a = expr o = binop b = expr { mk (Binop (o, a, b)) $startpos }
  | MINUS e = expr %prec UNARY { mk (Unop (Neg, e)) $startpos }
  | BANG e = expr %prec UNARY { mk (Unop (Not, e)) $startpos }
  | STAR e = expr %prec UNARY { mk (Deref e) $startpos }
  | i = if_expr { i }
  | b = block { mk (Block b) $startpos }

if_expr:
  | IF c = expr b = block { mk (If (c, b, None)) $startpos }
  | IF c = expr b = block ELSE e = block { mk (If (c, b, Some e)) $startpos }
  | IF c = expr b = block ELSE i = if_expr
    { mk (If (c, b, Some { stmts = []; tail = Some i })) $startpos }

arm:
  | n = UIDENT LPAREN x = IDENT RPAREN FATARROW e = expr { (n, x, e) }
  | DOT n = UIDENT LPAREN x = IDENT RPAREN FATARROW e = expr { (n, x, e) }
  | DOT n = UIDENT FATARROW e = expr { (n, "", e) }

arms:
  | { [] }
  | a = arm { [a] }
  | a = arm sep rest = arms { a :: rest }

sep:
  | NL nls { () }
  | COMMA nls { () }

inits:
  | { [] }
  | x = IDENT COLON e = expr nls { [(x, e)] }
  | x = IDENT COLON e = expr COMMA nls fs = inits { (x, e) :: fs }

%inline binop:
  | PLUS { Add } | MINUS { Sub } | STAR { Mul } | SLASH { Div } | PERCENT { Rem }
  | EQEQ { Eq } | NE { Ne } | LT { Lt } | LE { Le } | GT { Gt } | GE { Ge }
  | ANDAND { And } | OROR { Or }
