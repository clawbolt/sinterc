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

(* for 循环（C30）：解析时展开成 while，之后的检查、降级、翻译验证都不用改。
   隐藏变量的名字带 #，用户写不出来，不会和用户的变量冲突；嵌套的 for 在各自的块里遮住外层的。
   for x in a..b { … }：
     { var #i: I64 = a; let #n: I64 = b; while #i < #n { let x = #i; { … }; #i = #i + 1 } }
   a、b 各求值一次；#i < #n 时加一不会溢出。x 是 let，循环体里不能改。
   for x in v { … }（v 是变量或字段里的 Vec）：
     { var #k: I64 = 0; while #k < vec_len(v) { let x = vec_get(v, #k); { … }; #k = #k + 1 } }
   每一轮按借用取第 i 个元素：可复制的元素复制，需要析构的元素是投影，锁住 v（Lean M4、M12）；
   长度每轮重新读，循环体里改了 v 也只会少走或多走几轮，下标照样检查。 *)
let for_loop pos x (src : [ `Range of expr * expr | `Vec of expr ]) body =
  let e d = mk d pos and s d = mks d pos in
  let rec copy (v : expr) =
    match v.e with
    | Var _ -> { v with id = fresh_id () }
    | Field (a, f) -> { v with e = Field (copy a, f); id = fresh_id () }
    | Deref a -> { v with e = Deref (copy a); id = fresh_id () }
    | _ -> raise (Syntax (v.pos, "for 只能遍历变量或字段里的 Vec，或者写成 a..b"))
  in
  let loop k init cond first =
    let step = s (Assign (k, e (Binop (Add, e (Var k), e (Int 1L))))) in
    let inner = { stmts = [ s (Let (false, x, None, first)); s (Expr (e (Block body))); step ]; tail = None } in
    s (Expr (e (Block { stmts = init @ [ s (While (cond, inner)) ]; tail = None })))
  in
  match src with
  | `Range (a, b) ->
      loop "#i"
        [ s (Let (true, "#i", Some TI64, a)); s (Let (false, "#n", Some TI64, b)) ]
        (e (Binop (Lt, e (Var "#i"), e (Var "#n"))))
        (e (Var "#i"))
  | `Vec v ->
      loop "#k"
        [ s (Let (true, "#k", Some TI64, e (Int 0L))) ]
        (e (Binop (Lt, e (Var "#k"), e (Call ("vec_len", [ copy v ])))))
        (e (Call ("vec_get", [ copy v; e (Var "#k") ])))

(* 类型参数 <T, U: Show + linear>：拆成 (名字, 是否 linear) 和 trait 约束 (名字, trait)。 *)
let split_tparams tp =
  ( List.map (fun (n, bs) -> (n, List.mem "" bs)) tp,
    List.concat_map (fun (n, bs) -> List.filter_map (fun b -> if b = "" then None else Some (n, b)) bs) tp )

(* 结构体、枚举的类型参数不能写 trait 约束。 *)
let plain_tparams pos tp =
  let tp, bs = split_tparams tp in
  if bs <> [] then raise (Syntax (p pos, "结构体和枚举的类型参数暂不能写 trait 约束"));
  tp

let ty_of_name = function
  | "I64" -> TI64
  | "Bool" -> TBool
  | "Str" -> TStr
  | "Arena" -> TArena ""        (* 名字由参数名补上 *)
  | n -> TStruct n
%}

%token <int64> INT
%token <string> STRING
%token <string> IDENT UIDENT
%token FN STRUCT LET VAR IF ELSE WHILE RETURN TRUE FALSE BOX INOUT SINK
%token LINEAR ERRDEFER MATCH RESULT FATARROW QUESTION ARENA IN AMP BAR ENUM TRAIT IMPL FOR
%token ARROW EQEQ NE LE GE LT GT ANDAND OROR EQ DOTDOT
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
    { Struct { name = n; tparams = plain_tparams $startpos tp; fields = fs; linear = false; ipos = p $startpos } }
  | LINEAR STRUCT n = UIDENT tp = tparams LBRACE nls fs = fields RBRACE
    { Struct { name = n; tparams = plain_tparams $startpos tp; fields = fs; linear = true; ipos = p $startpos } }
  | ENUM n = UIDENT tp = tparams LBRACE nls vs = variants RBRACE
    { Enum { name = n; tparams = plain_tparams $startpos tp; variants = vs; ipos = p $startpos } }
  | f = fn_item { f }
  | TRAIT n = UIDENT LBRACE nls ms = msigs RBRACE
    { Trait { name = n; methods = ms; ipos = p $startpos } }
  | IMPL t = UIDENT FOR s = ty LBRACE nls fs = impl_fns RBRACE
    { Impl { trait = t; self = s; fns = fs; ipos = p $startpos } }

fn_item:
  | FN n = IDENT tp = tparams LPAREN ps = separated_list(COMMA, param) RPAREN r = ret b = block
    { let tp, bs = split_tparams tp in
      Fn { name = n; tparams = tp; bounds = bs; params = ps; ret = r; body = b; ipos = p $startpos } }

(* trait 里的方法签名，没有函数体 *)
msigs:
  | { [] }
  | FN n = IDENT LPAREN ps = separated_list(COMMA, param) RPAREN r = ret nls ms = msigs
    { { mname = n; mparams = ps; mret = r; mpos = p $startpos } :: ms }

impl_fns:
  | { [] }
  | f = fn_item nls fs = impl_fns { f :: fs }

(* 类型参数：<T, U: linear, V: Show + Eq> *)
tparams:
  | { [] }
  | LT ts = separated_nonempty_list(COMMA, tparam) GT { ts }

tparam:
  | n = UIDENT { (n, []) }
  | n = UIDENT COLON bs = separated_nonempty_list(PLUS, bound) { (n, bs) }

(* "" 表示 linear *)
bound:
  | LINEAR { "" }
  | t = UIDENT { t }

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
  | a = expr DOT f = IDENT EQ e = expr { mks (SetField (a, f, e)) $startpos }
  | WHILE c = expr b = block { mks (While (c, b)) $startpos }
  | FOR x = IDENT IN a = expr DOTDOT b = expr bl = block { for_loop $startpos x (`Range (a, b)) bl }
  | FOR x = IDENT IN v = expr bl = block { for_loop $startpos x (`Vec v) bl }
  | RETURN e = option(expr) { mks (Return e) $startpos }
  | ERRDEFER e = expr { mks (Errdefer e) $startpos }
  | LET t = UIDENT LBRACE nls bs = binds RBRACE EQ e = expr { mks (Destruct (t, bs, e)) $startpos }
  | e = expr { mks (Expr e) $startpos }

expr:
  | n = INT { mk (Int n) $startpos }
  | s = STRING { mk (Str s) $startpos }
  | TRUE { mk (Bool true) $startpos }
  | FALSE { mk (Bool false) $startpos }
  | LPAREN RPAREN { mk Unit $startpos }
  | x = IDENT { mk (Var x) $startpos }
  | LPAREN e = expr RPAREN { e }
  | ARENA BAR r = IDENT BAR b = block { mk (Arena (r, b)) $startpos }
  | f = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN { mk (Call (f, args)) $startpos }
  | n = UIDENT LBRACE nls fs = inits RBRACE { mk (StructLit (n, fs)) $startpos }
  | e = expr DOT f = IDENT { mk (Field (e, f)) $startpos }
  (* 方法调用 e.f(…)：就是 f(e, …)，函数名由类型检查按 e 的类型确定（"." 开头表示还没确定） *)
  | e = expr DOT f = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    { mk (Call ("." ^ f, e :: args)) $startpos(f) }
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

(* 拆开结构体：let Node { v, next: n } = e *)
binds:
  | { [] }
  | b = bind nls { [b] }
  | b = bind COMMA nls bs = binds { b :: bs }

bind:
  | f = IDENT { (f, f) }
  | f = IDENT COLON x = IDENT { (f, x) }
