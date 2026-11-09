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
     { var #i: I64 = a; let #n: I64 = b; while #i < #n { let x = #i; #i = #i + 1; { … } } }
   a、b 各求值一次；#i < #n 时加一不会溢出。x 是 let，循环体里不能改。
   for x in v { … }（v 是变量或字段里的 Vec）：
     { var #k: I64 = 0; while #k < vec_len(v) { let x = vec_get(v, #k); #k = #k + 1; { … } } }
   每一轮按借用取第 i 个元素：可复制的元素复制，需要析构的元素是投影，锁住 v（Lean M4、M12）；
   长度每轮重新读，循环体里改了 v 也只会少走或多走几轮，下标照样检查。
   加一放在取出元素之后、循环体之前（C31）：continue 直接回到条件，不会漏掉加一。 *)
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
    let inner = { stmts = [ s (Let (false, x, None, first)); step; s (Expr (e (Block body))) ]; tail = None } in
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

(* for x in inout v { … }（C33，Lean M18）：x 是第 i 个元素本身（可变借用），不是副本。展开成
     { var #k: I64 = 0; while #k < vec_len(v) { let #jN = #k; #k = #k + 1; { …[x := vec_get(v, #jN)]… } } }
   循环体里的 x 换成位置 vec_get(v, #jN)：读 x 就是读元素，x.f = e 改元素的字段，f(inout x) 把元素借出去，
   x = e 给元素整体赋值（SetPlace，旧值先析构）；把 x 移走照样被拒绝（不能从 Vec 里移出元素）。
   #jN 每个循环一个名字，嵌套时外层的 x 不会被内层的 #j 遮住。
   排他性：循环体里不能提到 v 的根变量——v 整个借给了循环，不能 push / pop / 读 / 再借（M18 的 okBody；
   bad_pop 说明去掉这条会越界）。按名字检查，比按作用域严格：循环体里新声明的同名变量也算。 *)
let inout_count = ref 0

let for_inout pos x (v : expr) body =
  let e d = mk d pos and s d = mks d pos in
  let rec copy (v : expr) =
    match v.e with
    | Var _ -> { v with id = fresh_id () }
    | Field (a, f) -> { v with e = Field (copy a, f); id = fresh_id () }
    | Deref a -> { v with e = Deref (copy a); id = fresh_id () }
    | _ -> raise (Syntax (v.pos, "for x in inout 只能遍历变量或字段里的 Vec"))
  in
  let rec root (v : expr) = match v.e with Var r -> r | Field (a, _) | Deref a -> root a | _ -> "" in
  let r = root (copy v) in
  incr inout_count;
  let j = "#j" ^ string_of_int !inout_count in
  let place (at : pos) = { e = Call ("vec_get", [ copy v; { e = Var j; pos = at; id = fresh_id () } ]); pos = at; id = fresh_id () } in
  let mention (at : pos) =
    raise (Syntax (at, Printf.sprintf "for %s in inout %s 的循环体里不能用 %s（%s 整个借给了循环，不能 push、pop、读或再借）" x r r r))
  in
  (* 把循环体里的 x 换成元素的位置；遇到重新绑定 x 的地方停下（之后的 x 是新的变量）。顺便检查有没有提到 v *)
  let rec ex (a : expr) : expr =
    let d =
      match a.e with
      | Var y when y = r -> mention a.pos
      | Var y when y = x -> (place a.pos).e
      | Int _ | Bool _ | Unit | Str _ | Var _ -> a.e
      | Binop (o, b, c) -> Binop (o, ex b, ex c)
      | Unop (o, b) -> Unop (o, ex b)
      | Call (f, bs) -> Call (f, List.map ex bs)
      | StructLit (n, fs) -> StructLit (n, List.map (fun (f, b) -> (f, ex b)) fs)
      | Field (b, f) -> Field (ex b, f)
      | Deref b -> Deref (ex b)
      | Try b -> Try (ex b)
      | Variant (w, b) -> Variant (w, Option.map ex b)
      | Match (bw, b, arms) ->
          Match (bw, ex b, List.map (fun (w, y, c) -> (w, y, if y = x then check_only c else ex c)) arms)
      | Arena (n, b) -> Arena (n, if n = x then check_block b else blk b)
      | If (c, b1, b2) -> If (ex c, blk b1, Option.map blk b2)
      | Block b -> Block (blk b)
      | Lambda (ps, b) -> Lambda (ps, if List.mem x ps then check_only b else ex b)
    in
    { a with e = d }
  (* x 被遮住之后：不再替换，但仍然不能提到 v *)
  and check_only (a : expr) : expr =
    let rec scan (a : expr) =
      match a.e with
      | Var y when y = r -> mention a.pos
      | Int _ | Bool _ | Unit | Str _ | Var _ -> ()
      | Binop (_, b, c) -> scan b; scan c
      | Unop (_, b) | Field (b, _) | Deref b | Try b | Lambda (_, b) -> scan b
      | Call (_, bs) -> List.iter scan bs
      | StructLit (_, fs) -> List.iter (fun (_, b) -> scan b) fs
      | Variant (_, b) -> Option.iter scan b
      | Match (_, b, arms) -> scan b; List.iter (fun (_, _, c) -> scan c) arms
      | Arena (_, b) | Block b -> scan_block b
      | If (c, b1, b2) -> scan c; scan_block b1; Option.iter scan_block b2
    and scan_block (b : block) = List.iter scan_stmt b.stmts; Option.iter scan b.tail
    and scan_stmt (t : stmt) =
      match t.s with
      | Let (_, _, _, b) | Expr b | Errdefer b | Destruct (_, _, b) -> scan b
      | Assign (y, b) -> if y = r then mention t.spos; scan b
      | SetField (b, _, c) | SetPlace (b, c) -> scan b; scan c
      | While (c, b) -> scan c; scan_block b
      | Return b -> Option.iter scan b
      | Decl _ | Break | Continue -> ()
    in
    scan a;
    a
  and check_block (b : block) = ignore (check_only { e = Block b; pos = p pos; id = 0 }); b
  and blk (b : block) : block =
    let rec go = function
      | [] -> ([], true)
      | (t : stmt) :: rest -> (
          match t.s with
          | Let (m, y, ty, c) when y = x ->
              let t' = { t with s = Let (m, y, ty, ex c) } in
              let rest = List.map (fun t -> ignore (check_block { stmts = [ t ]; tail = None }); t) rest in
              (t' :: rest, false)
          | Decl (y, _) when y = x ->
              let rest = List.map (fun t -> ignore (check_block { stmts = [ t ]; tail = None }); t) rest in
              (t :: rest, false)
          | Destruct (n, bs, c) when List.exists (fun (_, y) -> y = x) bs ->
              let t' = { t with s = Destruct (n, bs, ex c) } in
              let rest = List.map (fun t -> ignore (check_block { stmts = [ t ]; tail = None }); t) rest in
              (t' :: rest, false)
          | _ ->
              let t' = { t with s = stmt t } in
              let rest, live = go rest in
              (t' :: rest, live))
    in
    let stmts, live = go b.stmts in
    { stmts; tail = Option.map (fun c -> if live then ex c else check_only c) b.tail }
  and stmt (t : stmt) : stmt_desc =
    match t.s with
    | Assign (y, _) when y = r -> mention t.spos
    | Assign (y, c) when y = x -> SetPlace (place t.spos, ex c)
    | Assign (y, c) -> Assign (y, ex c)
    | Let (m, y, ty, c) -> Let (m, y, ty, ex c)
    | Decl _ as d -> d
    | SetField (a, f, c) -> SetField (ex a, f, ex c)
    | SetPlace (a, c) -> SetPlace (ex a, ex c)
    | While (c, b) -> While (ex c, blk b)
    | Return c -> Return (Option.map ex c)
    | Expr c -> Expr (ex c)
    | Errdefer c -> Errdefer (ex c)
    | (Break | Continue) as d -> d
    | Destruct (n, bs, c) -> Destruct (n, bs, ex c)
  in
  let body = blk body in
  let step = s (Assign ("#k", e (Binop (Add, e (Var "#k"), e (Int 1L))))) in
  let inner = { stmts = [ s (Let (false, j, Some TI64, e (Var "#k"))); step; s (Expr (e (Block body))) ]; tail = None } in
  let cond = e (Binop (Lt, e (Var "#k"), e (Call ("vec_len", [ copy v ])))) in
  s (Expr (e (Block { stmts = [ s (Let (true, "#k", Some TI64, e (Int 0L))); s (While (cond, inner)) ]; tail = None })))

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
%token LINEAR ERRDEFER MATCH RESULT FATARROW QUESTION ARENA IN AMP BAR ENUM TRAIT IMPL FOR BREAK CONTINUE USE PUB
%token ARROW EQEQ NE LE GE LT GT ANDAND OROR EQ DOTDOT
%token PLUS MINUS STAR SLASH PERCENT BANG
%token LPAREN RPAREN LBRACE RBRACE COMMA COLON DOT NL EOF

%nonassoc LAMBDA   (* 闭包的函数体尽量往后延伸：|x| x + 1 是 |x| (x + 1) *)
%left OROR
%left ANDAND
%nonassoc EQEQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UNARY
%left DOT QUESTION

%start <Ast.source> program
%%

program:
  | nls us = uses is = items EOF { { uses = us; items = is } }

(* 文件开头的 use（C34）：use m 或 use m { a, B }；标准库写 use std.m（C35） *)
uses:
  | { [] }
  | USE m = IDENT nls us = uses { (m, None, p $startpos) :: us }
  | USE s = IDENT DOT m = IDENT nls us = uses { (s ^ "." ^ m, None, p $startpos) :: us }
  | USE m = IDENT LBRACE nls ns = use_names RBRACE nls us = uses { (m, Some ns, p $startpos) :: us }
  | USE s = IDENT DOT m = IDENT LBRACE nls ns = use_names RBRACE nls us = uses
    { (s ^ "." ^ m, Some ns, p $startpos) :: us }

use_names:
  | { [] }
  | n = use_name nls { [n] }
  | n = use_name COMMA nls ns = use_names { n :: ns }

use_name:
  | n = IDENT { n }
  | n = UIDENT { n }

items:
  | { [] }
  | i = item nls is = items { (false, i) :: is }
  | PUB i = item nls is = items { (true, i) :: is }

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
  | FN LPAREN ps = separated_list(COMMA, fn_param) RPAREN r = ret { TFn (ps, r) }

(* 函数类型 fn(T, inout U) 的参数：约定和类型（C40） *)
fn_param:
  | t = ty { (MLet, t) }
  | INOUT t = ty { (MInout, t) }
  | SINK t = ty { (MSink, t) }

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
  | FOR x = IDENT IN INOUT v = expr bl = block { for_inout $startpos x v bl }
  | FOR x = IDENT IN v = expr bl = block { for_loop $startpos x (`Vec v) bl }
  | RETURN e = option(expr) { mks (Return e) $startpos }
  | BREAK { mks Break $startpos }
  | CONTINUE { mks Continue $startpos }
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
  (* 闭包（C40）：|x, y| e，没有参数时写 || e *)
  | BAR ps = separated_list(COMMA, IDENT) BAR e = expr %prec LAMBDA { mk (Lambda (ps, e)) $startpos }
  | OROR e = expr %prec LAMBDA { mk (Lambda ([], e)) $startpos }

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
