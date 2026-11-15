(* C 后端（三）：一个 MIR 函数生成一个 C 函数。每个基本块是一个标签，块之间用 goto 跳转；
   MIR 已经按从左到右的顺序把操作数存进了局部变量，这里不需要再处理求值顺序。 *)
open Ast
open C_names
open C_types

(* let / inout 参数和投影在 C 里是指针，用到时先解引用。 *)
let local (f : Mir.func) l =
  if f.locals.(l).lref then Printf.sprintf "(*l%d)" l else Printf.sprintf "l%d" l

let place f (p : Mir.place) =
  List.fold_left
    (fun s -> function
      | Mir.PField f -> Printf.sprintf "%s.f_%s" s f
      | Mir.PDeref -> Printf.sprintf "(*%s)" s
      | Mir.PVar i -> Printf.sprintf "%s.u.v%d" s i
      | Mir.PIndex i -> Printf.sprintf "%s.data[sn_idx(%s.len, %s)]" s s (local f i))
    (local f p.base) p.proj

let operand f = function
  | Mir.Const (Mir.CInt n) -> Printf.sprintf "INT64_C(%Ld)" n
  | Mir.Const (Mir.CChar c) -> Printf.sprintf "UINT32_C(%d)" c
  | Mir.Const (Mir.CF64 x) -> Printf.sprintf "%h" x   (* 十六进制浮点数：和字面量的值完全相同 *)
  | Mir.Const (Mir.CBool b) -> if b then "true" else "false"
  | Mir.Const Mir.CUnit -> "0"
  | Mir.Const (Mir.CStr s) ->
      (* C 的字符串字面量：可打印的 ASCII 原样写，其余字节（包括 UTF-8 的中文）写成八进制转义 *)
      let b = Buffer.create (String.length s + 2) in
      Buffer.add_char b '"';
      String.iter
        (fun c ->
          match c with
          | '"' | '\\' -> Buffer.add_char b '\\'; Buffer.add_char b c
          | ' ' .. '~' -> Buffer.add_char b c
          | c -> Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c)))
        s;
      Buffer.add_char b '"';
      Buffer.contents b
  | Mir.Copy p | Mir.Move p -> place f p
  | Mir.Ref p | Mir.RefMut p -> Printf.sprintf "&%s" (place f p)

let rvalue d (g : Tast.generated) fn rv =
  let operand = operand fn in
  match rv with
  | Mir.Use o -> operand o
  | Mir.Bin (op, a, b) -> (
      let a = operand a and b = operand b in
      let f name = Printf.sprintf "sn_%s(%s, %s)" name a b in
      let c sym = Printf.sprintf "(%s %s %s)" a sym b in
      match op with
      | Add -> f "add" | Sub -> f "sub" | Mul -> f "mul" | Div -> f "div" | Rem -> f "rem"
      | Eq -> c "==" | Ne -> c "!=" | Lt -> c "<" | Le -> c "<=" | Gt -> c ">" | Ge -> c ">="
      | And -> c "&&" | Or -> c "||")
  | Mir.Un (Neg, a) -> Printf.sprintf "sn_neg(%s)" (operand a)
  | Mir.Un (Not, a) -> Printf.sprintf "(!%s)" (operand a)
  | Mir.Call (f, args) ->
      let name = match List.assoc_opt f g.vec_calls with Some (b, e) -> vec_fn d b e | None -> fname f in
      Printf.sprintf "%s(%s)" name (String.concat ", " (List.map operand args))
  | Mir.Agg (n, fs) ->
      let fs = List.map (fun (f, o) -> Printf.sprintf ".f_%s = %s" f (operand o)) fs in
      Printf.sprintf "((%s){ %s })" (sname d n) (String.concat ", " fs)
  | Mir.Unbox (t, o) ->
      let t = cty d t in
      Printf.sprintf "({ %s *p_ = %s; %s v_ = *p_; sn_free(p_); v_; })" t (operand o) t
  | Mir.BoxNew (t, o) ->
      let t = cty d t in
      Printf.sprintf "({ %s *p_ = sn_alloc(sizeof(%s)); *p_ = %s; p_; })" t t (operand o)
  | Mir.MkEnum (t, i, o) ->
      let payload = match o with Some o -> Printf.sprintf ", .u.v%d = %s" i (operand o) | None -> "" in
      Printf.sprintf "((%s){ .tag = %d%s })" (cty d t) i payload
  | Mir.Payload (o, i) -> Printf.sprintf "%s.u.v%d" (operand o) i
  | Mir.TagIs (o, i) -> Printf.sprintf "(%s.tag == %d)" (operand o) i
  | Mir.Consume _ -> failwith "Consume 只出现在 Eval 里"
  | Mir.Alloc (t, a, o) ->
      let t = cty d t in
      Printf.sprintf "({ %s *p_ = sn_arena_alloc(&%s, sizeof(%s)); *p_ = %s; p_; })" t (place fn a) t
        (operand o)

(* 从入口能走到的块；走不到的块（return 之后的死代码）不输出。 *)
let reachable (f : Mir.func) =
  let seen = Array.make (Array.length f.blocks) false in
  let rec go i =
    if not seen.(i) then begin
      seen.(i) <- true;
      match f.blocks.(i).term with
      | Mir.Goto j -> go j
      | Mir.Branch (_, a, b) -> go a; go b
      | Mir.Return | Mir.Halt | Mir.Unterminated -> ()
    end
  in
  go 0;
  seen

let signature d (f : Mir.func) =
  let ps =
    if f.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun l -> Printf.sprintf "%s l%d" (lty d f.locals.(l)) l) f.params)
  in
  Printf.sprintf "static %s %s(%s)" (if f.ret = TUnit then "void" else cty d f.ret) (fname f.name) ps

let func d g b (f : Mir.func) =
  let add s = Buffer.add_string b s in
  add (signature d f ^ " {\n");
  Array.iteri
    (fun i (l : Mir.local_info) ->
      if not (List.mem i f.params) then
        add (Printf.sprintf "  %s l%d;%s\n" (lty d l) i
               (if l.lname = "" then "" else Printf.sprintf "  /* %s */" l.lname)))
    f.locals;
  let seen = reachable f in
  let place = place f and operand = operand f and rvalue = rvalue d g f in
  Array.iteri
    (fun i (blk : Mir.block) ->
      if seen.(i) then begin
        add (Printf.sprintf "bb%d: ;\n" i);
        List.iter
          (function
            | Mir.Assign (p, rv) -> add (Printf.sprintf "  %s = %s;\n" (place p) (rvalue rv))
            | Mir.Eval (Mir.Consume (t, o)) -> add (Printf.sprintf "  %s\n" (drop_code d t (operand o)))
            | Mir.Eval (Mir.Payload _) -> ()   (* 只把值标记为已移走，没有代码 *)
            | Mir.Eval rv -> add (Printf.sprintf "  %s;\n" (rvalue rv))
            | Mir.Proj (q, p) -> add (Printf.sprintf "  l%d = &%s;\n" q (place p))
            | Mir.DropNow l ->
                add (Printf.sprintf "  %s\n" (drop_code d f.locals.(l).lty (local f l)))
            | Mir.DropIf (l, fl) ->
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code d f.locals.(l).lty (local f l)))
            | Mir.DropAt p -> add (Printf.sprintf "  %s\n" (drop_code d (Mir.place_ty d f p) (place p)))
            | Mir.DropField (l, path) ->
                let p = { (Mir.whole l) with proj = List.map (fun g -> Mir.PField g) path } in
                add (Printf.sprintf "  %s\n" (drop_code d (Mir.place_ty d f p) (place p)))
            | Mir.DropFieldIf (l, path, fl) ->
                let p = { (Mir.whole l) with proj = List.map (fun g -> Mir.PField g) path } in
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code d (Mir.place_ty d f p) (place p)))
            | Mir.Live _ | Mir.Raise -> ()
            | Mir.Enter l -> add (Printf.sprintf "  %s.head = 0;\n" (local f l))
            | Mir.Leave l -> add (Printf.sprintf "  sn_arena_free(&%s);\n" (local f l))
            | Mir.Drop _ -> failwith "Drop 应当已由 Own.elaborate 展开")
          blk.stmts;
        match blk.term with
        | Mir.Goto j -> add (Printf.sprintf "  goto bb%d;\n" j)
        | Mir.Branch (o, a, c) ->
            add (Printf.sprintf "  if (%s) goto bb%d; else goto bb%d;\n" (operand o) a c)
        | Mir.Halt -> add "  __builtin_unreachable();\n"   (* u_panic 不会返回 *)
        | Mir.Return | Mir.Unterminated -> (
            match f.ret_local with
            | Some r -> add (Printf.sprintf "  return l%d;\n" r)
            | None -> add "  return;\n")
      end)
    f.blocks;
  add "}\n"
