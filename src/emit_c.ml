(* C 后端：从 MIR 生成 C。每个基本块是一个标签，块之间用 goto 跳转；
   MIR 已经按从左到右的顺序把操作数存进了局部变量，这里不需要再处理求值顺序。 *)
open Ast

let prelude = {|#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static void sn_panic(const char *m) { fflush(stdout); fprintf(stderr, "panic: %s\n", m); exit(101); }
static int64_t sn_add(int64_t a, int64_t b) { int64_t r; if (__builtin_add_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_sub(int64_t a, int64_t b) { int64_t r; if (__builtin_sub_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_mul(int64_t a, int64_t b) { int64_t r; if (__builtin_mul_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_div(int64_t a, int64_t b) { if (b == 0) sn_panic("除以零"); if (a == INT64_MIN && b == -1) sn_panic("整数溢出"); return a / b; }
static int64_t sn_rem(int64_t a, int64_t b) { if (b == 0) sn_panic("除以零"); if (a == INT64_MIN && b == -1) sn_panic("整数溢出"); return a % b; }
static int64_t sn_neg(int64_t a) { if (a == INT64_MIN) sn_panic("整数溢出"); return -a; }
static void u_print_i64(int64_t x) { printf("%lld\n", (long long)x); }
static void u_print_bool(bool b) { puts(b ? "true" : "false"); }
/* 原型阶段统计还没释放的 Box，程序结束时检查有没有泄漏。 */
static long long sn_live = 0;
static void *sn_alloc(size_t n) { void *p = malloc(n); if (!p) sn_panic("内存不足"); sn_live++; return p; }
static void sn_free(void *p) { free(p); sn_live--; }
|}

let rec cty = function
  | TI64 -> "int64_t"
  | TBool -> "bool"
  | TUnit -> "void"
  | TStruct n -> "S_" ^ n
  | TBox t -> cty t ^ "*"

(* 析构一个值：先析构 Box 里面的值，再释放 Box；结构体逐个析构字段。 *)
let rec drop_code ty lv =
  match ty with
  | TBox t -> drop_code t ("(*" ^ lv ^ ")") ^ Printf.sprintf "sn_free(%s); " lv
  | TStruct n ->
      String.concat ""
        (List.map
           (fun (f : field) -> drop_code f.fty (lv ^ ".f_" ^ f.fname))
           (Hashtbl.find Typeck.struct_tbl n))
  | TI64 | TBool | TUnit -> ""

(* 结构体按依赖顺序输出：按值包含的结构体先定义（类型检查已经排除了成环）。 *)
let struct_order (p : program) =
  let defs = List.filter_map (function Struct s -> Some (s.name, s.fields) | _ -> None) p in
  let out = ref [] in
  let rec visit n =
    if not (List.mem_assoc n !out) then
      match List.assoc_opt n defs with
      | Some fs ->
          let rec dep = function TStruct m -> visit m | TBox t -> dep t | _ -> () in
          List.iter (fun (f : field) -> dep f.fty) fs;
          if not (List.mem_assoc n !out) then out := (n, fs) :: !out
      | None -> ()
  in
  List.iter (fun (n, _) -> visit n) defs;
  List.rev !out

(* ---------- 从 MIR 生成 ---------- *)

(* let / inout 参数和投影在 C 里是指针，用到时先解引用。 *)
let local (f : Mir.func) l =
  if f.locals.(l).lref then Printf.sprintf "(*l%d)" l else Printf.sprintf "l%d" l

let place f (p : Mir.place) =
  List.fold_left
    (fun s -> function
      | Mir.PField f -> Printf.sprintf "%s.f_%s" s f
      | Mir.PDeref -> Printf.sprintf "(*%s)" s)
    (local f p.base) p.proj

let operand f = function
  | Mir.Const (Mir.CInt n) -> Printf.sprintf "INT64_C(%Ld)" n
  | Mir.Const (Mir.CBool b) -> if b then "true" else "false"
  | Mir.Copy p | Mir.Move p -> place f p
  | Mir.Ref p | Mir.RefMut p -> Printf.sprintf "&%s" (place f p)

let rvalue fn rv =
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
  | Mir.Call (f, args) -> Printf.sprintf "u_%s(%s)" f (String.concat ", " (List.map operand args))
  | Mir.Agg (n, fs) ->
      let fs = List.map (fun (f, o) -> Printf.sprintf ".f_%s = %s" f (operand o)) fs in
      Printf.sprintf "((S_%s){ %s })" n (String.concat ", " fs)
  | Mir.BoxNew (t, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = sn_alloc(sizeof(%s)); *p_ = %s; p_; })" t t (operand o)

(* 从入口能走到的块；走不到的块（return 之后的死代码）不输出。 *)
let reachable (f : Mir.func) =
  let seen = Array.make (Array.length f.blocks) false in
  let rec go i =
    if not seen.(i) then begin
      seen.(i) <- true;
      match f.blocks.(i).term with
      | Mir.Goto j -> go j
      | Mir.Branch (_, a, b) -> go a; go b
      | Mir.Return | Mir.Unterminated -> ()
    end
  in
  go 0;
  seen

(* 局部变量的 C 类型：指针局部多一层 *。 *)
let lty (l : Mir.local_info) = if l.lref then cty l.lty ^ "*" else cty l.lty

let signature (f : Mir.func) =
  let ps =
    if f.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun l -> Printf.sprintf "%s l%d" (lty f.locals.(l)) l) f.params)
  in
  Printf.sprintf "static %s u_%s(%s)" (cty f.ret) f.name ps

let func b (f : Mir.func) =
  let add s = Buffer.add_string b s in
  add (signature f ^ " {\n");
  Array.iteri
    (fun i (l : Mir.local_info) ->
      if not (List.mem i f.params) then
        add (Printf.sprintf "  %s l%d;%s\n" (lty l) i
               (if l.lname = "" then "" else Printf.sprintf "  /* %s */" l.lname)))
    f.locals;
  let seen = reachable f in
  let place = place f and operand = operand f and rvalue = rvalue f in
  Array.iteri
    (fun i (blk : Mir.block) ->
      if seen.(i) then begin
        add (Printf.sprintf "bb%d: ;\n" i);
        List.iter
          (function
            | Mir.Assign (p, rv) -> add (Printf.sprintf "  %s = %s;\n" (place p) (rvalue rv))
            | Mir.Eval rv -> add (Printf.sprintf "  %s;\n" (rvalue rv))
            | Mir.Proj (q, p) -> add (Printf.sprintf "  l%d = &%s;\n" q (place p))
            | Mir.DropNow l ->
                add (Printf.sprintf "  %s\n" (drop_code f.locals.(l).lty (local f l)))
            | Mir.DropIf (l, fl) ->
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code f.locals.(l).lty (local f l)))
            | Mir.Drop _ -> failwith "Drop 应当已由 Own.elaborate 展开")
          blk.stmts;
        match blk.term with
        | Mir.Goto j -> add (Printf.sprintf "  goto bb%d;\n" j)
        | Mir.Branch (o, a, c) ->
            add (Printf.sprintf "  if (%s) goto bb%d; else goto bb%d;\n" (operand o) a c)
        | Mir.Return | Mir.Unterminated -> (
            match f.ret_local with
            | Some r -> add (Printf.sprintf "  return l%d;\n" r)
            | None -> add "  return;\n")
      end)
    f.blocks;
  add "}\n"

let program (p : program) (fs : Mir.func list) : string =
  let b = Buffer.create 4096 in
  let add s = Buffer.add_string b s in
  add prelude;
  add "\n";
  List.iter
    (fun (n, fs) ->
      add (Printf.sprintf "typedef struct S_%s {" n);
      List.iter (fun (f : field) -> add (Printf.sprintf " %s f_%s;" (cty f.fty) f.fname)) fs;
      add (Printf.sprintf " } S_%s;\n" n))
    (struct_order p);
  List.iter (fun f -> add (signature f ^ ";\n")) fs;
  List.iter (func b) fs;
  add
    "int main(void) {\n\
    \  u_main();\n\
    \  if (sn_live != 0) { fprintf(stderr, \"leak: %lld 个 Box 没有释放\\n\", sn_live); return 102; }\n\
    \  return 0;\n\
     }\n";
  Buffer.contents b
