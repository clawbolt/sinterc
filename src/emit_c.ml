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
|}

let cty = function
  | TI64 -> "int64_t"
  | TBool -> "bool"
  | TUnit -> "void"
  | TStruct n -> "S_" ^ n

(* 结构体按依赖顺序输出：按值包含的结构体先定义（类型检查已经排除了成环）。 *)
let struct_order (p : program) =
  let defs = List.filter_map (function Struct s -> Some (s.name, s.fields) | _ -> None) p in
  let out = ref [] in
  let rec visit n =
    if not (List.mem_assoc n !out) then
      match List.assoc_opt n defs with
      | Some fs ->
          List.iter (fun (f : field) -> match f.fty with TStruct m -> visit m | _ -> ()) fs;
          if not (List.mem_assoc n !out) then out := (n, fs) :: !out
      | None -> ()
  in
  List.iter (fun (n, _) -> visit n) defs;
  List.rev !out

(* ---------- 从 MIR 生成 ---------- *)

let place (p : Mir.place) =
  List.fold_left (fun s f -> Printf.sprintf "%s.f_%s" s f) (Printf.sprintf "l%d" p.base) p.proj

let operand = function
  | Mir.Const (Mir.CInt n) -> Printf.sprintf "INT64_C(%Ld)" n
  | Mir.Const (Mir.CBool b) -> if b then "true" else "false"
  | Mir.Copy p -> place p

let rvalue = function
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

let signature (f : Mir.func) =
  let ps =
    if f.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun l -> Printf.sprintf "%s l%d" (cty f.locals.(l).lty) l) f.params)
  in
  Printf.sprintf "static %s u_%s(%s)" (cty f.ret) f.name ps

let func b (f : Mir.func) =
  let add s = Buffer.add_string b s in
  add (signature f ^ " {\n");
  Array.iteri
    (fun i (l : Mir.local_info) ->
      if not (List.mem i f.params) then
        add (Printf.sprintf "  %s l%d;%s\n" (cty l.lty) i
               (if l.lname = "" then "" else Printf.sprintf "  /* %s */" l.lname)))
    f.locals;
  let seen = reachable f in
  Array.iteri
    (fun i (blk : Mir.block) ->
      if seen.(i) then begin
        add (Printf.sprintf "bb%d: ;\n" i);
        List.iter
          (function
            | Mir.Assign (p, rv) -> add (Printf.sprintf "  %s = %s;\n" (place p) (rvalue rv))
            | Mir.Eval rv -> add (Printf.sprintf "  %s;\n" (rvalue rv)))
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
  add "int main(void) { u_main(); return 0; }\n";
  Buffer.contents b
