(* C0 的 C 后端。块和 if 用 GCC/Clang 的语句表达式 `({ ... })`；
   求值顺序一律从左到右：有副作用的操作数先存进临时变量（C 本身不规定参数的求值顺序）。 *)
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

let counter = ref 0
let fresh base = incr counter; Printf.sprintf "%s_%d" base !counter

(* 没有副作用、也不会 panic 的表达式，可以直接嵌进 C 表达式。 *)
let rec pure (x : expr) =
  match x.e with
  | Int _ | Bool _ | Unit | Var _ -> true
  | Field (a, _) -> pure a
  | _ -> false

let rec expr env (x : expr) : string =
  match x.e with
  | Int n -> Printf.sprintf "INT64_C(%Ld)" n
  | Bool b -> if b then "true" else "false"
  | Unit -> "((void)0)"
  | Var v -> List.assoc v env
  | Binop (And, a, b) -> Printf.sprintf "(%s && %s)" (expr env a) (expr env b)
  | Binop (Or, a, b) -> Printf.sprintf "(%s || %s)" (expr env a) (expr env b)
  | Binop (op, a, b) ->
      let apply l r =
        match op with
        | Add -> Printf.sprintf "sn_add(%s, %s)" l r
        | Sub -> Printf.sprintf "sn_sub(%s, %s)" l r
        | Mul -> Printf.sprintf "sn_mul(%s, %s)" l r
        | Div -> Printf.sprintf "sn_div(%s, %s)" l r
        | Rem -> Printf.sprintf "sn_rem(%s, %s)" l r
        | Eq -> Printf.sprintf "(%s == %s)" l r
        | Ne -> Printf.sprintf "(%s != %s)" l r
        | Lt -> Printf.sprintf "(%s < %s)" l r
        | Le -> Printf.sprintf "(%s <= %s)" l r
        | Gt -> Printf.sprintf "(%s > %s)" l r
        | Ge -> Printf.sprintf "(%s >= %s)" l r
        | And | Or -> assert false
      in
      in_order env [ a; b ] (function [ l; r ] -> apply l r | _ -> assert false)
  | Unop (Neg, a) -> Printf.sprintf "sn_neg(%s)" (expr env a)
  | Unop (Not, a) -> Printf.sprintf "(!%s)" (expr env a)
  | Call (f, args) ->
      in_order env args (fun xs -> Printf.sprintf "u_%s(%s)" f (String.concat ", " xs))
  | StructLit (n, inits) ->
      in_order env (List.map snd inits) (fun xs ->
          let fs = List.map2 (fun (f, _) v -> Printf.sprintf ".f_%s = %s" f v) inits xs in
          Printf.sprintf "((S_%s){ %s })" n (String.concat ", " fs))
  | Field (a, f) -> Printf.sprintf "(%s).f_%s" (expr env a) f
  | If (c, a, None) ->
      Printf.sprintf "({ if (%s) %s ((void)0); })" (expr env c) (block_stmt env a)
  | If (c, a, Some b) -> (
      match Hashtbl.find_opt Typeck.types x.pos with
      | Some (Typeck.Ty t) when t <> TUnit ->
          let tmp = fresh "t" in
          Printf.sprintf "({ %s %s; if (%s) %s else %s %s; })" (cty t) tmp (expr env c)
            (block_into env tmp a) (block_into env tmp b) tmp
      | _ ->
          Printf.sprintf "({ if (%s) %s else %s ((void)0); })" (expr env c)
            (block_stmt env a) (block_stmt env b))
  | Block b -> (
      let body, tail = stmts env b in
      match tail with
      | Some t -> Printf.sprintf "({ %s%s; })" body t
      | None -> Printf.sprintf "({ %s((void)0); })" body)

(* 按从左到右的顺序求值：不纯的操作数先存进临时变量。 *)
and in_order env (xs : expr list) (k : string list -> string) : string =
  if List.for_all pure xs then k (List.map (expr env) xs)
  else
    let decls, names =
      List.split
        (List.map
           (fun x ->
             if pure x then ("", expr env x)
             else
               let t = fresh "a" in
               (Printf.sprintf "__auto_type %s = %s; " t (expr env x), t))
           xs)
    in
    Printf.sprintf "({ %s%s; })" (String.concat "" decls) (k names)

(* 块里的语句，返回（语句文本，尾表达式文本）；`let` 依次扩展环境。 *)
and stmts env (b : block) : string * string option =
  let buf = Buffer.create 64 in
  let rec go env = function
    | [] -> Option.map (expr env) b.tail
    | (s : stmt) :: rest -> (
        match s.s with
        | Let (v, _, e) ->
            let cv = fresh ("v_" ^ v) in
            Buffer.add_string buf (Printf.sprintf "__auto_type %s = %s; " cv (expr env e));
            go ((v, cv) :: env) rest
        | Assign (v, e) ->
            Buffer.add_string buf (Printf.sprintf "%s = %s; " (List.assoc v env) (expr env e));
            go env rest
        | While (c, body) ->
            Buffer.add_string buf (Printf.sprintf "while (%s) %s " (expr env c) (block_stmt env body));
            go env rest
        | Return None -> Buffer.add_string buf "return; "; go env rest
        | Return (Some e) ->
            Buffer.add_string buf (Printf.sprintf "return %s; " (expr env e));
            go env rest
        | Expr e ->
            Buffer.add_string buf (Printf.sprintf "%s; " (expr env e));
            go env rest)
  in
  let tail = go env b.stmts in
  (Buffer.contents buf, tail)

(* 块作为 C 语句：尾表达式只求值、丢掉结果。 *)
and block_stmt env b =
  let body, tail = stmts env b in
  match tail with
  | Some t -> Printf.sprintf "{ %s%s; }" body t
  | None -> Printf.sprintf "{ %s}" body

(* 块的值写进临时变量 `tmp`；块以 return 结束时没有尾表达式。 *)
and block_into env tmp b =
  let body, tail = stmts env b in
  match tail with
  | Some t -> Printf.sprintf "{ %s%s = %s; }" body tmp t
  | None -> Printf.sprintf "{ %s}" body

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

let program (p : program) : string =
  counter := 0;
  let b = Buffer.create 1024 in
  let add s = Buffer.add_string b s in
  add prelude;
  add "\n";
  List.iter (fun (n, fs) ->
      add (Printf.sprintf "typedef struct S_%s {" n);
      List.iter (fun (f : field) -> add (Printf.sprintf " %s f_%s;" (cty f.fty) f.fname)) fs;
      add (Printf.sprintf " } S_%s;\n" n))
    (struct_order p);
  let sig_of name params ret =
    let ps =
      if params = [] then "void"
      else String.concat ", " (List.map (fun (x, t) -> Printf.sprintf "%s v_%s" (cty t) x) params)
    in
    Printf.sprintf "static %s u_%s(%s)" (cty ret) name ps
  in
  List.iter (function
      | Fn { name; params; ret; _ } -> add (sig_of name params ret ^ ";\n")
      | Struct _ -> ())
    p;
  List.iter (function
      | Fn { name; params; ret; body; _ } ->
          let env = List.map (fun (x, _) -> (x, "v_" ^ x)) params in
          let s, tail = stmts env body in
          let tail_text =
            match tail, body.tail with
            | Some t, Some te ->
                let never =
                  match te.e with
                  | If _ | Block _ -> Hashtbl.find_opt Typeck.types te.pos = Some Typeck.Never
                  | _ -> false
                in
                if ret = TUnit || never then t ^ "; " else Printf.sprintf "return %s; " t
            | _ -> ""
          in
          add (Printf.sprintf "%s {\n  %s%s\n}\n" (sig_of name params ret) s tail_text)
      | Struct _ -> ())
    p;
  add "int main(void) { u_main(); return 0; }\n";
  Buffer.contents b
