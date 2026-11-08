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
/* 对照测试用：按 sn_seq 里的 0 / 1 依次返回假 / 真，用完之后一直返回假（循环因此一定结束）。 */
static const char *sn_seq = "";
static bool u_choose(void) { if (*sn_seq) return *sn_seq++ == '1'; return false; }
/* 原型阶段统计还没释放的 Box，程序结束时检查有没有泄漏。 */
static long long sn_live = 0;
static void *sn_alloc(size_t n) { void *p = malloc(n); if (!p) sn_panic("内存不足"); sn_live++; return p; }
static void sn_free(void *p) { free(p); sn_live--; }
static void *sn_realloc(void *p, size_t n) { if (!p) return sn_alloc(n); void *q = realloc(p, n); if (!q) sn_panic("内存不足"); return q; }
/* Vec 的下标检查：越界时 panic，不读内存。 */
static int64_t sn_idx(int64_t n, int64_t i) { if (i < 0 || i >= n) sn_panic("下标越界"); return i; }
/* 区域：一串内存块，分配时往当前块后面追加，离开 arena 块时整块释放。内存块也计入泄漏检查。 */
typedef struct sn_chunk { struct sn_chunk *next; size_t used, cap; _Alignas(16) char data[]; } sn_chunk;
typedef struct { sn_chunk *head; } sn_arena;
static void *sn_arena_alloc(sn_arena *a, size_t n) {
  n = (n + 15) & ~(size_t)15;
  if (!a->head || a->head->used + n > a->head->cap) {
    size_t cap = n > 4096 ? n : 4096;
    sn_chunk *c = sn_alloc(sizeof(sn_chunk) + cap);
    c->next = a->head; c->used = 0; c->cap = cap; a->head = c;
  }
  void *p = a->head->data + a->head->used;
  a->head->used += n;
  return p;
}
static void sn_arena_free(sn_arena *a) { sn_chunk *c = a->head; while (c) { sn_chunk *n = c->next; sn_free(c); c = n; } a->head = 0; }
|}

(* 泛型实例（名字里有 <）的 C 名字：编号加上去掉符号的写法，和用户写的名字（S_、u_ 开头）不会重名。 *)
let gen_names : (string, string) Hashtbl.t = Hashtbl.create 16

let cname prefix n =
  if not (String.contains n '<') then prefix ^ n
  else
    match Hashtbl.find_opt gen_names n with
    | Some c -> c
    | None ->
        let clean = String.map (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '_') n in
        let c = Printf.sprintf "g%d_%s" (Hashtbl.length gen_names) clean in
        Hashtbl.add gen_names n c;
        c

let sname n = cname "S_" n   (* 结构体、枚举 *)
let fname n = cname "u_" n   (* 函数 *)

(* Result<T, E> 的 C 名字由两个类型拼出来（每个用到的 Result 是一个具体的枚举）。 *)
let rec mangle = function
  | TI64 -> "I64"
  | TBool -> "Bool"
  | TUnit -> "U"
  | TStruct n -> if String.contains n '<' then sname n else n
  | TBox t -> "B" ^ mangle t
  | TVec t -> "V" ^ mangle t ^ "_"
  | TResult (a, b) -> "R" ^ mangle a ^ "_" ^ mangle b ^ "_"
  | TRef (t, _) -> "P" ^ mangle t
  | TArena _ -> "A"
  | TApp _ | TParam _ -> failwith "泛型的类型参数不会出现在生成的 C 里"

let rec cty = function
  | TI64 -> "int64_t"
  | TBool -> "bool"
  | TUnit -> "void"
  | TStruct n -> sname n
  | TBox t -> cty t ^ "*"
  | TResult _ as t -> "S_" ^ mangle t
  | TVec _ as t -> "S_" ^ mangle t
  | TRef (t, _) -> cty t ^ "*"
  | TArena _ -> "sn_arena"
  | TApp _ | TParam _ -> failwith "泛型的类型参数不会出现在生成的 C 里"

(* 析构一个值：先析构 Box 里面的值，再释放 Box；结构体、枚举（含 Result）调用这个类型的析构函数。
   析构函数每个类型一个（type_defs 生成），所以经过 Box 的递归类型（链表、树）的析构是递归调用（Lean M11）。 *)
let drop_fn t = "sn_drop_" ^ cty t

let rec drop_code ty lv =
  match ty with
  | TBox t -> drop_code t ("(*" ^ lv ^ ")") ^ Printf.sprintf "sn_free(%s); " lv
  | (TStruct _ | TResult _ | TVec _) when Typeck.needs_drop ty -> Printf.sprintf "%s(&%s); " (drop_fn ty) lv
  | TI64 | TBool | TUnit | TRef _ | TArena _ | TStruct _ | TResult _ | TVec _ | TApp _ | TParam _ -> ""

(* 析构函数的函数体：结构体逐个析构字段；枚举按运行时标签析构对应变体的载荷（Lean M8 的 dropOne）；
   Vec 逐个析构元素，再释放缓冲区（Lean M12）。 *)
let drop_body ty lv =
  match ty, Typeck.variants ty with
  | _, Some vs ->
      let cases =
        List.concat
          (List.mapi
             (fun i (_, t) ->
               match t with
               | Some t ->
                   let d = drop_code t (Printf.sprintf "%s.u.v%d" lv i) in
                   if d = "" then [] else [ Printf.sprintf "case %d: %sbreak; " i d ]
               | None -> [])
             vs)
      in
      if cases = [] then "" else Printf.sprintf "switch (%s.tag) { %sdefault: break; } " lv (String.concat "" cases)
  | TVec t, _ ->
      Printf.sprintf "for (int64_t i_ = 0; i_ < %s.len; i_++) { %s} if (%s.data) sn_free(%s.data); " lv
        (drop_code t (lv ^ ".data[i_]")) lv lv
  | TStruct n, None ->
      String.concat ""
        (List.map (fun (f : field) -> drop_code f.fty (lv ^ ".f_" ^ f.fname)) (Hashtbl.find Typeck.struct_tbl n))
  | _ -> ""

(* Vec 内建函数的辅助函数名。 *)
let vec_fn b e = Printf.sprintf "sn_v%s_%s" b (cty (TVec e))

(* 类型定义：先给所有结构体、枚举写前向声明，再按依赖顺序写定义（按值包含的类型先定义；
   Box 指向的类型只需要前向声明，放到后面再处理，所以经过 Box 的递归没有问题）；
   最后是需要析构的类型的析构函数（先写原型，析构函数之间可以互相调用）。 *)
let type_defs (p : program) (fs : Mir.func list) =
  let fwd = Buffer.create 256 and defs = Buffer.create 1024 in
  let protos = Buffer.create 256 and bodies = Buffer.create 1024 in
  let seen = Hashtbl.create 16 in
  let later = Queue.create () in
  let declare t =
    Hashtbl.replace seen (cty t) ();
    Buffer.add_string fwd (Printf.sprintf "typedef struct %s %s;\n" (cty t) (cty t));
    if Typeck.needs_drop t then begin
      Buffer.add_string protos (Printf.sprintf "static void %s(%s *p);\n" (drop_fn t) (cty t));
      Buffer.add_string bodies
        (Printf.sprintf "static void %s(%s *p) { %s}\n" (drop_fn t) (cty t) (drop_body t "(*p)"))
    end
  in
  let rec visit t =
    match t, Typeck.variants t with
    | (TBox t | TRef (t, _)), _ -> Queue.add t later
    | (TStruct _ | TResult _ | TVec _), _ when Hashtbl.mem seen (cty t) -> ()
    | TVec e, _ ->
        (* 动态数组：缓冲区指针、长度、容量；元素类型只需要前向声明 *)
        declare t;
        Queue.add e later;
        Buffer.add_string defs
          (Printf.sprintf "struct %s { %s *data; int64_t len, cap; };\n" (cty t) (cty e))
    | _, Some vs ->
        (* 枚举（含 Result）：标签加上各个载荷的 union *)
        declare t;
        List.iter (fun (_, t) -> Option.iter visit t) vs;
        let pays =
          List.concat
            (List.mapi
               (fun i (_, t) ->
                 match t with Some t -> [ Printf.sprintf " %s v%d;" (cty t) i ] | None -> [])
               vs)
        in
        Buffer.add_string defs
          (Printf.sprintf "struct %s { int64_t tag;%s };\n" (cty t)
             (if pays = [] then "" else Printf.sprintf " union {%s } u;" (String.concat "" pays)))
    | TStruct n, _ ->
        declare t;
        let fields = Hashtbl.find Typeck.struct_tbl n in
        List.iter (fun (f : field) -> visit f.fty) fields;
        Buffer.add_string defs (Printf.sprintf "struct %s {" (sname n));
        List.iter
          (fun (f : field) -> Buffer.add_string defs (Printf.sprintf " %s f_%s;" (cty f.fty) f.fname))
          fields;
        Buffer.add_string defs " };\n"
    | (TI64 | TBool | TUnit | TArena _ | TResult _ | TApp _ | TParam _), _ -> ()
  in
  List.iter
    (function
      | Struct { name; tparams = []; _ } | Enum { name; tparams = []; _ } -> visit (TStruct name)
      | Struct _ | Enum _ | Fn _ | Trait _ | Impl _ -> ())   (* 泛型定义本身没有 C 类型，用到的实例从函数的局部变量找到 *)
    p;
  List.iter
    (fun (f : Mir.func) ->
      visit f.ret;
      Array.iter (fun (l : Mir.local_info) -> visit l.lty) f.locals)
    fs;
  (* Vec 内建函数的辅助函数用到的类型（vec_pop 返回 Option<T>） *)
  Hashtbl.iter
    (fun _ (b, e) ->
      visit (TVec e);
      if b = "pop" then visit (Typeck.instantiate "Option" [ e ]))
    Typeck.vec_calls;
  while not (Queue.is_empty later) do
    visit (Queue.pop later)
  done;
  (* Vec 内建函数：每个用到的（函数, 元素类型）一个辅助函数。push 满了就把容量翻倍（realloc），
     pop 空的时候返回 .None。 *)
  let helpers = Buffer.create 256 in
  Hashtbl.iter
    (fun _ (b, e) ->
      let v = cty (TVec e) and te = cty e in
      Buffer.add_string helpers
        (match b with
        | "new" -> Printf.sprintf "static %s %s(void) { return (%s){ 0, 0, 0 }; }\n" v (vec_fn b e) v
        | "push" ->
            Printf.sprintf
              "static void %s(%s *v, %s x) { if (v->len == v->cap) { int64_t c = v->cap ? v->cap * 2 : 4; v->data = sn_realloc(v->data, (size_t)c * sizeof(%s)); v->cap = c; } v->data[v->len++] = x; }\n"
              (vec_fn b e) v te te
        | "pop" ->
            let o = cty (Typeck.instantiate "Option" [ e ]) in
            Printf.sprintf
              "static %s %s(%s *v) { if (v->len == 0) return (%s){ .tag = 1 }; v->len--; return (%s){ .tag = 0, .u.v0 = v->data[v->len] }; }\n"
              o (vec_fn b e) v o o
        | _ -> Printf.sprintf "static int64_t %s(%s *v) { return v->len; }\n" (vec_fn b e) v))
    Typeck.vec_calls;
  Buffer.contents fwd ^ Buffer.contents defs ^ Buffer.contents protos ^ Buffer.contents bodies
  ^ Buffer.contents helpers

(* ---------- 从 MIR 生成 ---------- *)

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
  | Mir.Call (f, args) ->
      let name = match Hashtbl.find_opt Typeck.vec_calls f with Some (b, e) -> vec_fn b e | None -> fname f in
      Printf.sprintf "%s(%s)" name (String.concat ", " (List.map operand args))
  | Mir.Agg (n, fs) ->
      let fs = List.map (fun (f, o) -> Printf.sprintf ".f_%s = %s" f (operand o)) fs in
      Printf.sprintf "((%s){ %s })" (sname n) (String.concat ", " fs)
  | Mir.Unbox (t, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = %s; %s v_ = *p_; sn_free(p_); v_; })" t (operand o) t
  | Mir.BoxNew (t, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = sn_alloc(sizeof(%s)); *p_ = %s; p_; })" t t (operand o)
  | Mir.MkEnum (t, i, o) ->
      let payload = match o with Some o -> Printf.sprintf ", .u.v%d = %s" i (operand o) | None -> "" in
      Printf.sprintf "((%s){ .tag = %d%s })" (cty t) i payload
  | Mir.Payload (o, i) -> Printf.sprintf "%s.u.v%d" (operand o) i
  | Mir.TagIs (o, i) -> Printf.sprintf "(%s.tag == %d)" (operand o) i
  | Mir.Consume _ -> failwith "Consume 只出现在 Eval 里"
  | Mir.Alloc (t, a, o) ->
      let t = cty t in
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
  Printf.sprintf "static %s %s(%s)" (cty f.ret) (fname f.name) ps

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
            | Mir.Eval (Mir.Consume (t, o)) -> add (Printf.sprintf "  %s\n" (drop_code t (operand o)))
            | Mir.Eval (Mir.Payload _) -> ()   (* 只把值标记为已移走，没有代码 *)
            | Mir.Eval rv -> add (Printf.sprintf "  %s;\n" (rvalue rv))
            | Mir.Proj (q, p) -> add (Printf.sprintf "  l%d = &%s;\n" q (place p))
            | Mir.DropNow l ->
                add (Printf.sprintf "  %s\n" (drop_code f.locals.(l).lty (local f l)))
            | Mir.DropIf (l, fl) ->
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code f.locals.(l).lty (local f l)))
            | Mir.DropAt p -> add (Printf.sprintf "  %s\n" (drop_code (Mir.place_ty f p) (place p)))
            | Mir.DropField (l, path) ->
                let p = { (Mir.whole l) with proj = List.map (fun g -> Mir.PField g) path } in
                add (Printf.sprintf "  %s\n" (drop_code (Mir.place_ty f p) (place p)))
            | Mir.DropFieldIf (l, path, fl) ->
                let p = { (Mir.whole l) with proj = List.map (fun g -> Mir.PField g) path } in
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code (Mir.place_ty f p) (place p)))
            | Mir.Live _ | Mir.Raise -> ()
            | Mir.Enter l -> add (Printf.sprintf "  %s.head = 0;\n" (local f l))
            | Mir.Leave l -> add (Printf.sprintf "  sn_arena_free(&%s);\n" (local f l))
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

(* batch：对照测试的函数名。给了的话不调用 u_main，而是每个函数按所有长度不超过 3 的选择序列各跑一遍，
   每次跑完检查有没有没释放的 Box 或区域内存块。 *)
let program ?batch (p : program) (fs : Mir.func list) : string =
  let b = Buffer.create 4096 in
  let add s = Buffer.add_string b s in
  add prelude;
  add "\n";
  add (type_defs p fs);
  List.iter (fun f -> add (signature f ^ ";\n")) fs;
  List.iter (func b) fs;
  (match batch with
  | None ->
      add
        "int main(void) {\n\
        \  u_main();\n\
        \  if (sn_live != 0) { fprintf(stderr, \"leak: %lld 个 Box 没有释放\\n\", sn_live); return 102; }\n\
        \  return 0;\n\
         }\n"
  | Some names ->
      let rec seqs n = if n = 0 then [ "" ] else "" :: List.concat_map (fun s -> [ "1" ^ s; "0" ^ s ]) (seqs (n - 1)) in
      let seqs = List.sort_uniq compare (seqs 3) in
      add
        (Printf.sprintf "static const char *sn_seqs[] = { %s };\n"
           (String.concat ", " (List.map (Printf.sprintf "\"%s\"") seqs)));
      add "typedef void (*sn_test)(void);\n";
      List.iter (fun n -> add (Printf.sprintf "static void t_%s(void) { (void)u_%s(); }\n" n n)) names;
      add
        (Printf.sprintf "static const sn_test sn_tests[] = { %s };\n"
           (String.concat ", " (List.map (( ^ ) "t_") names)));
      add
        (Printf.sprintf "static const char *sn_names[] = { %s };\n"
           (String.concat ", " (List.map (Printf.sprintf "\"%s\"") names)));
      add
        (Printf.sprintf
           "int main(void) {\n\
           \  int n = %d, m = %d, bad = 0;\n\
           \  for (int i = 0; i < n; i++)\n\
           \    for (int s = 0; s < m; s++) {\n\
           \      sn_seq = sn_seqs[s];\n\
           \      sn_tests[i]();\n\
           \      if (sn_live != 0) {\n\
           \        if (bad < 10) printf(\"%%s 在选择序列 \\\"%%s\\\" 下泄漏 %%lld 块\\n\", sn_names[i], sn_seqs[s], sn_live);\n\
           \        bad++; sn_live = 0;\n\
           \      }\n\
           \    }\n\
           \  printf(\"运行：%%d 个 sinterc 接受的程序 × %%d 组选择，泄漏 %%d 次\\n\", n, m, bad);\n\
           \  return bad ? 1 : 0;\n\
            }\n"
           (List.length names) (List.length seqs)));
  Buffer.contents b
