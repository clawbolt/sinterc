(* C 后端（四）：闭包（Fn 值）、scope 里的任务、Fn 值的调用的辅助函数。 *)
open Ast
open C_names
open C_types

(* Fn(…)（Lean `Closures.Stored`）：每个造闭包的 Fn#… 一个环境结构体（捕获按值存进去）、一个转发函数（把环境的字段按指针
   交给提升出的函数，后面接上调用的实参）、一个析构函数（析构每个捕获，释放环境）；每种 Fn 类型一个 call#…，
   按类型的签名经过 code 调用。let / inout 参数在 C 里是指针，sink 参数是值。 *)
let closures d (g : Tast.generated) =
  let b = Buffer.create 256 in
  let add s = Buffer.add_string b s in
  let pty (m, t) = if m = MSink then cty d t else cty d t ^ " *" in
  let rty r = if r = TUnit then "void" else cty d r in
  let names = List.sort compare (List.map fst g.packs) in
  List.iteri
    (fun k pk ->
      let (c : Tast.closure), t = List.assoc pk g.packs in
      match t with
      | TOFn (pts, r) ->
          let fields = List.mapi (fun i (_, _, t) -> Printf.sprintf " %s c%d;" (cty d t) i) c.caps in
          add (Printf.sprintf "struct sn_env%d {%s };\n" k (if fields = [] then " char c_;" else String.concat "" fields));
          let ps = List.mapi (fun i p -> Printf.sprintf ", %s a%d" (pty p) i) pts in
          let fwd =
            List.mapi (fun i _ -> Printf.sprintf "&x->c%d" i) c.caps @ List.mapi (fun i _ -> Printf.sprintf "a%d" i) pts
          in
          add
            (Printf.sprintf "static %s sn_code%d(void *e%s) { struct sn_env%d *x = e; (void)x; %s%s(%s); }\n" (rty r) k
               (String.concat "" ps) k
               (if r = TUnit then "" else "return ")
               (fname c.cname) (String.concat ", " fwd));
          let drops = List.mapi (fun i (_, _, t) -> drop_code d t (Printf.sprintf "x->c%d" i)) c.caps in
          add
            (Printf.sprintf "static void sn_envdrop%d(void *e) { struct sn_env%d *x = e; (void)x; %ssn_free(e); }\n" k k
               (String.concat "" drops));
          let args = List.mapi (fun i (_, _, t) -> Printf.sprintf "%s c%d" (cty d t) i) c.caps in
          let sets = List.mapi (fun i _ -> Printf.sprintf "x->c%d = c%d; " i i) c.caps in
          add
            (Printf.sprintf
               "static sn_fn %s(%s) { struct sn_env%d *x = sn_alloc(sizeof(struct sn_env%d)); %sreturn (sn_fn){ x, (void (*)(void))sn_code%d, sn_envdrop%d }; }\n"
               (fname pk)
               (if args = [] then "void" else String.concat ", " args)
               k k (String.concat "" sets) k k)
      | _ -> ())
    names;
  (* s.spawn(|| …)（Lean `Concurrency.Scope`）：每个 spawn#… 一个环境结构体（只读共享的捕获存指针，移进任务的存值）、
     一个在新线程里运行的函数（按提升出的函数的约定交出捕获，再释放环境），spawn#… 本身填好环境交给 sn_spawn *)
  let tasks = List.sort compare g.spawns in
  List.iteri
    (fun k (sp, (c : Tast.closure)) ->
      let fields = List.mapi (fun i (m, _, t) -> Printf.sprintf " %s c%d;" (pty (m, t)) i) c.caps in
      add (Printf.sprintf "struct sn_tenv%d {%s };\n" k (if fields = [] then " char c_;" else String.concat "" fields));
      add
        (Printf.sprintf "static void sn_task%d(void *e) { struct sn_tenv%d *x = e; (void)x; %s(%s); free(e); }\n" k k
           (fname c.cname)
           (String.concat ", " (List.mapi (fun i _ -> Printf.sprintf "x->c%d" i) c.caps)));
      let ps = List.mapi (fun i (m, _, t) -> Printf.sprintf ", %s c%d" (pty (m, t)) i) c.caps in
      let sets = List.mapi (fun i _ -> Printf.sprintf "x->c%d = c%d; " i i) c.caps in
      add
        (Printf.sprintf
           "static void %s(int64_t *s%s) { struct sn_tenv%d *x = malloc(sizeof(struct sn_tenv%d)); if (!x) sn_panic(\"内存不足\"); %ssn_spawn(*s, sn_task%d, x); }\n"
           (fname sp) (String.concat "" ps) k k (String.concat "" sets) k))
    tasks;
  let calls = List.sort compare g.fcalls in
  List.iter
    (fun (n, t) ->
      match t with
      | TOFn (pts, r) ->
          let ps = List.mapi (fun i p -> Printf.sprintf ", %s a%d" (pty p) i) pts in
          let cast = Printf.sprintf "(%s (*)(void *%s))" (rty r) (String.concat "" (List.map (fun p -> ", " ^ pty p) pts)) in
          add
            (Printf.sprintf "static %s %s(sn_fn *f%s) { %s(%sf->code)(f->env%s); }\n" (rty r) (fname n) (String.concat "" ps)
               (if r = TUnit then "" else "return ")
               cast
               (String.concat "" (List.mapi (fun i _ -> Printf.sprintf ", a%d" i) pts)))
      | _ -> ())
    calls;
  Buffer.contents b
