(* 表达式、语句和块的检查（互相递归的一组函数）。每个函数检查一段 Ast，返回它的带类型的语法树（Tast，见 tast.ml）：
   类型写在节点上；调用写的是最终的函数名（泛型实例、特化、提升出的闭包、内建函数的别名）和每个实参的约定；
   match 补全成每个变体一个分支；for 只留选中的展开；scope、errdefer、解构带上降级要的东西。

   检查时还按表达式 id 记两张表，只在类型检查里面用：types（有几处要看之前检查过的子表达式的类型，包括推断泛型调用时先检查过的实参）、
   method_res（方法调用解析出的函数名，复制 Ast 时沿用：特化、对照模式）。泛型调用的类型实参直接传给 mk_call。 *)
open Ast
open Ctx
open Builtin
open Region
open Generics
open Pattern
module T = Tast

(* ---------- 建节点 ---------- *)

let node pos e ty : T.expr = { T.e; ty; pos }

(* 类型是 t 的变量：调用改写时加上的实参（闭包的捕获、scope 的句柄）。 *)
let tvar pos v t = node pos (T.Var v) (Ty t)

(* 没有检查的子表达式：前面的实参不会返回（类型是 Never）时，检查就停在那里，后面的实参不再检查。
   它们只会出现在走不到的代码里，用一个常量占位。 *)
let skipped (a : expr) = node a.pos (T.Lit (T.Int 0L)) Never

(* fn(…) 参数的实参（闭包）在不会生成代码的函数里（泛型函数本身有闭包时、带 fn(…) 参数的函数）：这些函数不降级，占位。 *)
let not_lowered (a : expr) = node a.pos (T.Lit T.Unit) (Ty TUnit)

(* 调用 f(args)：inst 是泛型调用推出的类型实参，节点见 generics.ml 的 call_node。 *)
let mk_call ?inst f (args : T.expr list) : T.expr_desc = call_node f inst args

(* 借自位置 e 的引用的区域名（返回借用和 &p，Lean `Projection.Return`）：e 是变量或它的字段、元素时是这个变量借自的变量
   （拥有值的变量就是它自己）；经过 *r（r 是 &T in w）时是 w；不是位置时是临时值（先存进临时变量，降级时锁住它）。
   锁住哪个局部由 borrow.ml 按降级后的局部确定，这里的名字只用来比较类型（返回的引用要借自签名里写的参数）。 *)
let rec borrow_region env (e : expr) =
  match e.e with
  | Var v -> Option.value (List.assoc_opt v env.cur.bind_regions) ~default:v
  | Field (a, _) | Call ("vec_get", [ a; _ ]) -> borrow_region env a
  | Deref ({ e = Var v; _ } as a) -> (
      match List.assoc_opt v env.vars with Some (TRef (_, w), _) -> w | _ -> borrow_region env a)
  | Deref a -> (
      (* *p.m（p.m 是 &T in w）：借自 w；类型已经检查过（在 types 里） *)
      match Hashtbl.find_opt types a.id with Some (Ty (TRef (_, w))) -> w | _ -> borrow_region env a)
  | _ -> "临时值"

let rec expr env (x : expr) : T.expr =
  let t = expr1 env x in
  Hashtbl.replace types x.id t.T.ty;
  t

(* 按期望类型检查。 *)
and check env want (x : expr) : T.expr =
  let t = check1 env want x in
  Hashtbl.replace types x.id t.T.ty;
  t

and check1 env want (x : expr) : T.expr =
  let mk e ty = node x.pos e ty in
  match x.e with
  | Call (f, (r :: _ as args)) when is_method f -> check1 env want { x with e = Call (method_name env x f r, args) }
  | Variant (v, a) -> (
      let w = vname v in
      let variant_node i payload = mk (T.Variant { variant = v; tag = i; payload }) (Ty want) in
      match variant want v, a with
      | _ when variants want = None -> err x.pos "这里需要 %s，%s 是枚举的变体" (string_of_ty want) w
      | None, _ -> err x.pos "%s 没有变体 %s" (string_of_ty want) w
      | Some (i, Some t), Some a ->
          let a' = check env t a in
          variant_node i (Some a')
      | Some (i, None), None -> variant_node i None
      | Some (i, Some TUnit), None ->
          (* 载荷是 () 时可以省略：Ok 就是 Ok(())，补上 () *)
          variant_node i (Some (mk (T.Lit T.Unit) (Ty TUnit)))
      | Some (_, Some t), None -> err x.pos "%s 带一个 %s 类型的载荷，要写 %s(…)" w (string_of_ty t) w
      | Some (_, None), Some _ -> err x.pos "%s 没有载荷，不能写 %s(…)" w w)
  | If (c, a, Some b) ->
      let c' = expr env c in
      expect env c.pos TBool c'.T.ty;
      let a', ta = block env ~want a in
      expect env x.pos want ta;
      let b', tb = block env ~want b in
      expect env x.pos want tb;
      mk (T.If (c', a', Some b')) (Ty want)
  | Block b ->
      let b', t = block env ~want b in
      mk (T.Block b') t
  | Match (b, s, arms) -> match_ env (Some want) x.pos b s arms
  | PMatch (b, s, arms) -> check env want (pmatch_ env x b s arms)
  | Arena (r, b) -> arena env (Some want) x.pos r b
  | Call ("vec_new", []) -> (
      match want with
      | TVec t ->
          let inst = vec_inst "new" t in
          mk (mk_call ?inst "vec_new" []) (Ty want)
      | _ -> err x.pos "这里需要 %s，Vec.new() 是 Vec" (string_of_ty want))
  | Call (f, args) when not (List.mem f special) ->
      let t = call env (Some want) x f args in
      expect env x.pos want t.T.ty;
      t
  | StructLit (n, inits) ->
      let t = struct_lit env (Some want) x n inits in
      expect env x.pos want t.T.ty;
      t
  | Lambda (ys, body) when (match want with TOFn _ -> true | _ -> false) -> pack env x ys body want
  | _ ->
      let t = expr1 env x in
      expect env x.pos want t.T.ty;
      t

and expr1 env (x : expr) : T.expr =
  let pos = x.pos in
  let mk e ty = node pos e ty in
  match x.e with
  | Call (f, [ { e = Var sv; _ }; lam ]) when is_method f && method_base f = "spawn" && scope_of env sv <> None ->
      spawn env x (Option.get (scope_of env sv)) sv lam
  | Call (f, (r :: _ as args)) when is_method f -> expr1 env { x with e = Call (method_name env x f r, args) }
  | Int n -> mk (T.Lit (T.Int n)) (Ty TI64)
  | Float f -> mk (T.Lit (T.Float f)) (Ty TF64)
  | Char c -> mk (T.Lit (T.Char c)) (Ty TChar)
  | Bool b -> mk (T.Lit (T.Bool b)) (Ty TBool)
  | Unit -> mk (T.Lit T.Unit) (Ty TUnit)
  | Str s -> mk (T.Lit (T.Str s)) (Ty TStr)
  | Var v -> (
      if scope_of env v <> None then err pos "%s 是 scope 的句柄，只能用来启动任务：%s.spawn(|| …)" v v;
      note env v;
      match List.assoc_opt v env.vars with
      | Some (TFn _, _) ->
          err pos "%s 是函数参数，只能调用它（%s(…)），或者作为实参传给 fn(…) 类型的参数" v v
      | Some (t, _) -> mk (T.Var v) (Ty t)
      | None -> err pos "未定义的变量 %s" v)
  | Lambda _ ->
      err pos "无法确定闭包的类型：写在调用的实参里传给 fn(…) 参数，或者写在知道类型的地方（例如 var f: Fn(I64) -> I64 = |x| …）"
  | Binop (op, a, b) -> (
      let a' = expr env a in
      let b' = expr env b in
      let ta = a'.T.ty and tb = b'.T.ty in
      let bin t = mk (T.Binop (op, a', b')) (Ty t) in
      match op with
      (* 算术和大小比较：两边都是 I64，或者都是 F64（不做隐式转换，用 to_f64 / to_i64）；% 只用于 I64 *)
      | Rem -> expect env a.pos TI64 ta; expect env b.pos TI64 tb; bin TI64
      | Add | Sub | Mul | Div ->
          let t = num ta in
          expect env a.pos t ta; expect env b.pos t tb; bin t
      | Lt | Le | Gt | Ge ->
          let t = match ta with Ty TChar -> TChar | _ -> num ta in   (* 字符按码点比较 *)
          expect env a.pos t ta; expect env b.pos t tb; bin TBool
      | Eq | Ne -> (
          match ta, tb with
          | Ty TStr, _ | _, Ty TStr -> err pos "字符串用 a.eq(b) 比较"
          | Ty TBytes, _ | _, Ty TBytes -> err pos "字节串用 a.eq(b) 比较"
          | Ty (TStruct _ | TUnit | TBox _ | TVec _ | TResult _ | TRef _ | TArena _ | TApp _ | TParam _ | TFn _ | TOFn _), _
          | _, Ty (TStruct _ | TUnit | TBox _ | TVec _ | TResult _ | TRef _ | TArena _ | TApp _ | TParam _ | TFn _ | TOFn _) ->
              err pos "只能比较 I64、F64 和 Bool"
          | _ -> ignore (join env pos "比较" ta tb); bin TBool)
      | And | Or -> expect env a.pos TBool ta; expect env b.pos TBool tb; bin TBool)
  | Unop (Neg, a) ->
      let a' = expr env a in
      let t = num a'.T.ty in
      expect env a.pos t a'.T.ty;
      mk (T.Unop (Neg, a')) (Ty t)
  | Unop (Not, a) ->
      let a' = expr env a in
      expect env a.pos TBool a'.T.ty;
      mk (T.Unop (Not, a')) (Ty TBool)
  | Call ("panic", [ a ]) ->
      (* panic(msg)（Lean `FlowGraph.Panic`）：说明按借用读；不会返回，类型是 Never（和 return 一样） *)
      let a' = expr env a in
      expect env a.pos TStr a'.T.ty;
      mk (T.Panic a') Never
  | Call ("panic", _) -> err pos "panic 需要 1 个参数（说明文字）"
  | Call ("os_exit", [ a ]) ->
      (* os_exit(code)：和 panic 一样结束进程、不析构任何值（Lean `FlowGraph.Panic`），退出码是 code 的低 8 位 *)
      let a' = expr env a in
      expect env a.pos TI64 a'.T.ty;
      mk (T.Exit a') Never
  | Call ("os_exit", _) -> err pos "os_exit 需要 1 个参数（退出码）"
  | Call ("box", [ a ]) -> (
      let a' = expr env a in
      match a'.T.ty with
      | Ty t when is_linear t -> err pos "Box 里不能放 linear 值（还不能从 Box 里移出）"
      | Ty t -> mk (T.Box a') (Ty (TBox t))
      | Never -> mk (T.Box a') Never)
  | Call ("consume", [ a ]) -> (
      let a' = expr env a in
      match a'.T.ty with
      | Ty (TStruct n) when Hashtbl.mem linear_tbl n ->
          (* 只能在定义这个 linear 类型的文件里消费：别的模块只能交给它的 pub 函数 *)
          let base = match String.index_opt n '<' with Some i -> String.sub n 0 i | None -> n in
          (match Hashtbl.find_opt (!resolved).Resolved.type_file base with
          | Some fi when fi <> pos.file ->
              let m = match String.rindex_opt base '.' with Some i -> String.sub base 0 i | None -> "" in
              err pos "%s 定义在模块 %s 里，只能在那里 consume（可以让模块 %s 提供一个 pub 的函数来消费它）" n m m
          | _ -> ());
          mk (T.Consume a') (Ty TUnit)
      | Never -> mk (T.Consume a') Never
      | Ty t -> err pos "consume 只能用于 linear 结构体，%s 不是" (string_of_ty t))
  | Call ("unbox", [ a ]) -> (
      (* 取出 Box 里的值：Box 按 sink 交出，堆内存释放（Lean `Datatypes.Recursive` 的 unbox） *)
      let a' = expr env a in
      match a'.T.ty with
      | Ty (TBox t) -> mk (T.Unbox a') (Ty t)
      | Never -> mk (T.Unbox a') Never
      | Ty t -> err pos "unbox 的参数必须是 Box，%s 不是" (string_of_ty t))
  | Call ((("box" | "unbox" | "consume") as f), _) -> err pos "%s 需要 1 个参数" f
  | Call ("alloc", [ a; v ]) -> (
      let a' = expr env a in
      match a'.T.ty with
      | Ty (TArena r) -> (
          let v' = expr env v in
          match v'.T.ty with
          | Never -> mk (T.Alloc (a', v')) Never
          | Ty TUnit -> err v.pos "区域里不能放 ()"
          | Ty t when needs_drop t ->
              err pos "区域里不能放需要析构或 linear 的值 %s：区域结束时不运行析构（D4）" (string_of_ty t)
          | Ty t when region_names t <> [] -> err v.pos "还不支持在区域里存放引用"
          | Ty t -> mk (T.Alloc (a', v')) (Ty (TRef (t, r))))
      | Never -> mk (T.Alloc (a', skipped v)) Never
      | Ty t -> err a.pos "alloc 的第一个参数必须是区域，%s 不是" (string_of_ty t))
  | Call ("alloc", _) -> err pos "alloc 需要 2 个参数：区域和值"
  | Call ("text", [ a; s ]) -> (
      (* text(a, s)：s 交给区域 a，得到整段的切片 &str in a；之后缓冲区不再改动，区域结束时释放（Lean `Strings.Slice`） *)
      let a' = expr env a in
      match a'.T.ty with
      | Ty (TArena r) ->
          let s' = check env TStr s in
          mk (mk_call "text" [ a'; s' ]) (Ty (TRef (TView, r)))
      | Never -> mk (mk_call "text" [ a'; skipped s ]) Never
      | Ty t -> err a.pos "text 的第一个参数必须是区域，%s 不是" (string_of_ty t))
  | Call ("text", _) -> err pos "text 需要 2 个参数：区域和字符串"
  | Call ((("read_file_in" | "read_stdin_in" | "read_chunk") as f), a :: rest) -> (
      (* 流式读入：结果是区域 a 里的切片，和 text 一样 *)
      let a' = expr env a in
      match a'.T.ty with
      | Ty (TArena r) -> (
          let v = TRef (TView, r) in
          match f, rest with
          | "read_file_in", [ p ] ->
              let p' = expr env p in
              expect env p.pos TStr p'.T.ty;
              mk (mk_call f [ a'; p' ]) (Ty (TResult (v, TStr)))
          | "read_stdin_in", [] -> mk (mk_call f [ a' ]) (Ty v)
          | "read_chunk", [ n ] ->
              let n' = expr env n in
              expect env n.pos TI64 n'.T.ty;
              mk (mk_call f [ a'; n' ]) (Ty v)
          | _ -> err pos "%s 的参数个数不对" f)
      | Never -> mk (mk_call f (a' :: List.map skipped rest)) Never
      | Ty t -> err a.pos "%s 的第一个参数必须是区域，%s 不是" f (string_of_ty t))
  | Call ("ref_of", [ a ]) -> (
      (* &p：借自 p 所在变量的只读引用（Lean `Projection.Return` 的 callOwn：就是一个投影，锁住那个变量） *)
      if root a = None then err pos "& 后面必须是变量、字段、元素或解引用（要借用的位置）";
      let a' = expr env a in
      match a'.T.ty with
      | Never -> mk (T.Ref a') Never
      | Ty t when region_names t <> [] -> err pos "还不支持指向引用或区域的引用"
      | Ty TUnit -> err pos "不能引用 ()"
      | Ty t -> mk (T.Ref a') (Ty (TRef (t, borrow_region env a))))
  | Call ("str_view", [ s ]) ->
      (* s.view()：整个字符串的切片，借自 s（Lean `Projection.Return`）：切片活着时 s 被锁住，不能修改、移动、析构 *)
      let s' = expr env s in
      expect env s.pos TStr s'.T.ty;
      mk (mk_call "str_view" [ s' ]) (Ty (TRef (TView, borrow_region env s)))
  | Call (f, v :: rest) when List.mem_assoc f view_sigs -> (
      (* 切片的方法：子切片和原切片在同一个区域里（Lean `Strings.Slice`） *)
      let int (e : expr) =
        let e' = expr env e in
        expect env e.pos TI64 e'.T.ty;
        e'
      in
      let str (e : expr) =
        let e' = expr env e in
        expect env e.pos TStr e'.T.ty;
        e'
      in
      let v' = expr env v in
      let call rest' t = mk (mk_call f (v' :: rest')) (Ty t) in
      match v'.T.ty with
      | Never -> mk (mk_call f (v' :: List.map skipped rest)) Never
      | Ty (TRef (TView, _) as tv) -> (
          match f, rest with
          | "view_len", [] -> call [] TI64
          | "view_byte", [ i ] -> call [ int i ] TI64
          | "view_char_at", [ i ] -> call [ int i ] TChar
          | "view_char_len", [ i ] -> call [ int i ] TI64
          | "view_sub", [ i; j ] ->
              let i' = int i in
              let j' = int j in
              call [ i'; j' ] tv
          | "view_to_str", [] -> call [] TStr
          | "view_eq", [ s ] -> call [ str s ] TBool
          | "view_eq_view", [ w ] -> (
              let w' = expr env w in
              match w'.T.ty with
              | Ty (TRef (TView, _)) | Never -> call [ w' ] TBool
              | Ty t -> err w.pos "eq_view 的参数必须是字符串切片，%s 不是（和 Str 比较用 eq）" (string_of_ty t))
          | "view_find", [ s; i ] ->
              let s' = str s in
              let i' = int i in
              call [ s'; i' ] TI64
          | "view_print_str", [] -> call [] TUnit
          | _ -> err pos "%s 的参数个数不对" f)
      | Ty t -> err v.pos "%s 的第一个参数必须是字符串切片 &str in …，%s 不是" f (string_of_ty t))
  | Call (f, []) when List.mem_assoc f view_sigs -> err pos "%s 的参数个数不对" f
  | Call ("vec_new", []) ->
      err pos "无法确定 Vec.new() 的元素类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方"
  | Call (("vec_push" | "vec_pop" | "vec_len" | "vec_get" | "vec_swap") as f, v :: rest) -> (
      let v' = expr env v in
      let call ?inst rest' t = mk (mk_call ?inst f (v' :: rest')) (Ty t) in
      match v'.T.ty, f, rest with
      | Never, "vec_get", [ i ] -> mk (T.Index (v', skipped i)) Never
      | Never, _, _ -> mk (mk_call f (v' :: List.map skipped rest)) Never
      | Ty (TVec t), "vec_push", [ e ] ->
          inout_place env v;
          let e' = check env t e in
          let inst = vec_inst "push" t in
          call ?inst [ e' ] TUnit
      | Ty (TVec t), "vec_pop", [] ->
          inout_place env v;
          let inst = vec_inst "pop" t in
          call ?inst [] (instantiate "Option" [ t ])
      | Ty (TVec t), "vec_len", [] ->
          let inst = vec_inst "len" t in
          call ?inst [] TI64
      | Ty (TVec t), "vec_swap", [ i; j ] ->
          (* 交换第 i、j 个元素：只调换顺序，拥有关系不变（Lean `Datatypes.VecSwap`），越界时 panic *)
          inout_place env v;
          let i' = expr env i in
          expect env i.pos TI64 i'.T.ty;
          let j' = expr env j in
          expect env j.pos TI64 j'.T.ty;
          let inst = vec_inst "swap" t in
          call ?inst [ i'; j' ] TUnit
      | Ty (TVec t), "vec_get", [ i ] ->
          (* 第 i 个元素这个位置：可复制的值复制，需要析构的值 let 绑定成投影（锁住 v），越界时 panic *)
          let i' = expr env i in
          expect env i.pos TI64 i'.T.ty;
          mk (T.Index (v', i')) (Ty t)
      | Ty (TVec _), _, _ ->
          err pos "%s 需要 %d 个参数" f (match f with "vec_push" | "vec_get" -> 2 | "vec_swap" -> 3 | _ -> 1)
      | Ty t, _, _ -> err v.pos "%s 的第一个参数必须是 Vec，%s 不是" f (string_of_ty t))
  | Call (("vec_new" | "vec_push" | "vec_pop" | "vec_len" | "vec_get" | "vec_swap") as f, _) -> err pos "%s 的参数个数不对" f
  | Deref a -> (
      let a' = expr env a in
      match a'.T.ty with
      | Ty (TRef (TView, _)) -> err pos "字符串切片不能解引用（要 Str 时用 to_str() 复制一份）"
      | Ty (TBox t | TRef (t, _)) -> mk (T.Deref a') (Ty t)
      | Never -> mk (T.Deref a') Never
      | Ty t -> err pos "%s 不是 Box 或引用，不能解引用" (string_of_ty t))
  | Call (f, args) -> call env None x f args
  | StructLit (n, inits) -> struct_lit env None x n inits
  | Field (a, f) -> (
      let a' = expr env a in
      match a'.T.ty with
      | Ty (TStruct n) when Hashtbl.mem enum_tbl n -> err pos "%s 是枚举，不能取字段 %s（用 match）" n f
      | Ty (TStruct n) -> (
          let fs = Hashtbl.find env.structs n in
          match List.find_opt (fun (g : field) -> g.fname = f) fs with
          | Some g -> mk (T.Field (a', f)) (Ty g.fty)
          | None -> err pos "%s 没有字段 %s" n f)
      | Never -> mk (T.Field (a', f)) Never
      | Ty t -> err pos "%s 不是结构体，不能取字段 %s" (string_of_ty t) f)
  | If (c, a, None) ->
      let c' = expr env c in
      expect env c.pos TBool c'.T.ty;
      let a', ta = block env a in
      expect env pos TUnit ta;
      mk (T.If (c', a', None)) (Ty TUnit)
  | If (c, a, Some b) ->
      let c' = expr env c in
      expect env c.pos TBool c'.T.ty;
      (* 先检查 else 分支（和以前一样：报哪个错误、闭包提升的先后都按这个顺序） *)
      let b', tb = block env b in
      let a', ta = block env a in
      mk (T.If (c', a', Some b')) (join env pos "if " ta tb)
  | Block b ->
      let b', t = block env b in
      mk (T.Block b') t
  | Variant (v, _) ->
      err pos "无法确定 %s 的类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方" (vname v)
  | Try a -> (
      no_exit env pos "?";
      let a' = expr env a in
      match a'.T.ty with
      | Never -> mk (T.Try a') Never
      | Ty (TResult (t, e)) -> (
          match env.ret with
          | TResult (_, e') when e = e' -> mk (T.Try a') (Ty t)
          | TResult (_, e') ->
              err pos "? 传出的错误类型是 %s，但函数返回的错误类型是 %s" (string_of_ty e)
                (string_of_ty e')
          | r -> err pos "? 只能用在返回 Result 的函数里（这个函数返回 %s）" (string_of_ty r))
      | Ty t -> err pos "%s 不是 Result，不能用 ?" (string_of_ty t))
  | Match (b, s, arms) -> match_ env None pos b s arms
  | PMatch (b, s, arms) -> expr env (pmatch_ env x b s arms)
  | Arena (r, b) -> arena env None pos r b

(* 函数调用。want：期望的返回类型（知道时用来推断泛型函数的类型实参）。 *)
(* 方法调用 r.f(…) 的函数名：先按接收者的类型找类型自己的方法（Lean `Traits.Inherent`，在定义处查一次，
   实例沿用这里的结果）；是核心部分里的 intrinsic fn 时换成它的内建函数（s.len() 就是 str_len(s)），
   否则找 trait 的方法。f 可能已经被 resolve.ml 换成了带模块前缀的名字，找方法时用最后一段。 *)
and method_name env (x : expr) f (r : expr) =
  let m = String.sub f 1 (String.length f - 1) in
  let base = match String.rindex_opt m '.' with Some i -> String.sub m (i + 1) (String.length m - i - 1) | None -> m in
  let rt = (expr env r).T.ty in
  let g =
    match (match rt with Ty t -> find_inherent base t | Never -> None), rt with
    | Some g, _ -> ( match Hashtbl.find_opt (!resolved).Resolved.prims g with Some p -> p.Resolved.builtin | None -> g)
    | None, Ty (TOFn _ as t) when base = "call" -> fcall_name env.fns t   (* f.call(x)：字段、Vec 元素里的闭包也能这样调用 *)
    | None, Ty t -> trait_method env x t base m
    | None, Never -> m
  in
  Hashtbl.replace method_res x.id g;
  g

(* 落到 trait 的方法（Lean `Traits.Inherent` 的 resolve_tra）：声明了方法 base、接收者类型 t 满足的 trait 恰好一个。
   一个都不满足时：只有一个 trait 有这个方法就用它（调用处报缺 impl），否则报 t 没有这个方法。
   普通函数不是方法：r.f(…) 不会落到函数 f(r, …)，要写进 impl 类型 { … }。
   预导入的 trait（核心部分的 Eq、Ord、Hash）和用户自己的 trait 都有这个方法时，用户的优先，同预导入的名字。 *)
and trait_method env (x : expr) t base m =
  let user l =
    let pre = Resolve.core ^ "." in
    let mine = List.filter (fun (tr, _) -> not (String.starts_with ~prefix:pre tr)) l in
    if mine <> [] then mine else l
  in
  let cands = user (List.rev (Hashtbl.find_all tmethods base)) in
  match user (List.filter (fun (tr, _) -> has_impl env.tbounds tr t) (List.rev (Hashtbl.find_all tmethods base))) with
  | [ (_, k) ] -> k
  | (a, _) :: (b, _) :: _ ->
      err x.pos "%s 的方法 %s 不确定：trait %s 和 %s 都有这个方法，%s 两个都满足" (string_of_ty t) base a b (string_of_ty t)
  | [] -> (
      match cands with
      | [ (_, k) ] -> k
      | _ ->
          if Hashtbl.mem (!resolved).Resolved.statics (type_head t ^ "." ^ base) then
            err x.pos "%s 没有方法 %s（%s 是类型函数：写成 %s.%s(…)）" (string_of_ty t) base base (type_head t) base
          else if Hashtbl.mem env.fns m || List.mem_assoc m builtins then
            err x.pos "%s 没有方法 %s（%s 是普通函数：写成 %s(…)，或者放进 impl %s { … }）" (string_of_ty t) base base base
              (string_of_ty t)
          else err x.pos "%s 没有方法 %s" (string_of_ty t) base)

and call env want (x : expr) f args : T.expr =
  match List.assoc_opt f env.vars with
  | Some (TFn (pts, r), _) ->
      (* 调用 fn(…) 参数：实参按参数约定检查；在特化里换成调用绑定的闭包提升出的函数，前面接上捕获 *)
      note env f;
      if List.length pts <> List.length args then
        err x.pos "%s 需要 %d 个参数，实际给了 %d 个" f (List.length pts) (List.length args);
      let args' =
        List.map2
          (fun (m, p) (a : expr) ->
            let a' = check env p a in
            if m = MInout then inout_place env a;
            a')
          pts args
      in
      let d =
        match if env.cur.concrete then List.assoc_opt f env.clos else None with
        | Some (c, caps) -> mk_call c.cname (List.map (fun (n, t) -> tvar x.pos n t) caps @ args')
        | None ->
            (* 带 fn(…) 参数的函数本身不降级，这个调用不会被用到 *)
            T.Call { callee = f; base = ""; targs = []; modes = List.map fst pts; borrows = []; args = args' }
      in
      node x.pos d (Ty r)
  | Some ((TOFn _ as t), _) ->
      (* 调用 Fn(…) 类型的变量 f：就是 call#…(f, …)，闭包按 inout（`Closures.Stored` 的 env_disjoint） *)
      let g = fcall_name env.fns t in
      call_named env want x g (var_expr x.pos f t :: args)
  | _ -> call_named env want x f args

(* 需要 Fn(pts) -> r 的地方的闭包（Lean `Closures.Stored`）：和当实参传的闭包一样检查、提升，捕获按值存进环境。
   闭包表达式换成 Fn#提升出的函数名(捕获…)。 *)
and pack env (x : expr) ys body want : T.expr =
  match want with
  | TOFn (pts, r) ->
      let c, _ = lambda env x ys body pts (Some r) in
      let lid = !lam_count in
      pack_check x.pos c.caps;
      let pk = "Fn#" ^ c.cname in
      pack_register ~concrete:env.cur.concrete pk c want;
      requests := Pack (lid, want, pk) :: !requests;
      node x.pos (mk_call pk (List.map (fun (_, v, t) -> tvar x.pos v t) c.caps)) (Ty want)
  | _ -> assert false

(* s.spawn(|| body)（Lean `Concurrency.Scope`）：闭包和当实参传的一样检查、提升成普通函数，捕获按位置分两种：
   scope 外面的变量只读共享（let；记进 fr.shared，降级时整个 scope 锁住它），scope 里面的变量移进任务（sink）。
   调用换成 spawn#提升出的函数名(inout s, 捕获…)，代码生成为它写启动线程的 C 函数。 *)
and spawn env (x : expr) fr sv (lam : expr) : T.expr =
  let ys, body =
    match lam.e with Lambda (ys, b) -> (ys, b) | _ -> err lam.pos "spawn 的参数要写成闭包 || …"
  in
  if ys <> [] then err lam.pos "spawn 的闭包没有参数（写成 || …）";
  let inner = List.length env.vars - fr.souter in   (* env.vars 的前 inner 个是 scope 里面声明的 *)
  let mode v written t =
    let rec idx i = function [] -> max_int | (y, _) :: r -> if y = v then i else idx (i + 1) r in
    if region_names t <> [] then err lam.pos "任务暂不能用含引用或区域的变量 %s" v;
    if idx 0 env.vars < inner then MSink
    else begin
      if written then
        err lam.pos "任务不能修改 scope 外面的变量 %s：它被所有任务只读共享（要修改的值先在 scope 里放进新的变量，再移进任务）" v;
      if not (List.mem v !(fr.shared)) then fr.shared := v :: !(fr.shared);
      MLet
    end
  in
  let c, _ = lambda ~mode env lam [] body [] (Some TUnit) in
  let lid = !lam_count in
  let sp = "spawn#" ^ c.cname in
  spawn_register ~concrete:env.cur.concrete sp c;
  requests := Spawn (lid, sp) :: !requests;
  let args = tvar x.pos sv TI64 :: List.map (fun (_, v, t) -> tvar x.pos v t) c.caps in
  node x.pos (mk_call sp args) (Ty TUnit)

(* 闭包 |ys| body 作 fn(pts) -> r 类型的参数的实参：参数的类型取 pts，在原地检查函数体，找出捕获，再提升（lift）。
   r 为 None 时只推出函数体的类型（泛型函数的类型实参还没确定时），不提升也不记请求。
   提升出的函数之后单独检查，这里检查函数体只为找出捕获和类型，不留它的语法树。
   r 为 Some 时记下 Lam 请求（编号是 !lam_count，调用的地方接着用它）。 *)
and lambda ?mode env (a : expr) ys body pts r : closure * t =
  let pos = a.pos in
  if env.lambda_frame <> None then err pos "闭包里暂不能再写闭包";
  if List.length ys <> List.length pts then
    err pos "闭包有 %d 个参数，这里需要 %d 个" (List.length ys) (List.length pts);
  List.iteri
    (fun i y -> if List.mem y (List.filteri (fun j _ -> j < i) ys) then err pos "闭包的参数 %s 重复" y)
    ys;
  let fr = { outer = List.length env.vars; used = ref []; written = Hashtbl.create 8 } in
  let vars = List.rev (List.map2 (fun y (m, t) -> (y, (t, m <> MLet))) ys pts) @ env.vars in
  (* 闭包体里不在外面的循环里：break / continue 不能跳出闭包 *)
  let env' =
    { env with vars; guards = []; ret = Option.value r ~default:TUnit; lambda_frame = Some fr; loop_depth = 0 }
  in
  let t = (match r with Some r -> check env' r body | None -> expr env' body).T.ty in
  let caps0 =
    List.map
      (fun v ->
        let t, _ = List.assoc v env.vars in
        (match t with
        | TFn _ -> err pos "闭包里暂不能用函数参数 %s（可以把它作为实参直接传下去）" v
        | _ -> ());
        let w = Hashtbl.mem fr.written v in
        ((match mode with Some m -> m v w t | None -> if w then MInout else MLet), v, t))
      !(fr.used)
  in
  let c = lift ~concrete:env.cur.concrete env.regions pos ys body pts r caps0 in
  if not env.cur.concrete then Hashtbl.replace skip_lower env.cur.fn_name ();
  (match r with
  | Some r ->
      incr lam_count;
      let l =
        { lid = !lam_count; lregions = env.regions; lexpr = a; lys = ys; lbody = body; lpts = pts; lret = r; lcaps = caps0 }
      in
      requests := Lam l :: !requests
  | None -> ());
  (c, t)

(* fn(…) 类型的参数 p 的实参：闭包，或者当前函数的同类型 fn(…) 参数（原样传下去）。
   返回绑定的闭包和它的捕获在调用处的实参（闭包写在这里时是外层变量本身，传下去的是这个特化里的捕获参数）。 *)
and fn_arg env f p pts pr (a : expr) : closure * T.expr list =
  let bad () = err a.pos "%s 的这个参数是 %s：要写闭包 |…| …，或者传入同类型的函数参数" f (string_of_ty p) in
  match a.e with
  | Lambda (ys, body) ->
      let c, _ = lambda env a ys body pts (Some pr) in
      (* 闭包写在这里：捕获的实参是外层变量本身（真实的区域；类型检查不再比较，由 `Closures.CapRef` 保证、Lean 复核） *)
      (c, List.map (fun (_, v, _) -> tvar a.pos v (fst (List.assoc v env.vars))) c.caps)
  | Var g -> (
      note env g;
      match List.assoc_opt g env.vars with
      | Some ((TFn _ as t), _) -> (
          if t <> p then err a.pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty p) (string_of_ty t);
          match List.assoc_opt g env.clos with
          | Some (c, caps) -> (c, List.map (fun (n, t) -> tvar a.pos n t) caps)
          | None -> ({ cname = ""; caps = []; rord = [] }, []))
      | Some ((TOFn (qs, u) as t), _) ->
          (* Fn(…) 变量传给 fn(…) 参数：当作闭包 |a…| g(a…)，它按 inout 捕获 g *)
          if TFn (qs, u) <> p then err a.pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty p) (string_of_ty t);
          let ys = List.mapi (fun i _ -> Printf.sprintf "#a%d" i) pts in
          let body = mkx a.pos (Call (g, List.map (fun y -> mkx a.pos (Var y)) ys)) in
          fn_arg env f p pts pr { a with e = Lambda (ys, body) }
      | _ -> bad ())
  | _ -> bad ()

(* 调用带 fn(…) 参数的函数 f：换成调用 f 对这些闭包的特化（spec_register），去掉 fn(…) 参数，后面接上每个闭包的捕获。
   ts：f 是泛型函数时推出的类型实参。args 是检查过的实参（fn(…) 参数的位置是占位，在这里去掉）。 *)
and specialize f ts (args : T.expr list) ps cls : T.expr_desc =
  let keep = List.filteri (fun i _ -> not (is_fn (snd (List.nth ps i)))) args in
  let name = spec_register f ts (List.map fst cls) in
  mk_call name (keep @ List.concat_map snd cls)

and call_named env want (x : expr) f args : T.expr =
  let inst = ref None in   (* 泛型函数：推出的类型实参 *)
  let pos = x.pos in
  let ps, r =
    match Hashtbl.find_opt env.fns f with
    | Some s -> s
    | None -> (
        match List.assoc_opt f builtins with
        | Some (ps, r) ->
            (* 内建函数签名里的 Option<…> 在这里换成实例 *)
            (List.map (fun (m, t) -> (m, resolve [] pos t)) ps, resolve [] pos r)
        | None -> (
            match Hashtbl.find_all tmethods f with
            | (tr, _) :: _ -> err pos "%s 是 trait %s 的方法，要写成方法调用 x.%s(…)" f tr f
            | [] -> err pos "未定义的函数 %s" f))
  in
  if List.length ps <> List.length args then
    err pos "%s 需要 %d 个参数，实际给了 %d 个" (bare f) (List.length ps) (List.length args);
  let has_fn = List.exists (fun (_, p) -> is_fn p) ps in
  (* 泛型函数：从期望的返回类型和实参类型推出类型实参，检查约束（普通的 T 不能是 linear 类型），
     然后按具体类型检查实参；类型实参都是具体类型时登记这个实例（Lean `Generics.Check` 的 Respects） *)
  let ps, r =
    match Hashtbl.find_opt gparams f with
    | None -> (ps, r)
    | Some tps ->
        let s = ref [] in
        Option.iter (fun w -> unify s r w) want;
        (* .A(e) / .B 自己推不出类型，留到类型实参确定之后按期望类型检查；
           fn(…) 参数：传下去的函数参数按它的类型推，闭包等别的实参推完之后再推 *)
        List.iter2
          (fun (_, p) (a : expr) ->
            if List.exists (fun n -> not (List.mem_assoc n !s)) (params_of p) then
              match p, a.e with
              | TFn _, Var g -> (
                  match List.assoc_opt g env.vars with Some (t, _) -> unify s p t | None -> ())
              | TFn _, _ | _, Variant _ -> ()
              | _ -> ( match (expr env a).T.ty with Ty t -> unify s p t | Never -> ()))
          ps args;
        (* 闭包的参数类型由别的实参确定，函数体的类型确定返回类型里的类型参数（例如 map 的 U） *)
        List.iter2
          (fun (_, p) (a : expr) ->
            match p, a.e with
            | TFn (pts, pr), Lambda (ys, body) when List.exists (fun n -> not (List.mem_assoc n !s)) (params_of pr) -> (
                List.iter
                  (fun (_, t) ->
                    List.iter
                      (fun n ->
                        if not (List.mem_assoc n !s) then
                          err a.pos "无法推断闭包参数的类型（%s 的类型参数 %s 要先从别的实参确定）" f n)
                      (params_of t))
                  pts;
                let pts = List.map (fun (m, t) -> (m, subst !s t)) pts in
                match snd (lambda env a ys body pts None) with
                | Ty t -> unify s pr t
                | Never -> ())
            | _ -> ())
          ps args;
        let ts =
          List.map
            (fun (n, lin) ->
              match List.assoc_opt n !s with
              | None -> err pos "无法推断 %s 的类型参数 %s（可以给返回值或实参写类型标注）" f n
              | Some t ->
                  type_arg pos t;
                  if is_linear t && not lin then
                    err pos "%s 的类型参数 %s 没有标 linear，不能是 linear 类型 %s（要写 %s: linear）" f n
                      (string_of_ty t) n;
                  t)
            tps
        in
        (* trait 约束：类型参数要在当前函数里写了同样的约束，具体类型要有 impl（Lean `Traits.Static` 的 RespectsT；
           泛型 impl 按 `Traits.ImplSearch` 递归地查） *)
        (* 返回借用的泛型函数：类型实参还不能含引用（结果会是指向引用的引用） *)
        if Hashtbl.mem let_regions f then
          List.iter
            (fun t ->
              if region_names t <> [] then
                err pos "%s 返回借用，类型实参还不能是含引用的类型 %s" (bare f) (string_of_ty t))
            ts;
        let named = List.combine (List.map fst tps) ts in
        List.iter
          (fun (n, tr) ->
            let t = List.assoc n named in
            if not (has_impl env.tbounds tr t) then
              let tr0, t0 = why_not env.tbounds tr t in
              match Hashtbl.mem methods f, t with
              | _ when t0 <> t -> (
                  (* 泛型 impl 的约束不满足：报最里面那个 *)
                  match t0 with
                  | TParam (m, _) ->
                      err pos "%s 没有写约束 %s，而 %s 的 %s 要用到它（要写 %s: %s）" m tr0 (string_of_ty t) tr m tr0
                  | _ -> err pos "%s 没有实现 %s，而 %s 的 %s 要用到它" (string_of_ty t0) tr0 (string_of_ty t) tr)
              | true, _ when tr = "ToStr" && params_of t = [] ->
                  (* 插值 "{p}"、p.to_str() 落到这里：p 的类型不是内建的那几种，也没有写 to_str *)
                  err pos "%s 没有实现 ToStr，不能在字符串里插值或调用 to_str（先写 fn to_str(x: %s) -> Str）"
                    (string_of_ty t) (string_of_ty t)
              | true, TParam (m, _) -> err pos "%s 没有写约束 %s，不能调用 %s（要写 %s: %s）" m tr (bare f) m tr
              | true, _ -> err pos "%s 没有实现 %s，不能调用 %s" (string_of_ty t) tr (bare f)
              | false, TParam (m, _) -> err pos "%s 要求 %s: %s，但 %s 没有写约束 %s" f n tr m tr
              | false, _ -> err pos "%s 没有实现 %s（%s 要求 %s: %s）" (string_of_ty t) tr f n tr)
          (Option.value (Hashtbl.find_opt gbounds f) ~default:[]);
        (* 含引用的类型实参：类型参数要出现在参数类型里，实例的区域才能从实参得到；
           区域名换成按出现顺序编号的 '0、'1…，不同调用处的同一种实例只生成一份，也不会和函数体里 arena 的名字冲突 *)
        List.iter2
          (fun (n, _) t ->
            if region_names t <> [] && not (List.exists (fun (_, p) -> List.mem n (params_of p)) ps) then
              err pos "%s 的类型参数 %s 只出现在返回类型里，不能是含引用的类型 %s（区域要从实参得到）" f n
                (string_of_ty t))
          tps ts;
        let ts = canon_regions ts in
        inst := Some ts;
        requests := Inst (f, ts, has_fn) :: !requests;
        request_inst f ts ~has_fn;
        let s = List.combine (List.map fst tps) ts in
        (List.map (fun (m, p) -> (m, subst s p)) ps, subst s r)
  in
  (* 区域参数按实参实例化（`Regions.Params`）：Arena 参数取实参的区域，
     参数类型里的其他区域名取实参类型里对应位置的区域 *)
  let s = ref [] in
  let rec bind p t =
    match p, t with
    | (TRef (p, x), TRef (t, y)) ->
        if not (List.mem_assoc x !s) then s := (x, y) :: !s;
        bind p t
    | TArena x, TArena y -> if not (List.mem_assoc x !s) then s := (x, y) :: !s
    | TBox p, TBox t | TVec p, TVec t -> bind p t
    | TResult (a, b), TResult (c, d) -> bind a c; bind b d
    | TStruct a, TStruct b -> (
        match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
        | Some (g, ps), Some (h, ts) when g = h -> List.iter2 bind ps ts
        | _ -> ())
    | _ -> ()
  in
  List.iter2
    (fun (_, p) (a : expr) ->
      if region_names p <> [] then match (expr env a).T.ty with Ty t -> bind p t | Never -> ())
    ps args;
  let s = !s in
  (* 返回借用：返回类型里的 let 参数名换成借自的实参的区域名 *)
  let s =
    s
    @ List.filter_map
        (fun (m, i) -> if List.mem_assoc m s then None else Some (m, borrow_region env (List.nth args i)))
        (Option.value (Hashtbl.find_opt let_regions f) ~default:[])
  in
  let cls = ref [] and lams = ref [] in
  let args' =
    List.map2
      (fun (m, p) (a : expr) ->
        let p = rename s p in
        match p with
        | TFn (pts, pr) ->
            let n0 = !lam_count in
            cls := !cls @ [ fn_arg env f p pts pr a ];
            lams := !lams @ [ (if !lam_count > n0 then !lam_count else 0) ];   (* 0：传下去的 fn(…) 参数，不是闭包 *)
            not_lowered a
        | _ ->
            let a' = check env p a in
            (if m = MInout then begin
               inout_place env a;
               (* inout 实参里的区域不能放宽（不变）：被调用的函数可能往里面写一个活得更短的引用 *)
               match a'.T.ty with
               | Ty t when t <> p ->
                   err a.pos "inout 实参的类型要和参数类型完全相同：需要 %s，实际是 %s（区域不能放宽）"
                     (string_of_ty p) (string_of_ty t)
               | _ -> ()
             end);
            a')
      ps args
  in
  let d =
    if has_fn && env.cur.concrete then specialize f !inst args' ps !cls
    else if has_fn then begin
      (* 不生成代码的函数（泛型函数）里：这个函数不降级。记下 Spec，调用节点的 callee 写成 spec#编号，
         单态化时按实例的闭包换成特化的调用 *)
      Hashtbl.replace skip_lower env.cur.fn_name ();
      incr spec_count;
      requests := Spec (!spec_count, f, !inst, ps, !lams) :: !requests;
      match mk_call ?inst:(!inst) f args' with
      | T.Call c -> T.Call { c with callee = "spec#" ^ string_of_int !spec_count }
      | d -> d
    end
    else mk_call ?inst:(!inst) f args'
  in
  node pos d (Ty (rename s r))

(* 结构体字面量。泛型结构体的类型实参取期望的类型，不知道时从字段的值推出来。字段按写的顺序求值。 *)
and struct_lit env want (x : expr) n inits : T.expr =
  let pos = x.pos in
  let m =
    match Hashtbl.find_opt gtypes n, want with
    | None, _ -> n
    | Some _, Some (TStruct w) when inst_of w = Some n -> w
    | Some (ps, _), _ ->
        let abs = instantiate n (List.map (fun p -> TParam (p, false)) ps) in
        let fs = match abs with TStruct a -> Hashtbl.find_opt env.structs a | _ -> None in
        let fs = match fs with Some fs -> fs | None -> err pos "未知结构体 %s" n in
        let s = ref [] in
        List.iter
          (fun (f : field) ->
            match List.assoc_opt f.fname inits with
            | Some e -> (match (expr env e).T.ty with Ty t -> unify s f.fty t | Never -> ())
            | None -> ())
          fs;
        let ts =
          List.map
            (fun p ->
              match List.assoc_opt p !s with
              | None -> err pos "无法推断 %s 的类型参数 %s（可以写类型标注，例如 let x: %s<…> = …）" n p n
              | Some t ->
                  data_arg pos t;
                  t)
            ps
        in
        (match instantiate n ts with TStruct m -> m | _ -> n)
  in
  let fs =
    match Hashtbl.find_opt env.structs m with
    | Some fs -> fs
    | None -> err pos "未知结构体 %s" n
  in
  let checked =
    List.map
      (fun (f : field) ->
        match List.filter (fun (g, _) -> g = f.fname) inits with
        | [ (_, (e : expr)) ] -> (f.fname, check env f.fty e)
        | [] -> err pos "%s 缺少字段 %s" n f.fname
        | _ -> err pos "字段 %s 重复" f.fname)
      fs
  in
  List.iter
    (fun (g, _) ->
      if not (List.exists (fun (f : field) -> f.fname = g) fs) then
        err pos "%s 没有字段 %s" n g)
    inits;
  node pos (T.StructLit (m, List.map (fun (g, _) -> (g, List.assoc g checked)) inits)) (Ty (TStruct m))

(* arena |r| { … }：r 比所有外层区域都活得短。块的值的类型不能含 r（R3）。 *)
and arena env want pos r (b : block) : T.expr =
  if List.mem_assoc r env.regions then err pos "区域 %s 已经存在（内外层区域不能同名）" r;
  let env' =
    { env with vars = (r, (TArena r, false)) :: env.vars; regions = (r, List.map fst env.regions) :: env.regions }
  in
  let b', t = match want with Some w -> block env' ~want:w b | None -> block env' b in
  (match t with
  | Ty ty when mentions r ty && not !lax_regions ->
      let at = match b.tail with Some e -> e.pos | None -> pos in
      err at "块的值的类型 %s 含有区域 %s，不能带出这个区域（R3）" (string_of_ty ty) r
  | _ -> ());
  node pos (T.Arena (r, b')) t

(* 带嵌套模式的 match：检查模式的类型；覆盖性（Lean `Matching.Coverage`：每个代表值都有分支匹配，每个分支都是某个代表值
   第一个匹配的）；然后展开（Lean `Matching.Nested`）：
     { [let #pN = s]  if test₀ { extract₀ } else if test₁ { extract₁ } … else { extract_last } }
   test 按借用（match let、==），extract 只对选中的分支按移动拆开（match let 时按借用）。返回展开后的表达式，
   调用的地方按它检查类型，得到的语法树就是这个 match 的。 *)
and pmatch_ env (x : expr) borrow (s : expr) arms : expr =
  let pos = x.pos in
  let ty = match (expr env s).T.ty with Ty t -> t | Never -> err s.pos "不能 match 一个不会返回的值" in
  if borrow && root s = None then err s.pos "match let 后面必须是变量、字段或解引用（要借用的位置）";
  (* 借来的参数里的位置不能移走：和 match let 一样按借用拆开 *)
  let borrow =
    borrow || (simple_place s && match root s with Some v -> List.mem v env.cur.let_params | None -> false)
  in
  List.iter
    (fun ((q : pat), _) ->
      check_pat env ty q;
      let vs = pvars q in
      List.iter
        (fun v -> if List.length (List.filter (( = ) v) vs) > 1 then err q.ppos "变量 %s 在同一个模式里出现了两次" v)
        vs)
    arms;
  let keys = List.sort_uniq compare (List.concat_map (fun (q, _) -> pat_lits q) arms) in
  let depth = List.fold_left (fun m (q, _) -> max m (pat_depth q)) 0 arms in
  let rs =
    try reps env keys depth ty
    with Too_many -> err pos "match 的模式太复杂（要检查的情况超过 100000 种），请拆成几层 match"
  in
  (match List.find_opt (fun r -> first_arm arms r = None) rs with
  | Some r -> err pos "match 没有覆盖所有情况，例如 %s" (show_aval r)
  | None -> ());
  List.iteri
    (fun i ((q : pat), _) ->
      if not (List.exists (fun r -> first_arm arms r = Some i) rs) then
        err q.ppos "这个分支永远匹配不到（前面的分支已经覆盖了它能匹配的值）")
    arms;
  let mkp, pre =
    if simple_place s then ((fun () -> copy_place s), [])
    else
      let n = fresh_pvar () in
      ((fun () -> mkx pos (Var n)), [ { s = Let (false, n, None, s); spos = pos } ])
  in
  let rec chain = function
    | [] -> unreachable pos
    | [ (q, e) ] -> pextract env pos borrow ty mkp q e
    | (q, e) :: rest -> (
        let ex = pextract env pos borrow ty mkp q e in
        match ptest env pos ty mkp q with
        | None -> ex
        | Some t -> mkx pos (If (t, { stmts = []; tail = Some ex }, Some { stmts = []; tail = Some (chain rest) })))
  in
  mkx pos (Block { stmts = pre; tail = Some (chain arms) })

(* 枚举（含 Result）的 match（Lean `Datatypes.Enum`）：必须穷尽，每个变体恰好一个分支；带载荷的变体绑定载荷。
   borrow：match let s { … }，s 必须是位置（Lean `Matching.Borrow`）。 *)
and match_ env want pos borrow (s : expr) arms : T.expr =
  let s' = expr env s in
  match s'.T.ty with
  | Never -> err s.pos "不能 match 一个不会返回的值"
  | Ty ty -> (
      match variants ty with
      | None -> err s.pos "%s 不是 Result 或枚举，不能 match" (string_of_ty ty)
      | Some vs ->
          if borrow && root s = None then
            err s.pos "match let 后面必须是变量、字段或解引用（要借用的位置）";
          (* 按借用绑定的载荷借自被匹配的位置所在的变量 *)
          let sr = if root s <> None then borrow_region env s else "临时值" in
          let arms', t = enum_match ~sr env want pos ty vs arms in
          node pos (T.Match { borrow; scrut = s'; arms = arms' }) t)

(* 每个变体一个分支（按变体的顺序），没写的变体用 _ 分支的体；返回分支和 match 的类型。 *)
and enum_match ?(sr = "") env want pos ty vs arms : T.arm list * t =
  let bind x t = env.cur.bind_regions <- (x, if sr <> "" && needs_drop t then sr else x) :: env.cur.bind_regions in
  List.iter
    (fun (v, _, (a : expr)) ->
      if v <> "_" && not (List.mem_assoc v vs) then err a.pos "%s 没有变体 %s" (string_of_ty ty) (vname v))
    arms;
  (* 通配分支 _（Lean `Matching.Wildcard` 的 covers）：最多一个，只能在最后，而且确实还有没写到的变体 *)
  (match List.filter (fun (v, _, _) -> v = "_") arms with
  | _ :: (_, _, (a : expr)) :: _ -> err a.pos "match 有两个 _ 分支"
  | [ (_, _, (a : expr)) ] ->
      (match List.rev arms with (v, _, _) :: _ when v = "_" -> () | _ -> err a.pos "_ 分支只能放在最后");
      if List.for_all (fun (v, _) -> List.exists (fun (u, _, _) -> u = v) arms) vs then
        err a.pos "所有变体都已经写到了，_ 分支是多余的"
  | [] -> ());
  let bodies =
    List.map
      (fun (v, pty) ->
        let w = vname v in
        match List.filter (fun (u, _, _) -> u = v) arms with
        | [ (_, x, (a : expr)) ] -> (
            match pty with
            | Some TUnit when x = "" || x = "_" ->
                (* () 载荷：分支可以写 Ok => …，也可以写 Ok(_) 或 Ok(u) *)
                (env, a)
            | Some t ->
                if x = "" then err a.pos "%s 带载荷，分支要写 %s(x) 或 %s(_)" w w w;
                bind x t;
                ({ env with vars = (x, (t, false)) :: env.vars }, a)
            | None ->
                if x <> "" then err a.pos "%s 没有载荷，分支要写 %s => …" w w;
                (env, a))
        | [] -> (
            (* 没写到的变体用 _ 分支：载荷绑到看不见的变量 _#，分支结束时析构 *)
            match arm_for arms (v, pty) with
            | Some (x, a) when x <> "" ->
                bind x (Option.get pty);
                ({ env with vars = (x, (Option.get pty, false)) :: env.vars }, a)
            | Some (_, a) -> (env, a)
            | None -> err pos "match 没有覆盖 %s（枚举的 match 必须穷尽）" w)
        | _ -> err pos "match 有两个 %s 分支" w)
      vs
  in
  let es, t =
    match want, bodies with
    | Some w, _ -> (List.map (fun (env', a) -> check env' w a) bodies, Ty w)
    | None, (env', a) :: rest ->
        let first = expr env' a in
        let t, es =
          List.fold_left
            (fun (acc, es) (env', a) ->
              let e = expr env' a in
              (join env pos "match " acc e.T.ty, es @ [ e ]))
            (first.T.ty, [ first ]) rest
        in
        (es, t)
    | None, [] -> err pos "空枚举不能 match"
  in
  (* 没写到的几个变体共用 _ 分支的体，每个变体检查一次：都用最后一次检查得到的树
     （展开嵌套模式时生成的变量名按检查的次数编号，这样每个分支里是同一个名字） *)
  let pairs = List.combine (List.map snd bodies) es in
  let last a = snd (List.find (fun (b, _) -> b == a) (List.rev pairs)) in
  (* 分支绑定的变量：写了的分支是写的名字，_ 分支在带载荷（不是 ()）的变体上是 _#（见 arm_for） *)
  (List.map2 (fun (v, pty) (a, _) -> { T.bind = fst (Option.get (arm_for arms (v, pty))); body = last a }) vs pairs, t)

(* want：块的值的期望类型（块最后的表达式按它检查）。返回块的语法树和块的类型。 *)
and block ?want env (b : block) : T.block * t =
  let rec go env = function
    | [] -> (
        match b.tail, want with
        | Some e, Some w ->
            let e' = check env w e in
            ([], Some e', e'.T.ty)
        | Some e, None ->
            let e' = expr env e in
            ([], Some e', e'.T.ty)
        | None, _ -> ([], None, Ty TUnit))
    | (s : stmt) :: rest -> (
        let pos = s.spos in
        (* 这条语句的语法树 d 接在后面语句的前面；后面的语句在 env' 里检查 *)
        let next env' d =
          let ss, tail, t = go env' rest in
          ({ T.s = d; spos = pos } :: ss, tail, t)
        in
        (* return、break、continue 之后的语句走不到，照样检查，块的类型是 Never *)
        let stop d =
          let ss, tail, _ = go env rest in
          ({ T.s = d; spos = pos } :: ss, tail, Never)
        in
        match s.s with
        | Decl (v, ty) ->
            check_ty env pos ty;
            (match ty with
            | TArena _ -> err pos "区域不能存进变量"
            | _ -> ());
            next { env with vars = (v, (ty, true)) :: env.vars } (T.Decl (v, ty))
        | Let (mut, v, ann, e) ->
            let ty, init =
              match ann with
              | Some a ->
                  check_ty env pos a;
                  (a, check env a e)
              | None -> (
                  let e' = expr env e in
                  match e'.T.ty with
                  | Ty t -> (t, e')
                  | Never -> err pos "%s 的初值不会返回，无法推断类型" v)
            in
            (match ty with TArena _ -> err pos "区域不能存进变量" | _ -> ());
            (* let 绑定需要析构的位置是投影（见 mir.ml 的 proj_init）：借自那个位置所在的变量 *)
            env.cur.bind_regions <- (v, if root e <> None && needs_drop ty then borrow_region env e else v) :: env.cur.bind_regions;
            next { env with vars = (v, (ty, mut)) :: env.vars } (T.Let { var = mut; name = v; ty; init })
        | Assign (v, e) ->
            note ~write:true env v;
            let e' =
              match List.assoc_opt v env.vars with
              | Some (t, true) -> check env t e
              | Some (_, false) -> err pos "%s 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）" v
              | None -> err pos "未定义的变量 %s" v
            in
            next env (T.Assign (v, e'))
        | SetField (a, f, e) ->
            (* 给字段赋值：a 是以可修改的变量为根的位置，路径上不经过引用；旧值由 own.ml 先析构 *)
            let rec path (x : expr) =
              match x.e with
              | Var _ -> ()
              | Field (b, _) -> path b
              | Deref b -> (
                  match Hashtbl.find_opt types b.id with
                  | Some (Ty (TBox _)) -> path b
                  | _ -> err x.pos "不能通过引用修改它指向的值")
              | Call (g, [ b; _ ]) when callee x g = "vec_get" -> path b
              | _ -> err x.pos "只能给变量（或它里面的位置）的字段赋值"
            in
            let a' = expr env a in
            let fty =
              match a'.T.ty with
              | Ty (TStruct n) when Hashtbl.mem enum_tbl n -> err a.pos "%s 是枚举，不能给字段 %s 赋值" n f
              | Ty (TStruct n) -> (
                  match List.find_opt (fun (g : field) -> g.fname = f) (Hashtbl.find env.structs n) with
                  | Some g -> g.fty
                  | None -> err pos "%s 没有字段 %s" n f)
              | Ty t -> err a.pos "%s 不是结构体，不能给字段 %s 赋值" (string_of_ty t) f
              | Never -> err a.pos "不能给一个不会返回的值的字段赋值"
            in
            path a;
            Option.iter (note ~write:true env) (root a);
            (match root a with
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err a.pos "%s 不可修改，不能给它的字段赋值（需要 var 变量或 inout / sink 参数）" v)
            | None -> err a.pos "只能给变量（或它里面的位置）的字段赋值");
            if is_linear fty then err pos "字段 %s 是 linear 值，不能直接赋值（旧值没有被消费）" f;
            let e' = check env fty e in
            next env (T.SetField (a', f, e'))
        | SetPlace (a, e) ->
            (* for x in inout v 里的 x = e：a 是 vec_get(v, #j)。和给字段赋值一样：v 以可修改的变量为根，
               路径上不经过引用；旧值由 own.ml 先析构（`Datatypes.VecInout` 的 replace_reach） *)
            let rec path (x : expr) =
              match x.e with
              | Var _ -> ()
              | Field (b, _) -> path b
              | Deref b -> (
                  match Hashtbl.find_opt types b.id with
                  | Some (Ty (TBox _)) -> path b
                  | _ -> err x.pos "不能通过引用修改它指向的值")
              | Call (g, [ b; _ ]) when callee x g = "vec_get" -> path b
              | _ -> err x.pos "只能给变量里的位置赋值"
            in
            let a' = expr env a in
            let t = match a'.T.ty with Ty t -> t | Never -> err a.pos "不能给一个不会返回的值赋值" in
            path a;
            Option.iter (note ~write:true env) (root a);
            (match root a with
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err pos "%s 不可修改，不能给它的元素赋值（需要 var 变量或 inout / sink 参数）" v)
            | None -> err pos "只能给变量里的位置赋值");
            if is_linear t then err pos "元素是 linear 值，不能直接赋值（旧值没有被消费）";
            let e' = check env t e in
            next env (T.SetPlace (a', e'))
        | For (a, sv, si) ->
            (* 按 a 的类型选展开（a 会在选中的展开里再检查一次，结果相同）；语法树里只留选中的展开 *)
            let vec =
              match (expr env a).T.ty with
              | Ty (TVec _) | Never -> true
              | Ty t ->
                  if find_inherent "next" t = None then
                    err a.pos "for 只能遍历 Vec、a..b 或迭代器（有 next 方法的值，例如 s.chars()、m.iter()），%s 不是"
                      (string_of_ty t);
                  false
            in
            go env ((if vec then sv else si) :: rest)
        | While (c, body) ->
            let c' = expr env c in
            expect env c.pos TBool c'.T.ty;
            let body', t = block { env with loop_depth = env.loop_depth + 1 } body in
            expect env pos TUnit t;
            next env (T.While (c', body'))
        | Scope (a, body) ->
            (* scope |s| { … }（Lean `Concurrency.Scope`）：s 只能用来 s.spawn(|| …)；块里不能 return、?，
               也不能 break / continue 到外面的循环（离开 scope 之前一定要等任务结束） *)
            let sname = match a.e with Var s -> s | _ -> assert false in
            let outer = List.length env.vars in
            let fr = { sname; sdepth = outer; souter = outer; shared = ref []; slam = env.lambda_frame <> None } in
            let env' =
              { env with vars = (sname, (TI64, false)) :: env.vars; scopes = fr :: env.scopes; loop_depth = 0 }
            in
            let body', t = block env' body in
            expect env pos TUnit t;
            next env (T.Scope { handle = sname; shared = List.rev !(fr.shared); body = body' })
        | (Break | Continue) as d ->
            if env.loop_depth = 0 then err pos "%s 只能用在循环里" (if d = Break then "break" else "continue");
            stop (if d = Break then T.Break else T.Continue)
        | Return None ->
            no_exit env pos "return";
            expect env pos env.ret (Ty TUnit);
            stop (T.Return None)
        | Return (Some e) ->
            no_exit env pos "return";
            let e' = check env env.ret e in
            stop (T.Return (Some e'))
        | Destruct (n, bs, e) ->
            (* 拆开结构体：每个字段都要写，移进各自的变量；linear 结构体只能 consume *)
            let e' = expr env e in
            let m =
              match e'.T.ty with
              | Ty (TStruct m) when (m = n || inst_of m = Some n) && Hashtbl.mem env.structs m -> m
              | Ty t when Ast.is_tuple n ->
                  err e.pos "这里要拆开的是 %s 个元素的元组，实际是 %s" (String.sub n 6 (String.length n - 6)) (string_of_ty t)
              | Ty t -> err e.pos "这里要拆开的是结构体 %s，实际是 %s" n (string_of_ty t)
              | Never -> err e.pos "不能拆开一个不会返回的值"
            in
            if Hashtbl.mem linear_tbl m then err pos "linear 结构体 %s 不能拆开（要用 consume）" n;
            let fs = Hashtbl.find env.structs m in
            List.iter
              (fun (f, _) -> if not (List.exists (fun (g : field) -> g.fname = f) fs) then err pos "%s 没有字段 %s" n f)
              bs;
            List.iter
              (fun (g : field) ->
                match List.filter (fun (f, _) -> f = g.fname) bs with
                | [ _ ] -> ()
                | [] -> err pos "拆开 %s 时缺少字段 %s（每个字段都要写）" n g.fname
                | _ -> err pos "字段 %s 重复" g.fname)
              fs;
            let fty f = (List.find (fun (g : field) -> g.fname = f) fs).fty in
            let vars = List.fold_left (fun vs (f, x) -> (x, (fty f, false)) :: vs) env.vars bs in
            let fields = List.map (fun (f, x) -> (f, x, fty f)) bs in
            next { env with vars } (T.Destruct { struct_name = n; fields; value = e' })
        | Errdefer e ->
            let v, call = errdefer env pos e in
            if List.mem v env.guards then err pos "%s 已经有一个生效的 errdefer，不能再登记" v;
            next { env with guards = v :: env.guards } (T.Errdefer { call; target = v })
        | Expr e ->
            let e' = expr env e in
            let ss, tail, r = go env rest in
            ({ T.s = T.Expr e'; spos = pos } :: ss, tail, if e'.T.ty = Never then Never else r))
  in
  let stmts, tail, t = go env b.stmts in
  ({ T.stmts; tail }, t)

(* errdefer f(…)：实参只能是变量、字段或常量，并且恰好以 sink 传入一个 linear 变量，
   出错返回时由它消费。返回这个变量和调用的语法树。 *)
and errdefer env pos (e : expr) =
  match e.e with
  | Call (f, _) when (match List.assoc_opt f env.vars with Some (TFn _, _) -> true | _ -> false) ->
      err pos "errdefer 暂不能调用函数参数 %s" f
  | Call (f, args) when f <> "box" ->
      let e' = expr env e in
      let f = callee e f in
      List.iter
        (fun (a : expr) ->
          match a.e with
          | Var _ | Field _ | Deref _ | Int _ | Float _ | Char _ | Bool _ | Unit -> ()
          | _ -> err a.pos "errdefer 的实参只能是变量、字段或常量")
        args;
      let modes = if f = "consume" then [ MSink ] else List.map fst (fst (Hashtbl.find sigs f)) in
      let targets =
        List.filter_map
          (fun (m, (a : expr)) ->
            match m, a.e with
            | MSink, Var v when is_linear (fst (List.assoc v env.vars)) -> Some v
            | _ -> None)
          (List.combine modes args)
      in
      (match targets with
      | [ v ] -> (v, e')
      | _ -> err pos "errdefer 的调用必须以 sink 方式传入恰好一个 linear 变量（出错时由它消费）")
  | _ -> err pos "errdefer 后面必须是一个函数调用"

let mode_name = function MLet -> "let" | MInout -> "inout" | MSink -> "sink"
