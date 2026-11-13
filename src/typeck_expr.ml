(* 表达式、语句和块的检查（互相递归的一组函数）。 *)
open Ast
open Typeck_env
open Typeck_builtins
open Typeck_regions
open Typeck_generics
open Typeck_pattern

(* 不拥有值的变量（let 绑定的位置、match 按借用绑定的载荷、for 的元素）借自哪个变量：变量名 → 区域名，后定义的在前。
   在 check_fn 开始时清空。只用来给引用的类型取名字（遮蔽时可能取错，那样只会多拒绝：真正锁住哪个局部由 borrow.ml 决定） *)
let bind_regions : (string * string) list ref = ref []

(* 借自位置 e 的引用的区域名（返回借用和 &p，Lean `Projection.Return`）：e 是变量或它的字段、元素时是这个变量借自的变量
   （拥有值的变量就是它自己）；经过 *r（r 是 &T in w）时是 w；不是位置时是临时值（先存进临时变量，降级时锁住它）。
   锁住哪个局部由 borrow.ml 按降级后的局部确定，这里的名字只用来比较类型（返回的引用要借自签名里写的参数）。 *)
let rec borrow_region env (e : expr) =
  match e.e with
  | Var v -> Option.value (List.assoc_opt v !bind_regions) ~default:v
  | Field (a, _) | Call ("vec_get", [ a; _ ]) -> borrow_region env a
  | Deref ({ e = Var v; _ } as a) -> (
      match List.assoc_opt v env.vars with Some (TRef (_, w), _) -> w | _ -> borrow_region env a)
  | Deref a -> borrow_region env a
  | _ -> "临时值"

let rec expr env (x : expr) : t =
  let t = expr1 env x in
  Hashtbl.replace types x.id t;
  t

(* 按期望类型检查。 *)
and check env want (x : expr) : t =
  let t = check1 env want x in
  Hashtbl.replace types x.id t;
  t

and check1 env want (x : expr) : t =
  match x.e with
  | Call (f, (r :: _ as args)) when is_method f -> check1 env want { x with e = Call (method_name env x f r, args) }
  | Variant (v, a) -> (
      let w = vname v in
      match variant want v, a with
      | _ when variants want = None -> err x.pos "这里需要 %s，%s 是枚举的变体" (string_of_ty want) w
      | None, _ -> err x.pos "%s 没有变体 %s" (string_of_ty want) w
      | Some (_, Some t), Some a ->
          ignore (check env t a);
          Ty want
      | Some (_, None), None -> Ty want
      | Some (_, Some TUnit), None -> Ty want   (* 载荷是 () 时可以省略：Ok 就是 Ok(()) *)
      | Some (_, Some t), None -> err x.pos "%s 带一个 %s 类型的载荷，要写 %s(…)" w (string_of_ty t) w
      | Some (_, None), Some _ -> err x.pos "%s 没有载荷，不能写 %s(…)" w w)
  | If (c, a, Some b) ->
      expect c.pos TBool (expr env c);
      expect x.pos want (block env ~want a);
      expect x.pos want (block env ~want b);
      Ty want
  | Block b -> block env ~want b
  | Match (b, s, arms) -> match_ env (Some want) x.pos b s arms
  | PMatch (b, s, arms) -> check env want (pmatch_ env x b s arms)
  | Arena (r, b) -> arena env (Some want) x.pos r b
  | Call ("vec_new", []) -> (
      match want with
      | TVec t ->
          vec_inst x "new" t;
          Ty want
      | _ -> err x.pos "这里需要 %s，Vec.new() 是 Vec" (string_of_ty want))
  | Call (f, args) when not (List.mem f special) ->
      let t = call env (Some want) x f args in
      expect x.pos want t;
      t
  | StructLit (n, inits) ->
      let t = struct_lit env (Some want) x n inits in
      expect x.pos want t;
      t
  | Lambda (ys, body) when (match want with TOFn _ -> true | _ -> false) -> pack env x ys body want
  | _ ->
      let t = expr1 env x in
      expect x.pos want t;
      t

and expr1 env (x : expr) : t =
  let pos = x.pos in
  match x.e with
  | Call (f, (r :: _ as args)) when is_method f -> expr1 env { x with e = Call (method_name env x f r, args) }
  | Int _ -> Ty TI64
  | Float _ -> Ty TF64
  | Char _ -> Ty TChar
  | Bool _ -> Ty TBool
  | Unit -> Ty TUnit
  | Str _ -> Ty TStr
  | Var v -> (
      note env.vars v;
      match List.assoc_opt v env.vars with
      | Some (TFn _, _) ->
          err pos "%s 是函数参数，只能调用它（%s(…)），或者作为实参传给 fn(…) 类型的参数" v v
      | Some (t, _) -> Ty t
      | None -> err pos "未定义的变量 %s" v)
  | Lambda _ ->
      err pos "无法确定闭包的类型：写在调用的实参里传给 fn(…) 参数，或者写在知道类型的地方（例如 var f: Fn(I64) -> I64 = |x| …）"
  | Binop (op, a, b) -> (
      let ta = expr env a in
      let tb = expr env b in
      match op with
      (* 算术和大小比较：两边都是 I64，或者都是 F64（不做隐式转换，用 to_f64 / to_i64）；% 只用于 I64 *)
      | Rem -> expect a.pos TI64 ta; expect b.pos TI64 tb; Ty TI64
      | Add | Sub | Mul | Div ->
          let t = num ta in
          expect a.pos t ta; expect b.pos t tb; Ty t
      | Lt | Le | Gt | Ge ->
          let t = match ta with Ty TChar -> TChar | _ -> num ta in   (* 字符按码点比较 *)
          expect a.pos t ta; expect b.pos t tb; Ty TBool
      | Eq | Ne -> (
          match ta, tb with
          | Ty TStr, _ | _, Ty TStr -> err pos "字符串用 a.eq(b) 比较"
          | Ty TBytes, _ | _, Ty TBytes -> err pos "字节串用 a.eq(b) 比较"
          | Ty (TStruct _ | TUnit | TBox _ | TVec _ | TResult _ | TRef _ | TArena _ | TApp _ | TParam _ | TFn _ | TOFn _), _
          | _, Ty (TStruct _ | TUnit | TBox _ | TVec _ | TResult _ | TRef _ | TArena _ | TApp _ | TParam _ | TFn _ | TOFn _) ->
              err pos "只能比较 I64、F64 和 Bool"
          | _ -> ignore (join pos "比较" ta tb); Ty TBool)
      | And | Or -> expect a.pos TBool ta; expect b.pos TBool tb; Ty TBool)
  | Unop (Neg, a) ->
      let ta = expr env a in
      let t = num ta in
      expect a.pos t ta; Ty t
  | Unop (Not, a) -> expect a.pos TBool (expr env a); Ty TBool
  | Call ("panic", [ a ]) ->
      (* panic(msg)（Lean `FlowGraph.Panic`）：说明按借用读；不会返回，类型是 Never（和 return 一样） *)
      expect a.pos TStr (expr env a);
      Never
  | Call ("panic", _) -> err pos "panic 需要 1 个参数（说明文字）"
  | Call ("os_exit", [ a ]) ->
      (* os_exit(code)：和 panic 一样结束进程、不析构任何值（Lean `FlowGraph.Panic`），退出码是 code 的低 8 位 *)
      expect a.pos TI64 (expr env a);
      Never
  | Call ("os_exit", _) -> err pos "os_exit 需要 1 个参数（退出码）"
  | Call ("box", [ a ]) -> (
      match expr env a with
      | Ty t when is_linear t -> err pos "Box 里不能放 linear 值（还不能从 Box 里移出）"
      | Ty t -> Ty (TBox t)
      | Never -> Never)
  | Call ("consume", [ a ]) -> (
      match expr env a with
      | Ty (TStruct n) when Hashtbl.mem linear_tbl n ->
          (* 只能在定义这个 linear 类型的文件里消费：别的模块只能交给它的 pub 函数 *)
          let base = match String.index_opt n '<' with Some i -> String.sub n 0 i | None -> n in
          (match Hashtbl.find_opt Ast.type_file base with
          | Some fi when fi <> Ast.file_of pos ->
              let m = match String.rindex_opt base '.' with Some i -> String.sub base 0 i | None -> "" in
              err pos "%s 定义在模块 %s 里，只能在那里 consume（可以让模块 %s 提供一个 pub 的函数来消费它）" n m m
          | _ -> ());
          Ty TUnit
      | Never -> Never
      | Ty t -> err pos "consume 只能用于 linear 结构体，%s 不是" (string_of_ty t))
  | Call ("unbox", [ a ]) -> (
      (* 取出 Box 里的值：Box 按 sink 交出，堆内存释放（Lean `Datatypes.Recursive` 的 unbox） *)
      match expr env a with
      | Ty (TBox t) -> Ty t
      | Never -> Never
      | Ty t -> err pos "unbox 的参数必须是 Box，%s 不是" (string_of_ty t))
  | Call ((("box" | "unbox" | "consume") as f), _) -> err pos "%s 需要 1 个参数" f
  | Call ("alloc", [ a; v ]) -> (
      match expr env a with
      | Ty (TArena r) -> (
          match expr env v with
          | Never -> Never
          | Ty TUnit -> err v.pos "区域里不能放 ()"
          | Ty t when needs_drop t ->
              err pos "区域里不能放需要析构或 linear 的值 %s：区域结束时不运行析构（D4）" (string_of_ty t)
          | Ty t when region_names t <> [] -> err v.pos "还不支持在区域里存放引用"
          | Ty t -> Ty (TRef (t, r)))
      | Never -> Never
      | Ty t -> err a.pos "alloc 的第一个参数必须是区域，%s 不是" (string_of_ty t))
  | Call ("alloc", _) -> err pos "alloc 需要 2 个参数：区域和值"
  | Call ("text", [ a; s ]) -> (
      (* text(a, s)：s 交给区域 a，得到整段的切片 &str in a；之后缓冲区不再改动，区域结束时释放（Lean `Strings.Slice`） *)
      match expr env a with
      | Ty (TArena r) ->
          ignore (check env TStr s);
          Ty (TRef (TView, r))
      | Never -> Never
      | Ty t -> err a.pos "text 的第一个参数必须是区域，%s 不是" (string_of_ty t))
  | Call ("text", _) -> err pos "text 需要 2 个参数：区域和字符串"
  | Call ((("read_file_in" | "read_stdin_in" | "read_chunk") as f), a :: rest) -> (
      (* 流式读入：结果是区域 a 里的切片，和 text 一样 *)
      match expr env a with
      | Ty (TArena r) -> (
          let v = TRef (TView, r) in
          match f, rest with
          | "read_file_in", [ p ] -> expect p.pos TStr (expr env p); Ty (TResult (v, TStr))
          | "read_stdin_in", [] -> Ty v
          | "read_chunk", [ n ] -> expect n.pos TI64 (expr env n); Ty v
          | _ -> err pos "%s 的参数个数不对" f)
      | Never -> Never
      | Ty t -> err a.pos "%s 的第一个参数必须是区域，%s 不是" f (string_of_ty t))
  | Call ("ref_of", [ a ]) -> (
      (* &p：借自 p 所在变量的只读引用（Lean `Projection.Return` 的 callOwn：就是一个投影，锁住那个变量） *)
      if root a = None then err pos "& 后面必须是变量、字段、元素或解引用（要借用的位置）";
      match expr env a with
      | Never -> Never
      | Ty t when region_names t <> [] -> err pos "还不支持指向引用或区域的引用"
      | Ty TUnit -> err pos "不能引用 ()"
      | Ty t -> Ty (TRef (t, borrow_region env a)))
  | Call ("str_view", [ s ]) ->
      (* s.view()：整个字符串的切片，借自 s（Lean `Projection.Return`）：切片活着时 s 被锁住，不能修改、移动、析构 *)
      expect s.pos TStr (expr env s);
      Ty (TRef (TView, borrow_region env s))
  | Call (f, v :: rest) when List.mem_assoc f view_sigs -> (
      (* 切片的方法：子切片和原切片在同一个区域里（Lean `Strings.Slice`） *)
      let int (e : expr) = expect e.pos TI64 (expr env e) in
      match expr env v with
      | Never -> Never
      | Ty (TRef (TView, _) as tv) -> (
          match f, rest with
          | "view_len", [] -> Ty TI64
          | "view_byte", [ i ] -> int i; Ty TI64
          | "view_sub", [ i; j ] -> int i; int j; Ty tv
          | "view_to_str", [] -> Ty TStr
          | "view_eq", [ s ] -> expect s.pos TStr (expr env s); Ty TBool
          | "view_eq_view", [ w ] -> (
              match expr env w with
              | Ty (TRef (TView, _)) | Never -> Ty TBool
              | Ty t -> err w.pos "eq_view 的参数必须是字符串切片，%s 不是（和 Str 比较用 eq）" (string_of_ty t))
          | "view_find", [ s; i ] -> expect s.pos TStr (expr env s); int i; Ty TI64
          | "view_print_str", [] -> Ty TUnit
          | _ -> err pos "%s 的参数个数不对" f)
      | Ty t -> err v.pos "%s 的第一个参数必须是字符串切片 &str in …，%s 不是" f (string_of_ty t))
  | Call (f, []) when List.mem_assoc f view_sigs -> err pos "%s 的参数个数不对" f
  | Call ("vec_new", []) ->
      err pos "无法确定 Vec.new() 的元素类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方"
  | Call (("vec_push" | "vec_pop" | "vec_len" | "vec_get" | "vec_swap") as f, v :: rest) -> (
      match expr env v, f, rest with
      | Never, _, _ -> Never
      | Ty (TVec t), "vec_push", [ e ] ->
          inout_place env v;
          ignore (check env t e);
          vec_inst x "push" t;
          Ty TUnit
      | Ty (TVec t), "vec_pop", [] ->
          inout_place env v;
          vec_inst x "pop" t;
          Ty (instantiate "Option" [ t ])
      | Ty (TVec t), "vec_len", [] ->
          vec_inst x "len" t;
          Ty TI64
      | Ty (TVec t), "vec_swap", [ i; j ] ->
          (* 交换第 i、j 个元素：只调换顺序，拥有关系不变（Lean `Datatypes.VecSwap`），越界时 panic *)
          inout_place env v;
          expect i.pos TI64 (expr env i);
          expect j.pos TI64 (expr env j);
          vec_inst x "swap" t;
          Ty TUnit
      | Ty (TVec t), "vec_get", [ i ] ->
          (* 第 i 个元素这个位置：可复制的值复制，需要析构的值 let 绑定成投影（锁住 v），越界时 panic *)
          expect i.pos TI64 (expr env i);
          Ty t
      | Ty (TVec _), _, _ ->
          err pos "%s 需要 %d 个参数" f (match f with "vec_push" | "vec_get" -> 2 | "vec_swap" -> 3 | _ -> 1)
      | Ty t, _, _ -> err v.pos "%s 的第一个参数必须是 Vec，%s 不是" f (string_of_ty t))
  | Call (("vec_new" | "vec_push" | "vec_pop" | "vec_len" | "vec_get" | "vec_swap") as f, _) -> err pos "%s 的参数个数不对" f
  | Deref a -> (
      match expr env a with
      | Ty (TRef (TView, _)) -> err pos "字符串切片不能解引用（要 Str 时用 to_str() 复制一份）"
      | Ty (TBox t | TRef (t, _)) -> Ty t
      | Never -> Never
      | Ty t -> err pos "%s 不是 Box 或引用，不能解引用" (string_of_ty t))
  | Call (f, args) -> call env None x f args
  | StructLit (n, inits) -> struct_lit env None x n inits
  | Field (a, f) -> (
      match expr env a with
      | Ty (TStruct n) when Hashtbl.mem enum_tbl n -> err pos "%s 是枚举，不能取字段 %s（用 match）" n f
      | Ty (TStruct n) -> (
          let fs = Hashtbl.find env.structs n in
          match List.find_opt (fun (g : field) -> g.fname = f) fs with
          | Some g -> Ty g.fty
          | None -> err pos "%s 没有字段 %s" n f)
      | Never -> Never
      | Ty t -> err pos "%s 不是结构体，不能取字段 %s" (string_of_ty t) f)
  | If (c, a, None) ->
      expect c.pos TBool (expr env c);
      expect pos TUnit (block env a);
      Ty TUnit
  | If (c, a, Some b) ->
      expect c.pos TBool (expr env c);
      join pos "if " (block env a) (block env b)
  | Block b -> block env b
  | Variant (v, _) ->
      err pos "无法确定 %s 的类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方" (vname v)
  | Try a -> (
      match expr env a with
      | Never -> Never
      | Ty (TResult (t, e)) -> (
          match env.ret with
          | TResult (_, e') when e = e' -> Ty t
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
   否则找 trait 的方法。f 可能已经被 modules.ml 换成了带模块前缀的名字，找方法时用最后一段。 *)
and method_name env (x : expr) f (r : expr) =
  let m = String.sub f 1 (String.length f - 1) in
  let base = match String.rindex_opt m '.' with Some i -> String.sub m (i + 1) (String.length m - i - 1) | None -> m in
  let rt = expr env r in
  let g =
    match (match rt with Ty t -> find_inherent base t | Never -> None), rt with
    | Some g, _ -> ( match Hashtbl.find_opt Ast.prims g with Some (b, _, _, _, _) -> b | None -> g)
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
    let pre = Modules.core ^ "." in
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
          if Hashtbl.mem Ast.statics (type_head t ^ "." ^ base) then
            err x.pos "%s 没有方法 %s（%s 是类型函数：写成 %s.%s(…)）" (string_of_ty t) base base (type_head t) base
          else if Hashtbl.mem env.fns m || List.mem_assoc m builtins then
            err x.pos "%s 没有方法 %s（%s 是普通函数：写成 %s(…)，或者放进 impl %s { … }）" (string_of_ty t) base base base
              (string_of_ty t)
          else err x.pos "%s 没有方法 %s" (string_of_ty t) base)

and call env want (x : expr) f args : t =
  match List.assoc_opt f env.vars with
  | Some (TFn (pts, r), _) ->
      (* 调用 fn(…) 参数：实参按参数约定检查；在特化里换成调用绑定的闭包提升出的函数，前面接上捕获 *)
      note env.vars f;
      if List.length pts <> List.length args then
        err x.pos "%s 需要 %d 个参数，实际给了 %d 个" f (List.length pts) (List.length args);
      List.iter2
        (fun (m, p) (a : expr) ->
          ignore (check env p a);
          if m = MInout then inout_place env a)
        pts args;
      (if !concrete then
         match List.assoc_opt f env.clos with
         | Some (c, caps) ->
             Hashtbl.replace rewrites x.id (c.cname, List.map (fun (n, t) -> var_expr x.pos n t) caps @ args)
         | None -> ());
      Ty r
  | Some ((TOFn _ as t), _) ->
      (* 调用 Fn(…) 类型的变量 f：就是 call#…(f, …)，闭包按 inout（`Closures.Stored` 的 env_disjoint） *)
      let g = fcall_name env.fns t in
      let args = var_expr x.pos f t :: args in
      Hashtbl.replace rewrites x.id (g, args);
      call_named env want x g args
  | _ -> call_named env want x f args

(* 需要 Fn(pts) -> r 的地方的闭包（Lean `Closures.Stored`）：和当实参传的闭包一样检查、提升，捕获按值存进环境。 *)
and pack env (x : expr) ys body want : t =
  match want with
  | TOFn (pts, r) ->
      let c, _ = lambda env x ys body pts (Some r) in
      List.iter
        (fun (_, v, t) ->
          if region_names t <> [] then
            err x.pos "Fn(…) 闭包暂不能用含引用或区域的变量 %s：闭包可以存起来，引用可能活不过它的区域" v;
          if is_linear t then err x.pos "Fn(…) 闭包暂不能用 linear 值 %s" v)
        c.caps;
      let pk = "Fn#" ^ c.cname in
      if not (Hashtbl.mem sigs pk) then Hashtbl.replace sigs pk (List.map (fun (_, _, t) -> (MSink, t)) c.caps, want);
      if !concrete then Hashtbl.replace packs pk (c, want);
      Hashtbl.replace rewrites x.id (pk, List.map (fun (_, v, t) -> var_expr x.pos v t) c.caps);
      Ty want
  | _ -> assert false

(* 闭包 |ys| body 作 fn(pts) -> r 类型的参数的实参：参数的类型取 pts，在原地检查函数体，找出捕获。
   r 为 None 时只推出函数体的类型（泛型函数的类型实参还没确定时）。会生成代码时把闭包提升成普通函数：
   捕获在前、闭包的参数在后；名字按位置和类型确定，同一个闭包只提升一次。 *)
and lambda env (a : expr) ys body pts r : closure * t =
  let pos = a.pos in
  if !lambda_frame <> None then err pos "闭包里暂不能再写闭包";
  if List.length ys <> List.length pts then
    err pos "闭包有 %d 个参数，这里需要 %d 个" (List.length ys) (List.length pts);
  List.iteri
    (fun i y -> if List.mem y (List.filteri (fun j _ -> j < i) ys) then err pos "闭包的参数 %s 重复" y)
    ys;
  let fr = { outer = List.length env.vars; used = ref []; written = Hashtbl.create 8 } in
  let vars = List.rev (List.map2 (fun y (m, t) -> (y, (t, m <> MLet))) ys pts) @ env.vars in
  let env' = { env with vars; guards = []; ret = Option.value r ~default:TUnit } in
  let depth = !loop_depth in
  lambda_frame := Some fr;
  loop_depth := 0;
  let t =
    Fun.protect
      ~finally:(fun () ->
        lambda_frame := None;
        loop_depth := depth)
      (fun () -> match r with Some r -> check env' r body | None -> expr env' body)
  in
  let caps0 =
    List.map
      (fun v ->
        let t, _ = List.assoc v env.vars in
        (match t with
        | TFn _ -> err pos "闭包里暂不能用函数参数 %s（可以把它作为实参直接传下去）" v
        | _ -> ());
        ((if Hashtbl.mem fr.written v then MInout else MLet), v, t))
      !(fr.used)
  in
  (* 捕获的类型里的区域换成区域参数 'c00、'c01…：按这里的嵌套顺序编号（外层在前），
     先后关系只取这里真实成立的（`Closures.CapRef` 的 capture_mono；调用时这些区域都活着是 capture_below） *)
  let outer_first = List.rev_map fst !regions in
  let idx r = Option.value (List.find_index (( = ) r) outer_first) ~default:max_int in
  let rs =
    List.sort_uniq compare (List.concat_map (fun (_, _, t) -> region_names t) caps0)
    |> List.stable_sort (fun a b -> compare (idx a) (idx b))
  in
  if List.length rs > 100 then err pos "闭包捕获的区域太多（最多 100 个）";
  let s = List.mapi (fun k r -> (r, Printf.sprintf "'c%02d" k)) rs in
  let rord = List.map (fun (r, c) -> (c, List.filter_map (fun (r', c') -> if r' <> r && outlives r' r then Some c' else None) s)) s in
  let caps = List.map (fun (m, v, t) -> (m, v, rename s t)) caps0 in
  let file, line = Ast.locate (if Array.length !Ast.files > 0 then !Ast.files.(0) else "") pos in
  let tys = List.map (fun (_, _, t) -> string_of_ty t) caps @ [ string_of_ty (TFn (pts, Option.value r ~default:TUnit)) ] in
  let cname = Printf.sprintf "闭包<%s:%d:%d, %s>" (Filename.basename file) line pos.col (String.concat ", " tys) in
  (match r with
  | Some r when !concrete ->
      if not (Hashtbl.mem requested cname) then begin
        Hashtbl.replace requested cname ();
        let params = caps @ List.map2 (fun y (m, t) -> (m, y, t)) ys pts in
        Hashtbl.replace sigs cname (List.map (fun (m, _, t) -> (m, t)) params, r);
        let b = match body.e with Block b -> b | _ -> { stmts = []; tail = Some body } in
        let body = map_body ~fresh:true (fun _ t -> rename s t) b in
        Queue.add
          (Fn { name = cname; tparams = []; bounds = []; params; ret = r; body; prim = None; ipos = pos }, [], [], rord)
          pending_items
      end
  | _ -> if not !concrete then Hashtbl.replace skip_lower !cur_fn ());
  ({ cname; caps; rord }, t)

(* fn(…) 类型的参数 p 的实参：闭包，或者当前函数的同类型 fn(…) 参数（原样传下去）。
   返回绑定的闭包和它的捕获在调用处的实参（闭包写在这里时是外层变量本身，传下去的是这个特化里的捕获参数）。 *)
and fn_arg env f p pts pr (a : expr) : closure * expr list =
  let bad () = err a.pos "%s 的这个参数是 %s：要写闭包 |…| …，或者传入同类型的函数参数" f (string_of_ty p) in
  match a.e with
  | Lambda (ys, body) ->
      let c, _ = lambda env a ys body pts (Some pr) in
      (* 闭包写在这里：捕获的实参是外层变量本身（真实的区域；类型检查不再比较，由 `Closures.CapRef` 保证、Lean 复核） *)
      (c, List.map (fun v -> var_expr a.pos v (fst (List.assoc v env.vars))) (List.map (fun (_, v, _) -> v) c.caps))
  | Var g -> (
      note env.vars g;
      match List.assoc_opt g env.vars with
      | Some ((TFn _ as t), _) -> (
          if t <> p then err a.pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty p) (string_of_ty t);
          match List.assoc_opt g env.clos with
          | Some (c, caps) -> (c, List.map (fun (n, t) -> var_expr a.pos n t) caps)
          | None -> ({ cname = ""; caps = []; rord = [] }, []))
      | Some ((TOFn (qs, u) as t), _) ->
          (* Fn(…) 变量传给 fn(…) 参数：当作闭包 |a…| g(a…)，它按 inout 捕获 g *)
          if TFn (qs, u) <> p then err a.pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty p) (string_of_ty t);
          let ys = List.mapi (fun i _ -> Printf.sprintf "#a%d" i) pts in
          let body = mkx a.pos (Call (g, List.map (fun y -> mkx a.pos (Var y)) ys)) in
          fn_arg env f p pts pr { a with e = Lambda (ys, body) }
      | _ -> bad ())
  | _ -> bad ()

(* 调用带 fn(…) 参数的函数 f：换成调用 f 对这些闭包的特化（第一次用到时生成，名字是 f[闭包, …]），
   去掉 fn(…) 参数，后面接上每个闭包的捕获（`Closures.Param`：捕获当作额外的实参，排他性在这里一起检查）。 *)
and specialize (x : expr) f args ps cls =
  let ts = Hashtbl.find_opt call_inst x.id in
  let base = match ts with Some ts -> inst_name f ts | None -> f in
  let name = base ^ "[" ^ String.concat ", " (List.map (fun ((c : closure), _) -> c.cname) cls) ^ "]" in
  let keep = List.filteri (fun i _ -> not (is_fn (snd (List.nth ps i)))) args in
  Hashtbl.replace rewrites x.id (name, keep @ List.concat_map snd cls);
  Hashtbl.remove call_inst x.id;
  if not (Hashtbl.mem requested name) then begin
    Hashtbl.replace requested name ();
    match (match ts with Some ts -> instance (f, ts) | None -> Hashtbl.find fn_items f) with
    | Fn g ->
        (* 泛型实例已经复制过；非泛型函数在这里复制（表达式换新的 id） *)
        let body = if ts = None then map_body ~fresh:true (fun _ t -> t) g.body else g.body in
        let fparams = List.filter (fun (_, _, t) -> is_fn t) g.params in
        let binding = List.map2 (fun (_, y, t) (c, _) -> (y, t, c)) fparams cls in
        (* 每个闭包的区域参数加上 @y 区分（两个闭包都有 'c00 时是不同的区域，彼此之间没有顺序） *)
        let ry (c : closure) y = List.map (fun (r, _) -> (r, r ^ "@" ^ y)) c.rord in
        let capsof (y, _, (c : closure)) = List.map (fun (m, v, t) -> (m, v ^ "@" ^ y, rename (ry c y) t)) c.caps in
        let caps = List.concat_map capsof binding in
        let params = List.filter (fun (_, _, t) -> not (is_fn t)) g.params @ caps in
        Hashtbl.replace sigs name (List.map (fun (m, _, t) -> (m, t)) params, g.ret);
        let clos = List.map (fun ((y, _, c) as b) -> (y, (c, List.map (fun (_, v, t) -> (v, t)) (capsof b)))) binding in
        let fvars = List.map (fun (y, t, _) -> (y, t)) binding in
        let rord =
          List.concat_map
            (fun (y, _, (c : closure)) -> List.map (fun (r, os) -> (r ^ "@" ^ y, List.map (fun o -> o ^ "@" ^ y) os)) c.rord)
            binding
        in
        Queue.add (Fn { g with name; params; body; tparams = []; bounds = [] }, clos, fvars, rord) pending_items
    | _ -> assert false
  end

and call_named env want (x : expr) f args : t =
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
              | _ -> ( match expr env a with Ty t -> unify s p t | Never -> ()))
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
        let rs =
          List.fold_left (fun acc r -> if List.mem r acc then acc else acc @ [ r ]) [] (List.concat_map region_names ts)
        in
        let ts = List.map (rename (List.mapi (fun i r -> (r, "'" ^ string_of_int i)) rs)) ts in
        Hashtbl.replace call_inst x.id ts;
        (* 方法调用直接用 impl 里的函数；impl 没写、用默认方法的，或者 impl 是泛型的，生成实例；
           带 fn(…) 参数的函数生成的是特化 *)
        if List.for_all (fun t -> params_of t = []) ts
           && (not has_fn)
           && not (Hashtbl.mem requested (inst_name f ts))
        then begin
          Hashtbl.replace requested (inst_name f ts) ();
          if Hashtbl.mem methods f then method_inst f (List.hd ts) else Queue.add (f, ts) pending
        end;
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
      if region_names p <> [] then match expr env a with Ty t -> bind p t | Never -> ())
    ps args;
  let s = !s in
  (* 返回借用：返回类型里的 let 参数名换成借自的实参的区域名 *)
  let s =
    s
    @ List.filter_map
        (fun (m, i) -> if List.mem_assoc m s then None else Some (m, borrow_region env (List.nth args i)))
        (Option.value (Hashtbl.find_opt let_regions f) ~default:[])
  in
  let cls = ref [] in
  List.iter2
    (fun (m, p) (a : expr) ->
      let p = rename s p in
      match p with
      | TFn (pts, pr) -> cls := !cls @ [ fn_arg env f p pts pr a ]
      | _ ->
          ignore (check env p a);
          if m = MInout then begin
            inout_place env a;
            (* inout 实参里的区域不能放宽（不变）：被调用的函数可能往里面写一个活得更短的引用 *)
            match Hashtbl.find_opt types a.id with
            | Some (Ty t) when t <> p ->
                err a.pos "inout 实参的类型要和参数类型完全相同：需要 %s，实际是 %s（区域不能放宽）"
                  (string_of_ty p) (string_of_ty t)
            | _ -> ()
          end)
    ps args;
  if has_fn then if !concrete then specialize x f args ps !cls else Hashtbl.replace skip_lower !cur_fn ();
  Ty (rename s r)

(* 结构体字面量。泛型结构体的类型实参取期望的类型，不知道时从字段的值推出来。 *)
and struct_lit env want (x : expr) n inits : t =
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
            | Some e -> (match expr env e with Ty t -> unify s f.fty t | Never -> ())
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
  List.iter
    (fun (f : field) ->
      match List.filter (fun (g, _) -> g = f.fname) inits with
      | [ (_, (e : expr)) ] -> ignore (check env f.fty e)
      | [] -> err pos "%s 缺少字段 %s" n f.fname
      | _ -> err pos "字段 %s 重复" f.fname)
    fs;
  List.iter
    (fun (g, _) ->
      if not (List.exists (fun (f : field) -> f.fname = g) fs) then
        err pos "%s 没有字段 %s" n g)
    inits;
  Ty (TStruct m)

(* arena |r| { … }：r 比所有外层区域都活得短。块的值的类型不能含 r（R3）。 *)
and arena env want pos r (b : block) : t =
  if List.mem_assoc r !regions then err pos "区域 %s 已经存在（内外层区域不能同名）" r;
  let saved = !regions in
  regions := (r, List.map fst saved) :: saved;
  let env' = { env with vars = (r, (TArena r, false)) :: env.vars } in
  let t = match want with Some w -> block env' ~want:w b | None -> block env' b in
  regions := saved;
  (match t with
  | Ty ty when mentions r ty && not !lax_regions ->
      let at = match b.tail with Some e -> e.pos | None -> pos in
      err at "块的值的类型 %s 含有区域 %s，不能带出这个区域（R3）" (string_of_ty ty) r
  | _ -> ());
  t

(* 枚举（含 Result）的 match（Lean `Datatypes.Enum`）：必须穷尽，每个变体恰好一个分支；带载荷的变体绑定载荷。
   borrow：match let s { … }，s 必须是位置（Lean `Matching.Borrow`）。 *)
(* 带嵌套模式的 match：检查模式的类型；覆盖性（Lean `Matching.Coverage`：每个代表值都有分支匹配，每个分支都是某个代表值
   第一个匹配的）；然后展开（Lean `Matching.Nested`）：
     { [let #pN = s]  if test₀ { extract₀ } else if test₁ { extract₁ } … else { extract_last } }
   test 按借用（match let、==），extract 只对选中的分支按移动拆开（match let 时按借用）。返回展开后的表达式，
   调用的地方按它检查类型；map_body 再把 PMatch 换成它。 *)
and pmatch_ env (x : expr) borrow (s : expr) arms : expr =
  let pos = x.pos in
  let ty = match expr env s with Ty t -> t | Never -> err s.pos "不能 match 一个不会返回的值" in
  if borrow && root s = None then err s.pos "match let 后面必须是变量、字段或解引用（要借用的位置）";
  (* 借来的参数里的位置不能移走：和 match let 一样按借用拆开 *)
  let borrow =
    borrow || (simple_place s && match root s with Some v -> List.mem v !let_params | None -> false)
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
  let d = mkx pos (Block { stmts = pre; tail = Some (chain arms) }) in
  Hashtbl.replace pdesugar x.id d;
  d

and match_ env want pos borrow (s : expr) arms : t =
  match expr env s with
  | Never -> err s.pos "不能 match 一个不会返回的值"
  | Ty ty -> (
      match variants ty with
      | None -> err s.pos "%s 不是 Result 或枚举，不能 match" (string_of_ty ty)
      | Some vs ->
          if borrow && root s = None then
            err s.pos "match let 后面必须是变量、字段或解引用（要借用的位置）";
          (* 按借用绑定的载荷借自被匹配的位置所在的变量 *)
          let sr = if root s <> None then borrow_region env s else "临时值" in
          enum_match ~sr env want pos ty vs arms)

and enum_match ?(sr = "") env want pos ty vs arms : t =
  let bind x t = bind_regions := (x, if sr <> "" && needs_drop t then sr else x) :: !bind_regions in
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
  match want, bodies with
  | Some w, _ ->
      List.iter (fun (env', a) -> ignore (check env' w a)) bodies;
      Ty w
  | None, (env', a) :: rest ->
      List.fold_left (fun acc (env', a) -> join pos "match " acc (expr env' a)) (expr env' a) rest
  | None, [] -> err pos "空枚举不能 match"

(* want：块的值的期望类型（块最后的表达式按它检查）。 *)
and block ?want env (b : block) : t =
  let rec go env = function
    | [] -> (
        match b.tail, want with
        | Some e, Some w -> check env w e
        | Some e, None -> expr env e
        | None, _ -> Ty TUnit)
    | (s : stmt) :: rest -> (
        let pos = s.spos in
        match s.s with
        | Decl (v, ty) ->
            check_ty env pos ty;
            (match ty with
            | TArena _ -> err pos "区域不能存进变量"
            | _ -> ());
            go { env with vars = (v, (ty, true)) :: env.vars } rest
        | Let (mut, v, ann, e) ->
            let ty =
              match ann with
              | Some a ->
                  check_ty env pos a;
                  ignore (check env a e);
                  a
              | None -> (
                  match expr env e with
                  | Ty t -> t
                  | Never -> err pos "%s 的初值不会返回，无法推断类型" v)
            in
            (match ty with TArena _ -> err pos "区域不能存进变量" | _ -> ());
            (* let 绑定需要析构的位置是投影（见 mir.ml 的 proj_init）：借自那个位置所在的变量 *)
            bind_regions := (v, if root e <> None && needs_drop ty then borrow_region env e else v) :: !bind_regions;
            go { env with vars = (v, (ty, mut)) :: env.vars } rest
        | Assign (v, e) ->
            note ~write:true env.vars v;
            (match List.assoc_opt v env.vars with
            | Some (t, true) -> ignore (check env t e)
            | Some (_, false) -> err pos "%s 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）" v
            | None -> err pos "未定义的变量 %s" v);
            go env rest
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
            let fty =
              match expr env a with
              | Ty (TStruct n) when Hashtbl.mem enum_tbl n -> err a.pos "%s 是枚举，不能给字段 %s 赋值" n f
              | Ty (TStruct n) -> (
                  match List.find_opt (fun (g : field) -> g.fname = f) (Hashtbl.find env.structs n) with
                  | Some g -> g.fty
                  | None -> err pos "%s 没有字段 %s" n f)
              | Ty t -> err a.pos "%s 不是结构体，不能给字段 %s 赋值" (string_of_ty t) f
              | Never -> err a.pos "不能给一个不会返回的值的字段赋值"
            in
            path a;
            Option.iter (note ~write:true env.vars) (root a);
            (match root a with
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err a.pos "%s 不可修改，不能给它的字段赋值（需要 var 变量或 inout / sink 参数）" v)
            | None -> err a.pos "只能给变量（或它里面的位置）的字段赋值");
            if is_linear fty then err pos "字段 %s 是 linear 值，不能直接赋值（旧值没有被消费）" f;
            ignore (check env fty e);
            go env rest
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
            let t = match expr env a with Ty t -> t | Never -> err a.pos "不能给一个不会返回的值赋值" in
            path a;
            Option.iter (note ~write:true env.vars) (root a);
            (match root a with
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err pos "%s 不可修改，不能给它的元素赋值（需要 var 变量或 inout / sink 参数）" v)
            | None -> err pos "只能给变量里的位置赋值");
            if is_linear t then err pos "元素是 linear 值，不能直接赋值（旧值没有被消费）";
            ignore (check env t e);
            go env rest
        | While (c, body) ->
            expect c.pos TBool (expr env c);
            incr loop_depth;
            Fun.protect ~finally:(fun () -> decr loop_depth) (fun () -> expect pos TUnit (block env body));
            go env rest
        | (Break | Continue) as d ->
            (* 之后的语句走不到，和 return 一样记作 Never *)
            if !loop_depth = 0 then err pos "%s 只能用在循环里" (if d = Break then "break" else "continue");
            ignore (go env rest);
            Never
        | Return None ->
            expect pos env.ret (Ty TUnit);
            ignore (go env rest);
            Never
        | Return (Some e) ->
            ignore (check env env.ret e);
            ignore (go env rest);
            Never
        | Destruct (n, bs, e) ->
            (* 拆开结构体：每个字段都要写，移进各自的变量；linear 结构体只能 consume *)
            let m =
              match expr env e with
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
            let vars =
              List.fold_left
                (fun vs (f, x) -> (x, ((List.find (fun (g : field) -> g.fname = f) fs).fty, false)) :: vs)
                env.vars bs
            in
            go { env with vars } rest
        | Errdefer e ->
            let v = errdefer env pos e in
            if List.mem v env.guards then err pos "%s 已经有一个生效的 errdefer，不能再登记" v;
            go { env with guards = v :: env.guards } rest
        | Expr e ->
            let t = expr env e in
            let r = go env rest in
            if t = Never then Never else r)
  in
  go env b.stmts

(* errdefer f(…)：实参只能是变量、字段或常量，并且恰好以 sink 传入一个 linear 变量，
   出错返回时由它消费。返回这个变量。 *)
and errdefer env pos (e : expr) =
  match e.e with
  | Call (f, _) when (match List.assoc_opt f env.vars with Some (TFn _, _) -> true | _ -> false) ->
      err pos "errdefer 暂不能调用函数参数 %s" f
  | Call (f, args) when f <> "box" ->
      ignore (expr env e);
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
      | [ v ] ->
          Hashtbl.replace errdefer_targets e.id v;
          v
      | _ -> err pos "errdefer 的调用必须以 sink 方式传入恰好一个 linear 变量（出错时由它消费）")
  | _ -> err pos "errdefer 后面必须是一个函数调用"

let mode_name = function MLet -> "let" | MInout -> "inout" | MSink -> "sink"

