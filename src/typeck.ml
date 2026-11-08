(* 类型检查：结构体按名字区分，泛型按实例化后的名字区分（见下面的“泛型”）；`return` 之后的代码类型记为 Never。
   `Ok(e)` / `Err(e)` 的类型要靠上下文确定（let 标注、返回值、实参、字段），
   所以有两种检查方式：expr 自己推出类型，check 按期望类型检查。 *)
open Ast

exception Error of pos * string

let err pos fmt = Printf.ksprintf (fun m -> raise (Error (pos, m))) fmt

type t = Ty of ty | Never

type env = {
  structs : (string, field list) Hashtbl.t;
  fns : (string, (mode * ty) list * ty) Hashtbl.t;
  ret : ty;                          (* 当前函数的返回类型 *)
  vars : (string * (ty * bool)) list;
      (* 局部变量和它是否可修改（var、inout / sink 参数），内层在前 *)
  guards : string list;              (* 当前生效的 errdefer 要消费的变量 *)
  tbounds : (string * string) list;  (* 当前泛型函数的类型参数的 trait 约束，(T, Show) 表示 T: Show *)
}

(* 每个表达式的类型，按表达式 id 记录。 *)
let types : (int, t) Hashtbl.t = Hashtbl.create 256

(* 每条 errdefer（按调用表达式的 id）要消费的变量。 *)
let errdefer_targets : (int, string) Hashtbl.t = Hashtbl.create 16

(* 函数签名，给降级用：每个参数的约定和类型、返回类型。 *)
let sigs : (string, (mode * ty) list * ty) Hashtbl.t = Hashtbl.create 16

(* 结构体定义和标了 linear 的结构体，给降级和代码生成用。 *)
let struct_tbl : (string, field list) Hashtbl.t = Hashtbl.create 16
let linear_tbl : (string, unit) Hashtbl.t = Hashtbl.create 16

(* 枚举定义：变体按声明顺序，下标就是运行时的标签（Lean M8）。 *)
let enum_tbl : (string, (string * ty option) list) Hashtbl.t = Hashtbl.create 16

(* 一个类型的变体（不是枚举时为 None）。Result<T, E> 就是变体 Ok(T)、Err(E) 的枚举，() 载荷记作没有载荷。 *)
let variants = function
  | TStruct n -> Hashtbl.find_opt enum_tbl n
  | TResult (t, e) ->
      let pay t = if t = TUnit then None else Some t in
      Some [ ("Ok", pay t); ("Err", pay e) ]
  | _ -> None

(* 变体的下标和载荷类型。 *)
let variant ty v =
  let rec go i = function
    | [] -> None
    | (w, t) :: rest -> if w = v then Some (i, t) else go (i + 1) rest
  in
  go 0 (Option.value (variants ty) ~default:[])

(* 报错时变体的写法：Ok / Err 不带点。 *)
let vname v = if v = "Ok" || v = "Err" then v else "." ^ v

(* linear 值必须被显式消费，不能在作用域结束时自动析构。 *)
let rec is_linear = function
  | TStruct n -> Hashtbl.mem linear_tbl n
  | TResult (a, b) -> is_linear a || is_linear b
  | TBox t | TVec t -> is_linear t
  | TParam (_, l) -> l
  | TI64 | TBool | TUnit | TRef _ | TArena _ | TApp _ -> false

(* 需要析构（或者是 linear）的类型：这些类型的值只能移动，不能复制。 *)
let rec needs_drop = function
  | TBox _ | TVec _ -> true
  | TStruct n -> (
      Hashtbl.mem linear_tbl n
      ||
      match Hashtbl.find_opt struct_tbl n with
      | Some fs -> List.exists (fun (f : field) -> needs_drop f.fty) fs
      | None -> (
          (* 枚举：某个变体的载荷需要析构 *)
          match Hashtbl.find_opt enum_tbl n with
          | Some vs -> List.exists (function _, Some t -> needs_drop t | _, None -> false) vs
          | None -> false))
  | TResult (a, b) -> needs_drop a || needs_drop b
  | TParam _ -> true            (* 类型参数按抽象类型检查：可能需要析构，只能移动（Lean M9） *)
  | TI64 | TBool | TUnit | TRef _ | TArena _ | TApp _ -> false

(* 内建函数按值接收可复制的参数，记作 sink。 *)
let builtins =
  [
    "print_i64", ([ (MSink, TI64) ], TUnit);
    "print_bool", ([ (MSink, TBool) ], TUnit);
    "print", ([ (MSink, TUnit) ], TUnit);   (* 实参是字符串字面量，在 call 里单独检查 *)
    "choose", ([], TBool);       (* 对照测试用：按运行时给定的选择序列返回真假 *)
  ]

(* 特殊处理的内建函数：参数类型不固定。 *)
let special = [ "box"; "unbox"; "consume"; "alloc"; "vec_new"; "vec_push"; "vec_pop"; "vec_len"; "vec_get" ]

(* Vec 的内建函数的参数约定（给降级用；类型由类型检查单独处理）。vec_get 是位置，不是调用。 *)
let vec_sigs =
  [
    "vec_new", ([], TUnit);
    "vec_push", ([ (MInout, TUnit); (MSink, TUnit) ], TUnit);
    "vec_pop", ([ (MInout, TUnit) ], TUnit);
    "vec_len", ([ (MLet, TUnit) ], TI64);
  ]

(* Vec 内建函数的实例（降级后的调用名，例如 vec_push<I64>）→ 函数（push 等）和元素类型，给代码生成找辅助函数用。 *)
let vec_calls : (string, string * ty) Hashtbl.t = Hashtbl.create 16

(* 方法调用 e.f(…)（UFCS）：解析成 Call (".f", e :: …)。类型检查按接收者的类型确定函数名
   （Vec 的 push / pop / len / get 是内建的 vec_…，其他就是函数 f），按表达式 id 记下来，检查完再把名字换掉。 *)
let method_res : (int, string) Hashtbl.t = Hashtbl.create 16

let is_method f = f <> "" && f.[0] = '.'

(* 调用实际的函数名。 *)
let callee (x : expr) f =
  if is_method f then Option.value (Hashtbl.find_opt method_res x.id) ~default:f else f

(* 位置表达式（变量、字段、解引用、vec_get(v, i)）所在的变量；其他表达式没有。 *)
let rec root (x : expr) =
  match x.e with
  | Var v -> Some v
  | Field (a, _) | Deref a -> root a
  | Call (f, [ a; _ ]) when callee x f = "vec_get" -> root a
  | _ -> None

(* inout 实参必须是可修改的位置：var 变量或 inout / sink 参数里面的位置。 *)
let inout_place env (a : expr) =
  match root a with
  | None -> err a.pos "inout 实参必须是变量、字段或解引用这样的位置"
  | Some v -> (
      match List.assoc_opt v env.vars with
      | Some (_, true) -> ()
      | _ -> err a.pos "%s 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）" v)

let show = function Ty t -> string_of_ty t | Never -> "!"

(* ---------- 区域（Lean M5、M7（四）） ----------
   当前可见的区域，内层在前；每个区域记下比它活得久的区域。
   函数的区域参数（a: Arena，以及参数类型里出现的其他区域名）之间没有顺序（D3）；
   arena 块引入的区域比所有外层区域和区域参数都活得短。 *)
let regions : (string * string list) list ref = ref []

(* --unchecked（C22）：不检查区域的先后和 R3，照样导出，看 Lean 是否独立地拒绝。 *)
let lax_regions = ref false

let outlives x y =
  !lax_regions || x = y || match List.assoc_opt y !regions with Some a -> List.mem x a | None -> false

(* 实例名 → 泛型名和类型实参。 *)
let inst_tbl : (string, string * ty list) Hashtbl.t = Hashtbl.create 16

(* 泛型结构体、枚举 g 的第 i 个类型参数能不能协变（后面定义）。 *)
let covariant_fwd : (string -> int -> bool) ref = ref (fun _ _ -> false)

(* got 能不能当作 want 用：外层区域的引用可以当作内层区域的引用（D2 协变），其余要求相同。
   同一个泛型结构体、枚举的两个实例：能协变的类型参数逐个按 sub 比较，其他的要求相同（C24，Lean M16）。 *)
let rec sub got want =
  match got, want with
  | TRef (t, x), TRef (t', y) -> t = t' && outlives x y
  | TResult (a, b), TResult (c, d) -> sub a c && sub b d
  | TStruct a, TStruct b when a <> b -> (
      match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
      | Some (g, ts), Some (h, us) when g = h && List.length ts = List.length us ->
          List.for_all Fun.id
            (List.mapi (fun i (t, u) -> t = u || (!covariant_fwd g i && sub t u)) (List.combine ts us))
      | _ -> false)
  | _ -> got = want

(* 泛型结构体、枚举的实例的类型实参（不是实例时为空）。区域名的收集、换名都要看进类型实参（C20，Lean M15）。 *)
let inst_args n = match Hashtbl.find_opt inst_tbl n with Some (_, ts) -> ts | None -> []

(* 换名之后重新实例化（instantiate 在后面定义）。 *)
let instantiate_fwd : (string -> ty list -> ty) ref = ref (fun _ _ -> assert false)

let rec mentions r = function
  | TRef (t, x) -> x = r || mentions r t
  | TArena x -> x = r
  | TBox t | TVec t -> mentions r t
  | TResult (a, b) -> mentions r a || mentions r b
  | TStruct n -> List.exists (mentions r) (inst_args n)
  | TI64 | TBool | TUnit | TApp _ | TParam _ -> false

let rec region_names = function
  | TRef (t, x) -> x :: region_names t
  | TArena x -> [ x ]
  | TBox t | TVec t -> region_names t
  | TResult (a, b) -> region_names a @ region_names b
  | TStruct n -> List.concat_map region_names (inst_args n)
  | TI64 | TBool | TUnit | TApp _ | TParam _ -> []

let rec rename s = function
  | TRef (t, x) -> TRef (rename s t, Option.value (List.assoc_opt x s) ~default:x)
  | TArena x -> TArena (Option.value (List.assoc_opt x s) ~default:x)
  | TBox t -> TBox (rename s t)
  | TVec t -> TVec (rename s t)
  | TResult (a, b) -> TResult (rename s a, rename s b)
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) when List.exists (fun t -> region_names t <> []) ts -> !instantiate_fwd g (List.map (rename s) ts)
      | _ -> t)
  | t -> t

(* 区域名都换成 _：C 后端里只差区域的类型是同一个 C 类型。 *)
let rec erase = function
  | TRef (t, _) -> TRef (erase t, "_")
  | TBox t -> TBox (erase t)
  | TVec t -> TVec (erase t)
  | TResult (a, b) -> TResult (erase a, erase b)
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) -> TStruct (g ^ "<" ^ String.concat ", " (List.map (fun t -> string_of_ty (erase t)) ts) ^ ">")
      | None -> t)
  | t -> t

(* 两个分支的类型合并：Never 和任何类型相容；区域不同时取活得短的那个。 *)
let join pos what a b =
  match a, b with
  | Never, x | x, Never -> x
  | Ty x, Ty y when sub x y -> Ty y
  | Ty x, Ty y when sub y x -> Ty x
  | _ -> err pos "%s的两个分支类型不同：%s 和 %s" what (show a) (show b)

let expect pos want got =
  match got with
  | Never -> ()
  | Ty t when sub t want -> ()
  | Ty t -> err pos "类型不匹配：需要 %s，实际是 %s" (string_of_ty want) (string_of_ty t)

(* param：函数参数的类型，可以是 Arena。 *)
let rec check_ty ?(param = false) env pos = function
  | TStruct n when not (Hashtbl.mem env.structs n || Hashtbl.mem enum_tbl n) -> err pos "未知类型 %s" n
  | TStruct n -> List.iter (check_ty env pos) (inst_args n)
  | TBox TUnit -> err pos "Box 里不能放 ()"
  | TBox t ->
      if is_linear t then err pos "Box 里不能放 linear 值（还不能从 Box 里移出）";
      check_ty env pos t
  | TVec t -> check_ty env pos t
  | TResult (a, b) -> check_ty env pos a; check_ty env pos b
  | TArena _ when not param -> err pos "Arena 只能作为函数参数的类型"
  | TRef (t, r) ->
      if not (List.mem_assoc r !regions) then err pos "未知区域 %s" r;
      if region_names t <> [] then err pos "还不支持指向引用或区域的引用";
      if t = TUnit then err pos "不能引用 ()";
      check_ty env pos t
  | _ -> ()

(* ---------- 泛型（Lean M9） ----------
   泛型结构体和枚举按用到的类型实参实例化成具体的类型，名字就是写法本身（例如 "Option<I64>"），
   登记到 struct_tbl / enum_tbl，之后的检查、降级、代码生成都把它当普通类型。
   泛型函数里的类型参数记作 TParam，按抽象类型检查：值只能移动，普通的 T 可以自动析构，
   T: linear 必须交出去；不能对 T 做运算、取字段或 match。调用处推出类型实参并检查约束，
   用到的每个实例复制一份函数体，换成具体类型后照常检查（单态化）。 *)

(* 泛型结构体、枚举的定义：类型参数名和原始定义（类型还没解析）。 *)
let gtypes : (string, string list * item) Hashtbl.t = Hashtbl.create 16

(* 泛型函数的类型参数；解析过类型的泛型函数本身，实例化时复制它。 *)
let gparams : (string, (string * bool) list) Hashtbl.t = Hashtbl.create 16
let gfns : (string, item) Hashtbl.t = Hashtbl.create 16

(* trait（Lean M10）：每个方法当作带一个类型参数 Self 的泛型函数，Self 换成实现的类型。
   traits：trait → 方法名；methods：方法名 → trait；impls：(trait, 类型) 有 impl；
   gbounds：泛型函数（和方法）的类型参数要满足的 trait 约束。
   impl 里的方法是普通的具体函数，名字记作 方法<类型>（和泛型函数的实例同一种写法），单态化时直接调用它。 *)
let traits : (string, string list) Hashtbl.t = Hashtbl.create 16
let methods : (string, string) Hashtbl.t = Hashtbl.create 16
let impls : (string * string, unit) Hashtbl.t = Hashtbl.create 16
let gbounds : (string, (string * string) list) Hashtbl.t = Hashtbl.create 16

(* 每个泛型调用（按调用表达式的 id）推出的类型实参，给降级时找实例的名字用。 *)
let call_inst : (int, ty list) Hashtbl.t = Hashtbl.create 16

(* 还没有生成的函数实例。 *)
let pending : (string * ty list) Queue.t = Queue.create ()
let requested : (string, unit) Hashtbl.t = Hashtbl.create 16

let inst_name g ts = g ^ "<" ^ String.concat ", " (List.map string_of_ty ts) ^ ">"

let inst_of n = Option.map fst (Hashtbl.find_opt inst_tbl n)

let is_generic f = Hashtbl.mem gparams f

let rec has_arena = function
  | TArena _ -> true
  | TBox t | TVec t | TRef (t, _) -> has_arena t
  | TResult (a, b) -> has_arena a || has_arena b
  | _ -> false

(* 类型实参的共同限制。泛型函数和 Vec 的类型实参可以含引用（C19）：实例的区域参数就是实参里的区域名（Lean M14）。 *)
let type_arg pos t =
  if t = TUnit then err pos "类型实参不能是 ()";
  if has_arena t then err pos "类型实参不能是 Arena"

let lin_arg pos t =
  if is_linear t then err pos "泛型结构体、枚举的类型实参暂不能是 linear 值（%s）" (string_of_ty t)

(* 泛型结构体、枚举的类型实参可以含引用（C20，Lean M15），还不能是 linear 值（字段和载荷的 linear 规则还没有和泛型一起证明）。 *)
let data_arg pos t =
  type_arg pos t;
  lin_arg pos t

(* 解析类型：sc 把类型参数名映射到类型（泛型函数里是 TParam，实例化时是类型实参）；
   Option<I64> 这样的写法换成实例。 *)
let rec resolve sc pos = function
  | TStruct n when List.mem_assoc n sc -> List.assoc n sc
  | TStruct "Vec" when not (List.mem_assoc "Vec" sc) -> err pos "Vec 要写元素类型：Vec<…>"
  | TStruct n when Hashtbl.mem gtypes n -> err pos "%s 是泛型类型，要写类型实参：%s<…>" n n
  | TApp ("Vec", ts) -> (
      (* 内建的动态数组：元素不能是 ()、引用或 linear 值（pop 返回 Option<T>，Option 的实参还不能是 linear 的） *)
      match List.map (resolve sc pos) ts with
      | [ t ] ->
          type_arg pos t;
          lin_arg pos t;
          TVec t
      | ts -> err pos "Vec 需要 1 个类型实参，实际给了 %d 个" (List.length ts))
  | TApp (n, ts) -> (
      match Hashtbl.find_opt gtypes n with
      | None -> err pos "%s 不是泛型类型，不能写类型实参" n
      | Some (ps, _) ->
          if List.length ps <> List.length ts then
            err pos "%s 需要 %d 个类型实参，实际给了 %d 个" n (List.length ps) (List.length ts);
          let ts = List.map (resolve sc pos) ts in
          List.iter (data_arg pos) ts;
          instantiate n ts)
  | TBox t -> TBox (resolve sc pos t)
  | TVec t -> TVec (resolve sc pos t)
  | TResult (a, b) -> TResult (resolve sc pos a, resolve sc pos b)
  | TRef (t, r) -> TRef (resolve sc pos t, r)
  | t -> t

and instantiate g ts =
  let name = inst_name g ts in
  if not (Hashtbl.mem inst_tbl name) then begin
    Hashtbl.replace inst_tbl name (g, ts);
    if Hashtbl.length inst_tbl > 1000 then
      err { line = 1; col = 1 } "泛型类型的实例超过 1000 个（可能是类型实参越来越大的递归类型，例如 W<T> 里有 Box<W<Box<T>>>）";
    let ps, it = Hashtbl.find gtypes g in
    let sc = List.combine ps ts in
    match it with
    | Struct { fields; linear; ipos; _ } ->
        Hashtbl.replace struct_tbl name
          (List.map (fun (f : field) -> { f with fty = resolve sc ipos f.fty }) fields);
        if linear then Hashtbl.replace linear_tbl name ()
    | Enum { variants; ipos; _ } ->
        Hashtbl.replace enum_tbl name
          (List.map (fun (v, t) -> (v, Option.map (resolve sc ipos) t)) variants)
    | Fn _ | Trait _ | Impl _ -> ()
  end;
  TStruct name

let () = instantiate_fwd := instantiate

(* 把类型参数换成类型（一次换完，换进去的类型不再替换）；实例里的类型参数也一起换。 *)
let rec subst s = function
  | TParam (n, _) as t -> Option.value (List.assoc_opt n s) ~default:t
  | TStruct n as t -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) -> instantiate g (List.map (subst s) ts)
      | None -> t)
  | TBox t -> TBox (subst s t)
  | TVec t -> TVec (subst s t)
  | TResult (a, b) -> TResult (subst s a, subst s b)
  | TRef (t, r) -> TRef (subst s t, r)
  | t -> t

(* 类型里出现的类型参数。 *)
let rec params_of = function
  | TParam (n, _) -> [ n ]
  | TStruct n -> (
      match Hashtbl.find_opt inst_tbl n with Some (_, ts) -> List.concat_map params_of ts | None -> [])
  | TBox t | TVec t | TRef (t, _) -> params_of t
  | TResult (a, b) -> params_of a @ params_of b
  | TI64 | TBool | TUnit | TArena _ | TApp _ -> []

(* 协变（C24，Lean M16）：泛型结构体、枚举 g 的第 i 个类型参数只出现在只读的位置时，
   g<…外层…> 可以当作 g<…内层…> 用。只读的位置：字段、载荷直接是这个参数，
   或者在 Result 里，或者是另一个实例里能协变的类型实参。
   Box、Vec 里的保守地算不变（实例之间不比较）；可以修改的 inout 实参本来就要求类型完全相同。 *)
let cov_memo : (string * int, bool) Hashtbl.t = Hashtbl.create 16

let rec covariant g i =
  match Hashtbl.find_opt cov_memo (g, i) with
  | Some b -> b
  | None ->
      (* 先记一个结果，防止递归的类型（经过自己）死循环 *)
      Hashtbl.replace cov_memo (g, i) true;
      let ps, _ = Hashtbl.find gtypes g in
      let p = List.nth ps i in
      let abs = match instantiate g (List.map (fun q -> TParam (q, false)) ps) with TStruct n -> n | _ -> g in
      let tys =
        match Hashtbl.find_opt struct_tbl abs, Hashtbl.find_opt enum_tbl abs with
        | Some fs, _ -> List.map (fun (f : field) -> f.fty) fs
        | None, Some vs -> List.filter_map snd vs
        | None, None -> []
      in
      let b = List.for_all (only_cov p) tys in
      Hashtbl.replace cov_memo (g, i) b;
      b

(* 类型参数 p 在 t 里只出现在只读的位置（或者不出现）。 *)
and only_cov p t =
  match t with
  | TParam _ -> true
  | TResult (a, b) -> only_cov p a && only_cov p b
  | TStruct n -> (
      match Hashtbl.find_opt inst_tbl n with
      | Some (g, ts) ->
          List.for_all Fun.id
            (List.mapi (fun i t -> (not (List.mem p (params_of t))) || (covariant g i && only_cov p t)) ts)
      | None -> true)
  | TBox _ | TVec _ | TRef _ -> not (List.mem p (params_of t))
  | TI64 | TBool | TUnit | TArena _ | TApp _ -> true

let () = covariant_fwd := covariant

(* 推断类型实参：模式 p 里的类型参数按 t 里对应位置的类型确定，先确定的为准。 *)
let rec unify s p t =
  match p, t with
  | TParam (n, _), t -> if not (List.mem_assoc n !s) then s := (n, t) :: !s
  | TBox p, TBox t | TVec p, TVec t -> unify s p t
  | TResult (a, b), TResult (c, d) -> unify s a c; unify s b d
  | TStruct a, TStruct b -> (
      match Hashtbl.find_opt inst_tbl a, Hashtbl.find_opt inst_tbl b with
      | Some (g, ps), Some (h, ts) when g = h -> List.iter2 (unify s) ps ts
      | _ -> ())
  | _ -> ()

(* 改写函数体里写出的类型（let 标注、var 声明），方法调用换成解析出的函数名（检查过的才有）。
   fresh：表达式换新的 id（复制出实例时用）。 *)
let map_body ~fresh (ft : pos -> ty -> ty) (b : block) : block =
  let rec ex (x : expr) =
    let e =
      match x.e with
      | (Int _ | Bool _ | Unit | Str _ | Var _) as d -> d
      | Binop (o, a, b) -> Binop (o, ex a, ex b)
      | Unop (o, a) -> Unop (o, ex a)
      | Call (f, args) -> Call (callee x f, List.map ex args)
      | StructLit (n, fs) -> StructLit (n, List.map (fun (f, e) -> (f, ex e)) fs)
      | Field (a, f) -> Field (ex a, f)
      | Deref a -> Deref (ex a)
      | Try a -> Try (ex a)
      | Variant (v, a) -> Variant (v, Option.map ex a)
      | Match (bw, s, arms) -> Match (bw, ex s, List.map (fun (v, y, e) -> (v, y, ex e)) arms)
      | Arena (r, b) -> Arena (r, blk b)
      | If (c, a, b) -> If (ex c, blk a, Option.map blk b)
      | Block b -> Block (blk b)
    in
    { x with e; id = (if fresh then fresh_id () else x.id) }
  and st (s : stmt) =
    let d =
      match s.s with
      | Let (m, v, t, e) -> Let (m, v, Option.map (ft s.spos) t, ex e)
      | Decl (v, t) -> Decl (v, ft s.spos t)
      | Assign (v, e) -> Assign (v, ex e)
      | SetField (a, f, e) -> SetField (ex a, f, ex e)
      | While (c, b) -> While (ex c, blk b)
      | Return e -> Return (Option.map ex e)
      | Expr e -> Expr (ex e)
      | Errdefer e -> Errdefer (ex e)
      | Destruct (n, bs, e) -> Destruct (n, bs, ex e)
    in
    { s with s = d }
  and blk (b : block) = { stmts = List.map st b.stmts; tail = Option.map ex b.tail } in
  blk b

(* Vec 的内建函数按元素类型实例化：降级时调用名换成 vec_push<T> 这样的实例名；
   元素类型是具体类型时登记，代码生成为它写辅助函数。 *)
let vec_inst (x : expr) b t =
  Hashtbl.replace call_inst x.id [ t ];
  if params_of t = [] then Hashtbl.replace vec_calls (inst_name ("vec_" ^ b) [ t ]) (b, t)

(* 每个表达式的类型按 id 记下来，给降级用。 *)
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
      let is_result = match want with TResult _ -> true | _ -> false in
      let w = vname v in
      match variant want v, a with
      | _ when variants want = None -> err x.pos "这里需要 %s，%s 是枚举的变体" (string_of_ty want) w
      | None, _ -> err x.pos "%s 没有变体 %s" (string_of_ty want) w
      | Some (_, Some t), Some a ->
          ignore (check env t a);
          Ty want
      | Some (_, None), None -> Ty want
      | Some (_, None), Some a when is_result ->
          (* Ok(()) / Err(())：Result 里 () 载荷记作没有载荷 *)
          ignore (check env TUnit a);
          Ty want
      | Some (_, Some t), None -> err x.pos "%s 带一个 %s 类型的载荷，要写 %s(…)" w (string_of_ty t) w
      | Some (_, None), Some _ -> err x.pos "%s 没有载荷，不能写 %s(…)" w w)
  | If (c, a, Some b) ->
      expect c.pos TBool (expr env c);
      expect x.pos want (block env ~want a);
      expect x.pos want (block env ~want b);
      Ty want
  | Block b -> block env ~want b
  | Match (b, s, arms) -> match_ env (Some want) x.pos b s arms
  | Arena (r, b) -> arena env (Some want) x.pos r b
  | Call ("vec_new", []) -> (
      match want with
      | TVec t ->
          vec_inst x "new" t;
          Ty want
      | _ -> err x.pos "这里需要 %s，vec_new() 是 Vec" (string_of_ty want))
  | Call (f, args) when not (List.mem f special) ->
      let t = call env (Some want) x f args in
      expect x.pos want t;
      t
  | StructLit (n, inits) ->
      let t = struct_lit env (Some want) x n inits in
      expect x.pos want t;
      t
  | _ ->
      let t = expr1 env x in
      expect x.pos want t;
      t

and expr1 env (x : expr) : t =
  let pos = x.pos in
  match x.e with
  | Call (f, (r :: _ as args)) when is_method f -> expr1 env { x with e = Call (method_name env x f r, args) }
  | Int _ -> Ty TI64
  | Bool _ -> Ty TBool
  | Unit -> Ty TUnit
  | Str _ -> err pos "字符串字面量暂时只能直接作为 print 的实参，例如 print(\"hello\")"
  | Var v -> (
      match List.assoc_opt v env.vars with
      | Some (t, _) -> Ty t
      | None -> err pos "未定义的变量 %s" v)
  | Binop (op, a, b) -> (
      let ta = expr env a in
      let tb = expr env b in
      match op with
      | Add | Sub | Mul | Div | Rem ->
          expect a.pos TI64 ta; expect b.pos TI64 tb; Ty TI64
      | Lt | Le | Gt | Ge ->
          expect a.pos TI64 ta; expect b.pos TI64 tb; Ty TBool
      | Eq | Ne -> (
          match ta, tb with
          | Ty (TStruct _ | TUnit | TBox _ | TVec _ | TResult _ | TRef _ | TArena _ | TApp _ | TParam _), _
          | _, Ty (TStruct _ | TUnit | TBox _ | TVec _ | TResult _ | TRef _ | TArena _ | TApp _ | TParam _) ->
              err pos "只能比较 I64 和 Bool"
          | _ -> ignore (join pos "比较" ta tb); Ty TBool)
      | And | Or -> expect a.pos TBool ta; expect b.pos TBool tb; Ty TBool)
  | Unop (Neg, a) -> expect a.pos TI64 (expr env a); Ty TI64
  | Unop (Not, a) -> expect a.pos TBool (expr env a); Ty TBool
  | Call ("box", [ a ]) -> (
      match expr env a with
      | Ty TUnit -> err pos "Box 里不能放 ()"
      | Ty t when is_linear t -> err pos "Box 里不能放 linear 值（还不能从 Box 里移出）"
      | Ty t -> Ty (TBox t)
      | Never -> Never)
  | Call ("consume", [ a ]) -> (
      match expr env a with
      | Ty (TStruct n) when Hashtbl.mem linear_tbl n -> Ty TUnit
      | Never -> Never
      | Ty t -> err pos "consume 只能用于 linear 结构体，%s 不是" (string_of_ty t))
  | Call ("unbox", [ a ]) -> (
      (* 取出 Box 里的值：Box 按 sink 交出，堆内存释放（Lean M11 的 unbox） *)
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
  | Call ("vec_new", []) ->
      err pos "无法确定 vec_new() 的元素类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方"
  | Call (("vec_push" | "vec_pop" | "vec_len" | "vec_get") as f, v :: rest) -> (
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
      | Ty (TVec t), "vec_get", [ i ] ->
          (* 第 i 个元素这个位置：可复制的值复制，需要析构的值 let 绑定成投影（锁住 v），越界时 panic *)
          expect i.pos TI64 (expr env i);
          Ty t
      | Ty (TVec _), _, _ ->
          err pos "%s 需要 %d 个参数" f (match f with "vec_push" | "vec_get" -> 2 | _ -> 1)
      | Ty t, _, _ -> err v.pos "%s 的第一个参数必须是 Vec，%s 不是" f (string_of_ty t))
  | Call (("vec_new" | "vec_push" | "vec_pop" | "vec_len" | "vec_get") as f, _) -> err pos "%s 的参数个数不对" f
  | Deref a -> (
      match expr env a with
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
  | Arena (r, b) -> arena env None pos r b

(* 函数调用。want：期望的返回类型（知道时用来推断泛型函数的类型实参）。 *)
(* 方法调用 r.f(…) 的函数名：接收者是 Vec 时 push / pop / len / get 是内建的 vec_…，否则是函数 f。 *)
and method_name env (x : expr) f (r : expr) =
  let m = String.sub f 1 (String.length f - 1) in
  let g =
    match expr env r, m with
    | Ty (TVec _), ("push" | "pop" | "len" | "get") -> "vec_" ^ m
    | _ -> m
  in
  Hashtbl.replace method_res x.id g;
  g

and call env want (x : expr) f args : t =
  if f = "print" then (
    match args with
    | [ { e = Str _; _ } ] -> Ty TUnit
    | _ -> err x.pos "print 的实参必须是一个字符串字面量，例如 print(\"hello\")（整数和布尔值用 print_i64、print_bool）")
  else
  let pos = x.pos in
  let ps, r =
    match Hashtbl.find_opt env.fns f with
    | Some s -> s
    | None -> (
        match List.assoc_opt f builtins with
        | Some s -> s
        | None -> err pos "未定义的函数 %s" f)
  in
  if List.length ps <> List.length args then
    err pos "%s 需要 %d 个参数，实际给了 %d 个" f (List.length ps) (List.length args);
  (* 泛型函数：从期望的返回类型和实参类型推出类型实参，检查约束（普通的 T 不能是 linear 类型），
     然后按具体类型检查实参；类型实参都是具体类型时登记这个实例（Lean M9 的 Respects） *)
  let ps, r =
    match Hashtbl.find_opt gparams f with
    | None -> (ps, r)
    | Some tps ->
        let s = ref [] in
        Option.iter (fun w -> unify s r w) want;
        (* .A(e) / .B 自己推不出类型，留到类型实参确定之后按期望类型检查 *)
        List.iter2
          (fun (_, p) (a : expr) ->
            if List.exists (fun n -> not (List.mem_assoc n !s)) (params_of p)
               && (match a.e with Variant _ -> false | _ -> true)
            then
              match expr env a with Ty t -> unify s p t | Never -> ())
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
        (* trait 约束：类型参数要在当前函数里写了同样的约束，具体类型要有 impl（Lean M10 的 RespectsT） *)
        let named = List.combine (List.map fst tps) ts in
        List.iter
          (fun (n, tr) ->
            let t = List.assoc n named in
            let ok =
              match t with
              | TParam (m, _) -> List.mem (m, tr) env.tbounds
              | t -> Hashtbl.mem impls (tr, string_of_ty t)
            in
            if not ok then
              match Hashtbl.mem methods f, t with
              | true, TParam (m, _) -> err pos "%s 没有写约束 %s，不能调用 %s（要写 %s: %s）" m tr f m tr
              | true, _ -> err pos "%s 没有实现 %s，不能调用 %s" (string_of_ty t) tr f
              | false, TParam (m, _) -> err pos "%s 要求 %s: %s，但 %s 没有写约束 %s" f n tr m tr
              | false, _ -> err pos "%s 没有实现 %s（%s 要求 %s: %s）" (string_of_ty t) tr f n tr)
          (Option.value (Hashtbl.find_opt gbounds f) ~default:[]);
        (* 含引用的类型实参（C19）：类型参数要出现在参数类型里，实例的区域才能从实参得到；
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
        (* 方法调用直接用 impl 里的函数，不用生成实例 *)
        if List.for_all (fun t -> params_of t = []) ts
           && (not (Hashtbl.mem methods f))
           && not (Hashtbl.mem requested (inst_name f ts))
        then begin
          Hashtbl.replace requested (inst_name f ts) ();
          Queue.add (f, ts) pending
        end;
        let s = List.combine (List.map fst tps) ts in
        (List.map (fun (m, p) -> (m, subst s p)) ps, subst s r)
  in
  (* 区域参数按实参实例化（M5（四））：Arena 参数取实参的区域，
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
  List.iter2
    (fun (m, p) (a : expr) ->
      let p = rename s p in
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

(* 枚举（含 Result）的 match（Lean M8）：必须穷尽，每个变体恰好一个分支；带载荷的变体绑定载荷。
   borrow：match let s { … }，s 必须是位置（Lean M8（二））。 *)
and match_ env want pos borrow (s : expr) arms : t =
  match expr env s with
  | Never -> err s.pos "不能 match 一个不会返回的值"
  | Ty ty -> (
      match variants ty with
      | None -> err s.pos "%s 不是 Result 或枚举，不能 match" (string_of_ty ty)
      | Some vs ->
          if borrow && root s = None then
            err s.pos "match let 后面必须是变量、字段或解引用（要借用的位置）";
          enum_match env want pos ty vs arms)

and enum_match env want pos ty vs arms : t =
  let is_result = match ty with TResult _ -> true | _ -> false in
  List.iter
    (fun (v, _, (a : expr)) ->
      if not (List.mem_assoc v vs) then err a.pos "%s 没有变体 %s" (string_of_ty ty) (vname v))
    arms;
  let bodies =
    List.map
      (fun (v, pty) ->
        let w = vname v in
        match List.filter (fun (u, _, _) -> u = v) arms with
        | [ (_, x, (a : expr)) ] -> (
            match pty with
            | Some t ->
                if x = "" then err a.pos "%s 带载荷，分支要写 %s(x) 或 %s(_)" w w w;
                ({ env with vars = (x, (t, false)) :: env.vars }, a)
            | None ->
                (* Result 里的 () 载荷：分支写 Ok(_) *)
                if x <> "" && not (is_result && x = "_") then
                  if is_result then err a.pos "%s 的值是 ()，分支要写 %s(_)" w w
                  else err a.pos "%s 没有载荷，分支要写 %s => …" w w;
                (env, a))
        | [] -> err pos "match 没有覆盖 %s（枚举的 match 必须穷尽）" w
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
            | TUnit -> err pos "C0 不支持 () 类型的变量 %s" v
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
            if ty = TUnit then err pos "C0 不支持 () 类型的变量 %s" v;
            (match ty with TArena _ -> err pos "区域不能存进变量" | _ -> ());
            go { env with vars = (v, (ty, mut)) :: env.vars } rest
        | Assign (v, e) ->
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
            (match root a with
            | Some v -> (
                match List.assoc_opt v env.vars with
                | Some (_, true) -> ()
                | _ -> err a.pos "%s 不可修改，不能给它的字段赋值（需要 var 变量或 inout / sink 参数）" v)
            | None -> err a.pos "只能给变量（或它里面的位置）的字段赋值");
            if is_linear fty then err pos "字段 %s 是 linear 值，不能直接赋值（旧值没有被消费）" f;
            ignore (check env fty e);
            go env rest
        | While (c, body) ->
            expect c.pos TBool (expr env c);
            expect pos TUnit (block env body);
            go env rest
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
  | Call (f, args) when f <> "box" ->
      ignore (expr env e);
      let f = callee e f in
      List.iter
        (fun (a : expr) ->
          match a.e with
          | Var _ | Field _ | Deref _ | Int _ | Bool _ | Unit -> ()
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

(* 整个程序的准备：结构体、枚举（含泛型定义）、函数签名、字段和载荷。返回检查函数体用的环境。 *)
let prepare (p : program) =
  let fns = Hashtbl.create 16 in
  Hashtbl.reset struct_tbl;
  Hashtbl.reset linear_tbl;
  Hashtbl.reset enum_tbl;
  Hashtbl.reset gtypes;
  Hashtbl.reset inst_tbl;
  Hashtbl.reset cov_memo;
  Hashtbl.reset gparams;
  Hashtbl.reset gfns;
  Hashtbl.reset call_inst;
  Hashtbl.reset requested;
  Hashtbl.reset traits;
  Hashtbl.reset methods;
  Hashtbl.reset impls;
  Hashtbl.reset gbounds;
  Hashtbl.reset vec_calls;
  Hashtbl.reset method_res;
  (* 内建的 Option<T>：vec_pop 返回它，写法和用户的枚举一样（.Some(x) / .None） *)
  Hashtbl.add gtypes "Option"
    ( [ "T" ],
      Enum
        {
          name = "Option";
          tparams = [ ("T", false) ];
          variants = [ ("Some", Some (TStruct "T")); ("None", None) ];
          ipos = { line = 1; col = 1 };
        } );
  Queue.clear pending;
  let defined n = Hashtbl.mem struct_tbl n || Hashtbl.mem enum_tbl n || Hashtbl.mem gtypes n in
  (* what 不为空：结构体、枚举的类型参数，不能标 linear *)
  let tparams_ok what ipos tps =
    List.iteri
      (fun i (t, lin) ->
        if List.mem_assoc t (List.filteri (fun j _ -> j < i) tps) then err ipos "类型参数 %s 重复" t;
        if lin && what <> "" then err ipos "%s的类型参数 %s 不能标 linear（只有函数的类型参数可以）" what t)
      tps
  in
  (* 先登记所有名字，字段和载荷的类型等名字都登记之后再解析 *)
  List.iter
    (function
      | Struct { name; tparams; fields; linear; ipos } as it ->
          if defined name then err ipos "类型 %s 重复定义" name;
          tparams_ok "结构体" ipos tparams;
          if tparams <> [] then Hashtbl.add gtypes name (List.map fst tparams, it)
          else begin
            Hashtbl.add struct_tbl name fields;
            if linear then Hashtbl.replace linear_tbl name ()
          end
      | Enum { name; tparams; variants; ipos } as it ->
          if defined name then err ipos "类型 %s 重复定义" name;
          tparams_ok "枚举" ipos tparams;
          if variants = [] then err ipos "枚举 %s 至少要有一个变体" name;
          List.iteri
            (fun i (v, _) ->
              if List.exists (fun (w, _) -> w = v) (List.filteri (fun j _ -> j < i) variants) then
                err ipos "枚举 %s 的变体 %s 重复" name v)
            variants;
          if tparams <> [] then Hashtbl.add gtypes name (List.map fst tparams, it)
          else Hashtbl.add enum_tbl name variants
      | Fn { name; ipos; _ } ->
          if Hashtbl.mem fns name || List.mem_assoc name builtins || List.mem name special then
            err ipos "函数 %s 重复定义" name;
          Hashtbl.add fns name ([], TUnit)
      | Trait { name; methods = ms; ipos } ->
          if Hashtbl.mem traits name then err ipos "trait %s 重复定义" name;
          List.iteri
            (fun i (m : msig) ->
              if List.exists (fun (n : msig) -> n.mname = m.mname) (List.filteri (fun j _ -> j < i) ms) then
                err m.mpos "trait %s 的方法 %s 重复" name m.mname)
            ms;
          Hashtbl.add traits name (List.map (fun (m : msig) -> m.mname) ms)
      | Impl _ -> ())
    p;
  (* 结构体、枚举不能不经过 Box 包含自己（大小是无限的）；经过 Box 的递归可以（Lean M11）。
     泛型类型的实参也算包含（保守：实参只放在 Box 里时也拒绝）。 *)
  let rec visit path n =
    if List.mem n path then
      err { line = 1; col = 1 } "类型 %s 不经过 Box 包含自己，大小是无限的（递归要经过 Box，例如 next: Box<List>）" n;
    let rec inner = function
      | TStruct m -> visit (n :: path) m
      | TApp ("Vec", _) -> ()
      | TApp (m, ts) -> visit (n :: path) m; List.iter inner ts
      | TBox _ | TVec _ -> ()   (* Box、Vec 是指针，经过它们的递归大小有限 *)
      | TResult (a, b) -> inner a; inner b
      | _ -> ()
    in
    let raw = function
      | Struct { fields; _ } -> List.map (fun (f : field) -> f.fty) fields
      | Enum { variants; _ } -> List.filter_map snd variants
      | Fn _ | Trait _ | Impl _ -> []
    in
    match Hashtbl.find_opt struct_tbl n, Hashtbl.find_opt enum_tbl n, Hashtbl.find_opt gtypes n with
    | Some fs, _, _ -> List.iter (fun (f : field) -> inner f.fty) fs
    | _, Some vs, _ -> List.iter (fun (_, t) -> Option.iter inner t) vs
    | _, _, Some (_, it) -> List.iter inner (raw it)
    | None, None, None -> ()
  in
  Hashtbl.iter (fun n _ -> visit [] n) struct_tbl;
  Hashtbl.iter (fun n _ -> visit [] n) enum_tbl;
  Hashtbl.iter (fun n _ -> visit [] n) gtypes;
  (* 解析非泛型结构体、枚举的字段和载荷（Option<I64> 这样的写法换成实例） *)
  List.iter
    (function
      | Struct { name; tparams = []; fields; ipos; _ } ->
          Hashtbl.replace struct_tbl name
            (List.map (fun (f : field) -> { f with fty = resolve [] ipos f.fty }) fields)
      | Enum { name; tparams = []; variants; ipos; _ } ->
          Hashtbl.replace enum_tbl name (List.map (fun (v, t) -> (v, Option.map (resolve [] ipos) t)) variants)
      | Struct _ | Enum _ | Fn _ | Trait _ | Impl _ -> ())
    p;
  (* 函数签名：泛型函数的类型参数解析成 TParam；trait 的方法是带类型参数 Self 的泛型函数；
     impl 里的方法登记成 方法<类型> *)
  List.iter
    (function
      | Struct { tparams; ipos; _ } | Enum { tparams; ipos; _ } ->
          List.iter (fun (t, _) -> if defined t then err ipos "类型参数 %s 和已有的类型同名" t) tparams
      | Fn { name; tparams; bounds; params; ret; ipos; _ } ->
          tparams_ok "" ipos tparams;
          List.iter
            (fun (t, _) ->
              if defined t then err ipos "类型参数 %s 和已有的类型同名" t;
              if t = "Self" then err ipos "Self 不能用作类型参数的名字")
            tparams;
          List.iter (fun (_, tr) -> if not (Hashtbl.mem traits tr) then err ipos "未知 trait %s" tr) bounds;
          if bounds <> [] then Hashtbl.replace gbounds name bounds;
          let sc = List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams in
          let ps = List.map (fun (m, _, t) -> (m, resolve sc ipos t)) params in
          let ret = resolve sc ipos ret in
          if tparams <> [] then begin
            if List.exists (fun (_, t) -> region_names t <> []) ps || region_names ret <> [] then
              err ipos "泛型函数暂不支持区域参数和引用（区域和泛型的组合还没有实现）";
            Hashtbl.replace gparams name tparams
          end;
          Hashtbl.replace fns name (ps, ret)
      | Trait { name; methods = ms; _ } ->
          (* Self 可以是任何类型（包括 linear 的），调用处的效果只由参数约定决定（Lean M10） *)
          let sc = [ ("Self", TParam ("Self", true)) ] in
          List.iter
            (fun (m : msig) ->
              if Hashtbl.mem fns m.mname || List.mem_assoc m.mname builtins || List.mem m.mname special
                 || Hashtbl.mem methods m.mname
              then err m.mpos "方法名 %s 和已有的函数或方法同名（暂不支持重载）" m.mname;
              let ps = List.map (fun (md, _, t) -> (md, resolve sc m.mpos t)) m.mparams in
              let ret = resolve sc m.mpos m.mret in
              if List.exists (fun (_, t) -> region_names t <> []) ps || region_names ret <> [] then
                err m.mpos "trait 的方法暂不支持区域参数和引用";
              Hashtbl.replace fns m.mname (ps, ret);
              Hashtbl.replace gparams m.mname [ ("Self", true) ];
              Hashtbl.replace gbounds m.mname [ ("Self", name) ];
              Hashtbl.replace methods m.mname name)
            ms
      | Impl { trait = tr; self; fns = ms; ipos } ->
          if not (Hashtbl.mem traits tr) then err ipos "未知 trait %s" tr;
          let st = resolve [] ipos self in
          type_arg ipos st;
          if region_names st <> [] then err ipos "暂不支持为含引用的类型 %s 实现 trait" (string_of_ty st);
          (match st with TArena _ -> err ipos "不能为 Arena 实现 trait" | _ -> ());
          if Hashtbl.mem impls (tr, string_of_ty st) then err ipos "%s 已经实现过 %s" (string_of_ty st) tr;
          Hashtbl.replace impls (tr, string_of_ty st) ();
          List.iter
            (function
              | Fn { name; tparams; params; ret; ipos; _ } ->
                  if tparams <> [] then err ipos "impl 里的方法暂不能有类型参数";
                  let sc = [ ("Self", st) ] in
                  Hashtbl.replace fns (inst_name name [ st ])
                    (List.map (fun (md, _, t) -> (md, resolve sc ipos t)) params, resolve sc ipos ret)
              | Struct _ | Enum _ | Trait _ | Impl _ -> ())
            ms)
    p;
  Hashtbl.reset sigs;
  Hashtbl.iter (Hashtbl.replace sigs) fns;
  List.iter (fun (f, s) -> Hashtbl.replace sigs f s) (builtins @ vec_sigs);
  let env0 = { structs = struct_tbl; fns; ret = TUnit; vars = []; guards = []; tbounds = [] } in
  (* 字段和载荷的规则；泛型定义按类型参数是抽象类型的实例检查 *)
  let abstract name tparams =
    if tparams = [] then name
    else match instantiate name (List.map (fun (t, _) -> TParam (t, false)) tparams) with TStruct n -> n | _ -> name
  in
  List.iter
    (function
      | Struct { name; tparams; linear; ipos; _ } ->
          List.iter
            (fun (f : field) ->
              if region_names f.fty <> [] then
                err ipos "结构体字段 %s 不能含引用或区域（区域类型还没有实现）" f.fname;
              check_ty env0 ipos f.fty;
              if f.fty = TUnit then err ipos "C0 不支持 () 类型的字段 %s" f.fname;
              if is_linear f.fty && not linear then
                err ipos "%s 的字段 %s 是 linear 的，%s 也必须标 linear" name f.fname name)
            (Hashtbl.find struct_tbl (abstract name tparams))
      | Enum { name; tparams; ipos; _ } ->
          List.iter
            (fun (v, t) ->
              match t with
              | None -> ()
              | Some t ->
                  if region_names t <> [] then
                    err ipos "%s 的变体 %s 的载荷不能含引用或区域（区域类型还没有实现）" name v;
                  check_ty env0 ipos t;
                  if t = TUnit then err ipos "%s 的变体 %s：没有载荷请直接写 %s" name v v;
                  if is_linear t then err ipos "%s 的变体 %s：载荷暂不能是 linear 值" name v)
            (Hashtbl.find enum_tbl (abstract name tparams))
      | Impl { trait = tr; self; fns = ms; ipos } ->
          (* impl 和 trait 一致：方法一样，每个方法的参数约定和类型相同（Self 换成实现的类型，Lean M10 的 Conforms） *)
          let st = resolve [] ipos self in
          check_ty env0 ipos st;
          let what = Printf.sprintf "impl %s for %s" tr (string_of_ty st) in
          let names = Hashtbl.find traits tr in
          let got = List.filter_map (function Fn f -> Some (f.name, f.ipos) | _ -> None) ms in
          List.iteri
            (fun i (n, at) ->
              if List.mem_assoc n (List.filteri (fun j _ -> j < i) got) then err at "%s 里的方法 %s 重复" what n;
              if not (List.mem n names) then err at "trait %s 没有方法 %s" tr n)
            got;
          List.iter (fun n -> if not (List.mem_assoc n got) then err ipos "%s 缺少方法 %s" what n) names;
          let s = [ ("Self", st) ] in
          List.iter
            (fun (n, at) ->
              let tps, tret = Hashtbl.find fns n in
              let ips, iret = Hashtbl.find fns (inst_name n [ st ]) in
              if List.length tps <> List.length ips then
                err at "%s 的方法 %s 有 %d 个参数，trait 里声明的是 %d 个" what n (List.length ips) (List.length tps);
              List.iteri
                (fun i ((m1, t1), (m2, t2)) ->
                  let t1 = subst s t1 in
                  if m1 <> m2 then
                    err at "%s 的方法 %s 的第 %d 个参数约定是 %s，trait 里声明的是 %s" what n (i + 1) (mode_name m2)
                      (mode_name m1);
                  if t1 <> t2 then
                    err at "%s 的方法 %s 的第 %d 个参数类型是 %s，应为 %s" what n (i + 1) (string_of_ty t2)
                      (string_of_ty t1))
                (List.combine tps ips);
              if subst s tret <> iret then
                err at "%s 的方法 %s 返回 %s，应为 %s" what n (string_of_ty iret) (string_of_ty (subst s tret)))
            got
      | Trait { methods = ms; _ } ->
          List.iter
            (fun (m : msig) ->
              let ps, ret = Hashtbl.find fns m.mname in
              List.iter (fun (_, t) -> check_ty env0 m.mpos t; if t = TUnit then err m.mpos "C0 不支持 () 类型的参数") ps;
              check_ty env0 m.mpos ret)
            ms
      | Fn _ -> ())
    p;
  env0

(* 把函数里写出的类型换成解析后的：签名取 prepare 解析好的，函数体里的 let 标注、var 声明按类型参数解析。
   泛型函数另外记下来，实例化时复制。 *)
let resolve_item = function
  | Fn f ->
      let ps, ret = Hashtbl.find sigs f.name in
      let sc = List.map (fun (t, lin) -> (t, TParam (t, lin))) f.tparams in
      let it =
        Fn
          {
            f with
            params = List.map2 (fun (m, x, _) (_, t) -> (m, x, t)) f.params ps;
            ret;
            body = map_body ~fresh:false (resolve sc) f.body;
          }
      in
      if f.tparams <> [] then Hashtbl.replace gfns f.name it;
      it
  | it -> it

(* 整个程序的条目逐个解析：impl 换成它的方法（普通函数，名字是 方法<类型>，Self 换成实现的类型）；
   trait 只有签名，已经登记过，不再留在程序里。 *)
let resolve_items = function
  | Fn _ as it -> [ resolve_item it ]
  | Impl { self; fns; ipos; _ } ->
      let st = resolve [] ipos self in
      List.filter_map
        (function
          | Fn f ->
              let name = inst_name f.name [ st ] in
              let ps, ret = Hashtbl.find sigs name in
              Some
                (Fn
                   {
                     f with
                     name;
                     params = List.map2 (fun (m, x, _) (_, t) -> (m, x, t)) f.params ps;
                     ret;
                     body = map_body ~fresh:false (resolve [ ("Self", st) ]) f.body;
                   })
          | Struct _ | Enum _ | Trait _ | Impl _ -> None)
        fns
  | Trait _ -> []
  | (Struct _ | Enum _) as it -> [ it ]

(* 泛型函数 f 按类型实参 ts 的实例：复制函数体（表达式换新的 id），类型参数换成具体类型。 *)
let instance (f, ts) =
  match Hashtbl.find gfns f with
  | Fn g ->
      let s = List.combine (List.map fst g.tparams) ts in
      Fn
        {
          g with
          name = inst_name f ts;
          tparams = [];
          bounds = [];
          params = List.map (fun (m, x, t) -> (m, x, subst s t)) g.params;
          ret = subst s g.ret;
          body = map_body ~fresh:true (fun _ -> subst s) g.body;
        }
  | it -> it

(* 检查一个函数。 *)
let check_fn env0 = function
  | Fn { params; ret; body; ipos; bounds; _ } ->
      (* 区域参数：Arena 参数和参数类型里出现的区域名，彼此之间没有顺序（D3） *)
      regions :=
        List.sort_uniq compare (List.concat_map (fun (_, _, t) -> region_names t) params)
        |> List.map (fun r -> (r, []));
      List.iter
        (fun (m, x, t) ->
          check_ty ~param:true env0 ipos t;
          (match t with
          | TArena _ when m <> MLet -> err ipos "区域参数 %s 只能按 let 传入" x
          | _ -> ());
          if t = TUnit then err ipos "C0 不支持 () 类型的参数 %s" x)
        params;
      check_ty env0 ipos ret;
      let vars = List.rev_map (fun (m, x, t) -> (x, (t, m <> MLet))) params in
      let env = { env0 with ret; vars; tbounds = bounds } in
      expect ipos ret (block env ~want:ret body);
      regions := []
  | Struct _ | Enum _ | Trait _ | Impl _ -> ()

let check_main env0 =
  match Hashtbl.find_opt env0.fns "main" with
  | Some ([], TUnit) when not (is_generic "main") -> ()
  | Some _ -> err { line = 1; col = 1 } "main 必须是 fn main()，没有参数、没有返回值、没有类型参数"
  | None -> err { line = 1; col = 1 } "缺少 fn main()"

(* 检查整个程序。返回解析过类型的程序（impl 换成它的方法），后面接上用到的泛型函数实例（单态化），每个实例照常检查。
   方法调用在检查之后换成解析出的函数名；实例是从检查过的泛型函数复制的，复制时已经换好。 *)
let program (p : program) : program =
  let env0 = prepare p in
  let p = List.concat_map resolve_items p in
  List.iter (check_fn env0) p;
  check_main env0;
  let p =
    List.map (function Fn f -> Fn { f with body = map_body ~fresh:false (fun _ t -> t) f.body } | it -> it) p
  in
  let rec drain acc =
    match Queue.take_opt pending with
    | None -> List.rev acc
    | Some r ->
        if List.length acc >= 1000 then
          err { line = 1; col = 1 } "泛型函数的实例超过 1000 个（可能是多态递归）";
        let it = instance r in
        check_fn env0 it;
        drain (it :: acc)
  in
  p @ drain []
