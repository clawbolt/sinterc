(* 整个程序的定义的登记和检查（Lean `Traits.*`、`Generics.Check`、`Datatypes.*`）：
   结构体、枚举（含泛型定义）、函数签名、trait 和 impl、类型自己的方法、字段和载荷的规则。
   以前都在 typeck.ml 的 prepare 一个函数里，这里按步骤拆开，执行的顺序不变（见 run）。
   现在结果还是写进 typing.ml 的第一组表；R4a 的下一步改成产出 Defs.t。 *)
open Ast
open Typeck_env
open Typeck_builtins
open Typeck_regions
open Typeck_generics
open Typeck_expr

(* ---------- 内建的定义 ---------- *)

(* 内建的 trait ToStr：fn to_str(x: Self) -> Str，字符串插值 "{x}" 用它；I64、F64、Char、Bool、Str 已经实现。
   内建的 Option<T>：vec_pop 返回它，写法和用户的枚举一样（.Some(x) / .None）。 *)
let builtin_defs fns =
  let k = trait_key "ToStr" "to_str" in
  Hashtbl.add traits "ToStr" [ "to_str" ];
  Hashtbl.replace methods k "ToStr";
  Hashtbl.add tmethods "to_str" ("ToStr", k);
  Hashtbl.replace fns k ([ (MLet, TParam ("Self", true)) ], TStr);
  Hashtbl.replace gparams k [ ("Self", true) ];
  Hashtbl.replace gbounds k [ ("Self", "ToStr") ];
  List.iter
    (fun t -> Hashtbl.add impls "ToStr" { head = t; itps = []; ibounds = []; defined = [ "to_str" ] })
    [ TI64; TF64; TChar; TBool; TStr ];
  Hashtbl.add gtypes "Option"
    ( [ "T" ],
      Enum
        {
          name = "Option";
          tparams = [ ("T", false) ];
          variants = [ ("Some", Some (TStruct "T")); ("None", None) ];
          ipos = Source.start;
        } )

(* ---------- 名字 ---------- *)

let defined n = Hashtbl.mem struct_tbl n || Hashtbl.mem enum_tbl n || Hashtbl.mem gtypes n

(* 类型参数和已有的类型同名：解析时类型参数优先，这条只防止用户写出看不清的代码；
   预导入的核心部分不检查，否则用户定义的类型 T、E 会和它的 impl<T> Option<T> 冲突 *)
let clash ipos t = defined t && not (Modules.in_core ipos)

(* what 不为空：结构体、枚举的类型参数，不能标 linear *)
let tparams_ok what ipos tps =
  List.iteri
    (fun i (t, lin) ->
      if List.mem_assoc t (List.filteri (fun j _ -> j < i) tps) then err ipos "类型参数 %s 重复" t;
      if lin && what <> "" then err ipos "%s的类型参数 %s 不能标 linear（只有函数的类型参数可以）" what t)
    tps

(* 先登记所有名字，字段和载荷的类型等名字都登记之后再解析 *)
let register_name fns = function
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
      List.iter (fun (m : msig) -> if m.mbody <> None then Hashtbl.replace defaults (trait_key name m.mname) ()) ms;
      Hashtbl.add traits name (List.map (fun (m : msig) -> m.mname) ms)
  | Impl _ -> ()

(* ---------- 类型 ---------- *)

(* 结构体、枚举不能不经过 Box 包含自己（大小是无限的）；经过 Box 的递归可以（Lean `Datatypes.Recursive`）。
   泛型类型的实参也算包含（保守：实参只放在 Box 里时也拒绝）。 *)
let check_finite () =
  let rec visit path n =
    if List.mem n path then
      err Source.start "类型 %s 不经过 Box 包含自己，大小是无限的（递归要经过 Box，例如 next: Box<List>）" n;
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
  Hashtbl.iter (fun n _ -> visit [] n) gtypes

(* 解析非泛型结构体、枚举的字段和载荷（Option<I64> 这样的写法换成实例） *)
let resolve_fields = function
  | Struct { name; tparams = []; fields; ipos; _ } ->
      Hashtbl.replace struct_tbl name (List.map (fun (f : field) -> { f with fty = resolve [] ipos f.fty }) fields)
  | Enum { name; tparams = []; variants; ipos; _ } ->
      Hashtbl.replace enum_tbl name (List.map (fun (v, t) -> (v, Option.map (resolve [] ipos) t)) variants)
  | Struct _ | Enum _ | Fn _ | Trait _ | Impl _ -> ()

(* ---------- 类型自己的方法和内建方法 ---------- *)

(* 类型自己的方法的头部（Lean `Traits.Inherent`）：impl 的类型参数都出现在头部里（实例的类型实参由接收者匹配得到），
   同名方法的头部不能重叠（CoherentM），所以查找的结果和书写顺序无关。按写出的顺序检查，重叠时报后写的那个 *)
let register_inherent (res : Resolved.t) =
  Hashtbl.reset inherent_heads;
  List.iter
    (fun (m, ({ Resolved.head = self; tparams = tps; fn_name = g; pos = ipos } : Resolved.inherent)) ->
      let st = resolve (List.map (fun (t, lin) -> (t, TParam (t, lin))) tps) ipos self in
      (match st with
      | TArena _ -> err ipos "不能给 Arena 写方法"
      | TFn _ | TOFn _ -> err ipos "不能给函数类型写方法"
      | TParam (t, _) -> err ipos "不能给类型参数 %s 本身写方法（impl<%s> %s 会和所有的头部重叠）" t t t
      | _ -> ());
      List.iter
        (fun (t, _) ->
          if not (List.mem t (params_of st)) then err ipos "impl 的类型参数 %s 没有出现在 %s 里" t (string_of_ty st))
        tps;
      List.iter
        (fun (h, _, _) ->
          if compat ~loose:true h st then
            err ipos "impl %s 和 impl %s 都有方法 %s，头部重叠（有的类型两个都能匹配）" (string_of_ty st) (string_of_ty h) m)
        (Hashtbl.find_all inherent_heads m);
      Hashtbl.add inherent_heads m (st, List.map fst tps, g))
    (Hashtbl.fold (fun m v acc -> (m, v) :: acc) res.Resolved.inherent [])

(* intrinsic fn 的签名要和编译器里的内建函数一致（参数约定和类型都相同），核心部分里写的就是真正的签名。
   类型不固定的（Vec 的、切片的，见 special）由类型检查单独处理，只检查有这个内建函数 *)
let check_prims (res : Resolved.t) =
  Hashtbl.iter
    (fun g ({ Resolved.builtin = b; params = ps; ret = r; tparams = tps; pos = ipos } : Resolved.prim) ->
      let tenv = List.map (fun (t, lin) -> (t, TParam (t, lin))) tps in
      match List.assoc_opt b builtins with
      | Some (bps, br) ->
          let same =
            List.length ps = List.length bps
            && List.for_all2 (fun (m, _, t) (m', t') -> m = m' && resolve tenv ipos t = resolve [] ipos t') ps bps
            && resolve tenv ipos r = resolve [] ipos br
          in
          if not same then err ipos "%s 的声明和内建函数 %s 的签名不一致" g b
      | None -> if not (List.mem b special) then err ipos "没有内建函数 %s" b)
    res.Resolved.prims

(* ---------- 签名 ----------
   泛型函数的类型参数解析成 TParam；trait 的方法是带类型参数 Self 的泛型函数；impl 里的方法登记成 方法<类型> *)

let type_params_unique = function
  | Struct { tparams; ipos; _ } | Enum { tparams; ipos; _ } ->
      List.iter (fun (t, _) -> if clash ipos t then err ipos "类型参数 %s 和已有的类型同名" t) tparams
  | Fn _ | Trait _ | Impl _ -> ()

let fn_signature fns = function
  | Fn { name; tparams; bounds; params; ret; ipos; _ } ->
      tparams_ok "" ipos tparams;
      List.iter
        (fun (t, _) ->
          if clash ipos t then err ipos "类型参数 %s 和已有的类型同名" t;
          if t = "Self" then err ipos "Self 不能用作类型参数的名字")
        tparams;
      List.iter (fun (_, tr) -> if not (Hashtbl.mem traits tr) then err ipos "未知 trait %s" tr) bounds;
      if bounds <> [] then Hashtbl.replace gbounds name bounds;
      let sc = List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams in
      let ps = List.map (fun (m, _, t) -> (m, resolve sc ipos t)) params in
      let ret = resolve sc ipos ret in
      (* 返回借用（Lean `Projection.Return`）：返回类型里的区域名是参数的名字时，这个参数要按 let 传入 *)
      let lr =
        List.concat
          (List.mapi
             (fun i (m, x, t) ->
               if not (List.mem x (region_names ret)) then []
               else
                 match m, t with
                 | MLet, TArena _ -> []
                 | MLet, _ -> [ (x, i) ]
                 | _ -> err ipos "返回的引用只能借自 let 参数，%s 是 %s 参数" x (if m = MInout then "inout" else "sink"))
             params)
      in
      if lr <> [] then Hashtbl.replace let_regions name lr;
      (* 泛型函数的签名里可以写区域参数和引用（Lean `Regions.DataGeneric`：函数体把区域参数当作抽象区域检查一次，
         调用处按实际区域换名；值里可以有一组引用，例如迭代器 MapIter<&Map<K, V> in r, &K in r>） *)
      if tparams <> [] then Hashtbl.replace gparams name tparams;
      Hashtbl.replace fns name (ps, ret)
  | Struct _ | Enum _ | Trait _ | Impl _ -> ()

(* Self 可以是任何类型（包括 linear 的），调用处的效果只由参数约定决定（Lean `Traits.Static`）。
   方法的函数名是 trait.方法，不和函数、别的 trait 的方法冲突 *)
let trait_signatures fns = function
  | Trait { name; methods = ms; _ } ->
      let sc = [ ("Self", TParam ("Self", true)) ] in
      List.iter
        (fun (m : msig) ->
          let k = trait_key name m.mname in
          let ps = List.map (fun (md, _, t) -> (md, resolve sc m.mpos t)) m.mparams in
          if ps = [] then err m.mpos "trait 的方法 %s 至少要有一个参数（接收者）" m.mname;
          if List.exists (fun (_, t) -> is_fn t) ps then err m.mpos "trait 的方法暂不能有 fn(…) 参数";
          let ret = resolve sc m.mpos m.mret in
          if List.exists (fun (_, t) -> region_names t <> []) ps || region_names ret <> [] then
            err m.mpos "trait 的方法暂不支持区域参数和引用";
          Hashtbl.replace fns k (ps, ret);
          Hashtbl.replace gparams k [ ("Self", true) ];
          Hashtbl.replace gbounds k [ ("Self", name) ];
          Hashtbl.replace methods k name;
          Hashtbl.add tmethods m.mname (name, k))
        ms
  | Struct _ | Enum _ | Fn _ | Impl _ -> ()

let impl_signatures fns = function
  | Impl { trait = tr; tparams; bounds; self; fns = ms; ipos } ->
      if not (Hashtbl.mem traits tr) then err ipos "未知 trait %s" tr;
      tparams_ok "" ipos tparams;
      List.iter
        (fun (t, _) ->
          if clash ipos t then err ipos "类型参数 %s 和已有的类型同名" t;
          if t = "Self" then err ipos "Self 不能用作类型参数的名字")
        tparams;
      List.iter (fun (_, b) -> if not (Hashtbl.mem traits b) then err ipos "未知 trait %s" b) bounds;
      let tsc = List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams in
      let st = resolve tsc ipos self in
      type_arg ipos st;
      if region_names st <> [] then err ipos "暂不支持为含引用的类型 %s 实现 trait" (string_of_ty st);
      (match st with
      | TArena _ -> err ipos "不能为 Arena 实现 trait"
      | TFn _ | TOFn _ -> err ipos "不能为函数类型实现 trait"
      | TParam (t, _) -> err ipos "不能为类型参数 %s 本身实现 trait（impl<%s> … for %s 会和所有的 impl 重叠）" t t t
      | _ -> ());
      (* `Traits.ImplSearch` 的 WF：impl 的类型参数都要出现在头部里（实例的类型实参由匹配得到） *)
      List.iter
        (fun (t, _) ->
          if not (List.mem t (params_of st)) then
            err ipos "impl 的类型参数 %s 没有出现在 %s 里" t (string_of_ty st))
        tparams;
      (* `Traits.ImplSearch` 的 Coherent：同一个 trait 的两个 impl 不能有共同的实例 *)
      List.iter
        (fun i ->
          if compat i.head st then
            if params_of i.head = [] && params_of st = [] then
              err ipos "%s 已经实现过 %s" (string_of_ty st) tr
            else
              err ipos "impl %s for %s 和已有的 impl %s for %s 重叠（有的类型两个都能匹配）" tr (string_of_ty st) tr
                (string_of_ty i.head))
        (Hashtbl.find_all impls tr);
      let defined = List.filter_map (function Fn f -> Some f.name | _ -> None) ms in
      Hashtbl.add impls tr { head = st; itps = List.map fst tparams; ibounds = bounds; defined };
      List.iter
        (function
          | Fn { name; tparams = ftps; params; ret; ipos; _ } ->
              if ftps <> [] then err ipos "impl 里的方法暂不能有类型参数";
              if List.exists (fun (_, _, t) -> is_fn t) params then err ipos "impl 里的方法暂不能有 fn(…) 参数";
              let sc = ("Self", st) :: tsc in
              let key = inst_name (trait_key tr name) [ st ] in
              Hashtbl.replace fns key
                (List.map (fun (md, _, t) -> (md, resolve sc ipos t)) params, resolve sc ipos ret);
              (* 泛型 impl 的方法是泛型函数 方法<头部>，类型参数和约束是 impl 的 *)
              if tparams <> [] then begin
                Hashtbl.replace gparams key tparams;
                if bounds <> [] then Hashtbl.replace gbounds key bounds
              end
          | Struct _ | Enum _ | Trait _ | Impl _ -> ())
        ms
  | Struct _ | Enum _ | Fn _ | Trait _ -> ()

(* 降级用的签名表：检查用的签名，加上内建函数的 *)
let build_sigs fns =
  Hashtbl.reset sigs;
  Hashtbl.iter (Hashtbl.replace sigs) fns;
  List.iter (fun (f, s) -> Hashtbl.replace sigs f s) (builtins @ vec_sigs @ view_sigs)

(* ---------- 字段、载荷和 impl 的规则 ----------
   泛型定义按类型参数是抽象类型的实例检查 *)

let abstract name tparams =
  if tparams = [] then name
  else match instantiate name (List.map (fun (t, _) -> TParam (t, false)) tparams) with TStruct n -> n | _ -> name

let check_struct env0 = function
  | Struct { name; tparams; linear; ipos; _ } ->
      List.iter
        (fun (f : field) ->
          if region_names f.fty <> [] then
            err ipos "结构体字段 %s 不能含引用或区域（区域类型还没有实现）" f.fname;
          check_ty env0 ipos f.fty;
          if is_linear f.fty && not linear then
            err ipos "%s 的字段 %s 是 linear 的，%s 也必须标 linear" name f.fname name)
        (Hashtbl.find struct_tbl (abstract name tparams))
  | Enum _ | Fn _ | Trait _ | Impl _ -> ()

let check_enum env0 = function
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
  | Struct _ | Fn _ | Trait _ | Impl _ -> ()

(* impl 和 trait 一致：方法一样，每个方法的参数约定和类型相同（Self 换成实现的类型，Lean `Traits.Static` 的 Conforms） *)
let check_impl env0 fns = function
  | Impl { trait = tr; tparams; self; fns = ms; ipos; _ } ->
      let st = resolve (List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams) ipos self in
      check_ty env0 ipos st;
      let what = Printf.sprintf "impl %s for %s" tr (string_of_ty st) in
      let names = Hashtbl.find traits tr in
      let got = List.filter_map (function Fn f -> Some (f.name, f.ipos) | _ -> None) ms in
      List.iteri
        (fun i (n, at) ->
          if List.mem_assoc n (List.filteri (fun j _ -> j < i) got) then err at "%s 里的方法 %s 重复" what n;
          if not (List.mem n names) then err at "trait %s 没有方法 %s" tr n)
        got;
      List.iter
        (fun n -> if not (List.mem_assoc n got || Hashtbl.mem defaults (trait_key tr n)) then err ipos "%s 缺少方法 %s" what n)
        names;
      let s = [ ("Self", st) ] in
      List.iter
        (fun (n, at) ->
          let tps, tret = Hashtbl.find fns (trait_key tr n) in
          let ips, iret = Hashtbl.find fns (inst_name (trait_key tr n) [ st ]) in
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
  | Struct _ | Enum _ | Fn _ | Trait _ -> ()

let check_trait env0 fns = function
  | Trait { name; methods = ms; _ } ->
      List.iter
        (fun (m : msig) ->
          let ps, ret = Hashtbl.find fns (trait_key name m.mname) in
          List.iter (fun (_, t) -> check_ty env0 m.mpos t) ps;
          check_ty env0 m.mpos ret)
        ms
  | Struct _ | Enum _ | Fn _ | Impl _ -> ()

(* ---------- 整个程序 ---------- *)

(* 登记和检查整个程序的定义，返回检查函数体用的环境。每一步都要在前一步登记完之后做：
   名字都登记了才能解析字段的类型，签名都登记了才能检查 impl 和 trait 是否一致。 *)
let run (res : Resolved.t) =
  reset ();
  resolved := res;
  let p = res.Resolved.items in
  let fns = Hashtbl.create 16 in
  builtin_defs fns;
  List.iter (register_name fns) p;
  check_finite ();
  List.iter resolve_fields p;
  Hashtbl.reset let_regions;
  Hashtbl.replace let_regions "str_view" [ ("s", 0) ];
  register_inherent res;
  check_prims res;
  List.iter
    (fun it ->
      type_params_unique it;
      fn_signature fns it;
      trait_signatures fns it;
      impl_signatures fns it)
    p;
  build_sigs fns;
  let env0 =
    {
      structs = struct_tbl;
      fns;
      ret = TUnit;
      vars = [];
      guards = [];
      tbounds = [];
      clos = [];
      cur = no_fn;
      regions = [];
      loop_depth = 0;
      lambda_frame = None;
      scopes = [];
    }
  in
  List.iter
    (fun it ->
      check_struct env0 it;
      check_enum env0 it;
      check_impl env0 fns it;
      check_trait env0 fns it)
    p;
  env0
