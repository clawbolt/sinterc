(* 类型检查：结构体按名字区分，泛型按实例化后的名字区分（见 typeck_generics.ml）；`return` 之后的代码类型记为 Never。
   `Ok(e)` / `Err(e)` 的类型要靠上下文确定（let 标注、返回值、实参、字段），
   所以有两种检查方式：expr 自己推出类型，check 按期望类型检查。

   拆成了几个文件，按依赖的顺序：typeck_env（共用的状态）、typeck_builtins、typeck_regions、typeck_generics、
   typeck_pattern、typeck_expr，这里是整个程序的准备和检查。其他模块照旧通过 Typeck 访问它们。 *)
open Ast
include Typeck_env
include Typeck_builtins
include Typeck_regions
include Typeck_generics
include Typeck_pattern
include Typeck_expr

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
  Hashtbl.reset defaults;
  Hashtbl.reset minst;
  Hashtbl.reset aliases;
  (* 内建的 trait ToStr：fn to_str(x: Self) -> Str，字符串插值 "{x}" 用它；I64、F64、Char、Bool、Str 已经实现 *)
  Hashtbl.add traits "ToStr" [ "to_str" ];
  Hashtbl.replace methods "to_str" "ToStr";
  Hashtbl.replace fns "to_str" ([ (MLet, TParam ("Self", true)) ], TStr);
  Hashtbl.replace gparams "to_str" [ ("Self", true) ];
  Hashtbl.replace gbounds "to_str" [ ("Self", "ToStr") ];
  List.iter
    (fun t -> Hashtbl.add impls "ToStr" { head = t; itps = []; ibounds = []; defined = [ "to_str" ] })
    [ TI64; TF64; TChar; TBool; TStr ];
  Hashtbl.reset vec_calls;
  Hashtbl.reset method_res;
  Hashtbl.reset skip_lower;
  Hashtbl.reset rewrites;
  Hashtbl.reset packs;
  Hashtbl.reset fcalls;
  Hashtbl.reset pdesugar;
  Hashtbl.reset fn_items;
  Queue.clear pending_items;
  lambda_frame := None;
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
          List.iter (fun (m : msig) -> if m.mbody <> None then Hashtbl.replace defaults m.mname ()) ms;
          Hashtbl.add traits name (List.map (fun (m : msig) -> m.mname) ms)
      | Impl _ -> ())
    p;
  (* 结构体、枚举不能不经过 Box 包含自己（大小是无限的）；经过 Box 的递归可以（Lean `Datatypes.Recursive`）。
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
              err ipos "泛型函数的签名里暂不能直接写区域或引用（类型实参可以是引用）";
            Hashtbl.replace gparams name tparams
          end;
          Hashtbl.replace fns name (ps, ret)
      | Trait { name; methods = ms; _ } ->
          (* Self 可以是任何类型（包括 linear 的），调用处的效果只由参数约定决定（Lean `Traits.Static`） *)
          let sc = [ ("Self", TParam ("Self", true)) ] in
          List.iter
            (fun (m : msig) ->
              if Hashtbl.mem fns m.mname || List.mem_assoc m.mname builtins || List.mem m.mname special
                 || Hashtbl.mem methods m.mname
              then err m.mpos "方法名 %s 和已有的函数或方法同名（暂不支持重载）" m.mname;
              let ps = List.map (fun (md, _, t) -> (md, resolve sc m.mpos t)) m.mparams in
              if List.exists (fun (_, t) -> is_fn t) ps then err m.mpos "trait 的方法暂不能有 fn(…) 参数";
              let ret = resolve sc m.mpos m.mret in
              if List.exists (fun (_, t) -> region_names t <> []) ps || region_names ret <> [] then
                err m.mpos "trait 的方法暂不支持区域参数和引用";
              Hashtbl.replace fns m.mname (ps, ret);
              Hashtbl.replace gparams m.mname [ ("Self", true) ];
              Hashtbl.replace gbounds m.mname [ ("Self", name) ];
              Hashtbl.replace methods m.mname name)
            ms
      | Impl { trait = tr; tparams; bounds; self; fns = ms; ipos } ->
          if not (Hashtbl.mem traits tr) then err ipos "未知 trait %s" tr;
          tparams_ok "" ipos tparams;
          List.iter
            (fun (t, _) ->
              if defined t then err ipos "类型参数 %s 和已有的类型同名" t;
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
                  let key = inst_name name [ st ] in
                  Hashtbl.replace fns key
                    (List.map (fun (md, _, t) -> (md, resolve sc ipos t)) params, resolve sc ipos ret);
                  (* 泛型 impl 的方法是泛型函数 方法<头部>，类型参数和约束是 impl 的 *)
                  if tparams <> [] then begin
                    Hashtbl.replace gparams key tparams;
                    if bounds <> [] then Hashtbl.replace gbounds key bounds
                  end
              | Struct _ | Enum _ | Trait _ | Impl _ -> ())
            ms)
    p;
  Hashtbl.reset sigs;
  Hashtbl.iter (Hashtbl.replace sigs) fns;
  List.iter (fun (f, s) -> Hashtbl.replace sigs f s) (builtins @ vec_sigs @ view_sigs);
  let env0 = { structs = struct_tbl; fns; ret = TUnit; vars = []; guards = []; tbounds = []; clos = [] } in
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
      | Impl { trait = tr; tparams; self; fns = ms; ipos; _ } ->
          (* impl 和 trait 一致：方法一样，每个方法的参数约定和类型相同（Self 换成实现的类型，Lean `Traits.Static` 的 Conforms） *)
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
            (fun n -> if not (List.mem_assoc n got || Hashtbl.mem defaults n) then err ipos "%s 缺少方法 %s" what n)
            names;
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
              List.iter (fun (_, t) -> check_ty env0 m.mpos t) ps;
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
   泛型 impl 的方法是泛型函数，类型参数和约束是 impl 的。
   trait 的签名已经登记过，不再留在程序里；默认方法是泛型函数 方法，类型参数 Self、约束 Self: trait。 *)
let resolve_items = function
  | Fn _ as it -> [ resolve_item it ]
  | Impl { tparams; bounds; self; fns; ipos; _ } ->
      let tsc = List.map (fun (t, lin) -> (t, TParam (t, lin))) tparams in
      let st = resolve tsc ipos self in
      List.filter_map
        (function
          | Fn f ->
              let name = inst_name f.name [ st ] in
              let ps, ret = Hashtbl.find sigs name in
              let it =
                Fn
                  {
                    f with
                    name;
                    tparams;
                    bounds;
                    params = List.map2 (fun (m, x, _) (_, t) -> (m, x, t)) f.params ps;
                    ret;
                    body = map_body ~fresh:false (resolve (("Self", st) :: tsc)) f.body;
                  }
              in
              if tparams <> [] then Hashtbl.replace gfns name it;
              Some it
          | Struct _ | Enum _ | Trait _ | Impl _ -> None)
        fns
  | Trait { name = tr; methods = ms; _ } ->
      List.filter_map
        (fun (m : msig) ->
          Option.map
            (fun body ->
              resolve_item
                (Fn
                   {
                     name = m.mname;
                     tparams = [ ("Self", true) ];
                     bounds = [ ("Self", tr) ];
                     params = m.mparams;
                     ret = m.mret;
                     body;
                     ipos = m.mpos;
                   }))
            m.mbody)
        ms
  | (Struct _ | Enum _) as it -> [ it ]

(* 检查一个函数。特化：clos 是 fn(…) 参数绑定的闭包，fvars 是这些参数（已经从参数里去掉，函数体里仍按名字用）。
   rord：闭包捕获的区域参数之间的先后关系（`Closures.CapRef`）；其余区域参数之间没有顺序（D3）。 *)
let check_fn ?(clos = []) ?(fvars = []) ?(rord = []) env0 = function
  | Fn { name; tparams; params; ret; body; ipos; bounds } ->
      cur_fn := name;
      concrete := tparams = [] && not (List.exists (fun (_, _, t) -> is_fn t) params);
      (* 区域参数：Arena 参数和参数类型里出现的区域名，彼此之间没有顺序（D3） *)
      regions :=
        List.sort_uniq compare (List.concat_map (fun (_, _, t) -> region_names t) params)
        |> List.map (fun r -> (r, Option.value (List.assoc_opt r rord) ~default:[]));
      List.iter
        (fun (m, x, t) ->
          check_ty ~param:true env0 ipos t;
          (match t with
          | TArena _ when m <> MLet -> err ipos "区域参数 %s 只能按 let 传入" x
          | TFn _ when m <> MLet -> err ipos "函数类型的参数 %s 只能按 let 传入（不写约定）" x
          | _ -> ()))
        params;
      check_ty env0 ipos ret;
      let vars = List.rev_map (fun (m, x, t) -> (x, (t, m <> MLet))) params in
      let_params := List.filter_map (fun (m, x, _) -> if m = MLet then Some x else None) params;
      let vars = List.map (fun (y, t) -> (y, (t, false))) fvars @ vars in
      let env = { env0 with ret; vars; tbounds = bounds; clos } in
      expect ipos ret (block env ~want:ret body);
      regions := []
  | Struct _ | Enum _ | Trait _ | Impl _ -> ()

let check_main env0 =
  match Hashtbl.find_opt env0.fns "main" with
  | Some ([], TUnit) when not (is_generic "main") -> ()
  | Some _ -> err { line = 1; col = 1 } "main 必须是 fn main()，没有参数、没有返回值、没有类型参数"
  | None -> err { line = 1; col = 1 } "缺少 fn main()"

(* fn to_str(x: P) -> Str 是 impl ToStr for P { … } 的简写（可以给多种类型各写一个），
   fn to_str<T: ToStr>(v: Vec<T>) -> Str 是泛型 impl 的简写。
   模块里的 m.to_str(x: P) -> Str 照常是函数，另外为 P 实现 ToStr（调用它），"{x}" 也就能用了。 *)
let sugar_to_str (p : program) =
  let suffix = ".to_str" in
  let qualified n =
    let k = String.length n and l = String.length suffix in
    k > l && String.sub n (k - l) l = suffix
  in
  List.concat_map
    (function
      | Fn ({ name = "to_str"; tparams; bounds; params; ret; ipos; _ } as f) -> (
          match params, ret with
          | [ (MLet, _, self) ], TStr ->
              [ Impl { trait = "ToStr"; tparams; bounds; self; fns = [ Fn { f with tparams = []; bounds = [] } ]; ipos } ]
          | _ -> err ipos "to_str 是 trait ToStr 的方法，要写成 fn to_str(x: T) -> Str")
      | Fn ({ name; tparams = []; params = [ (MLet, x, self) ]; ret = TStr; ipos; _ } as f) as it when qualified name ->
          let call = { e = Call (name, [ { e = Var x; pos = ipos; id = fresh_id () } ]); pos = ipos; id = fresh_id () } in
          let body = { stmts = []; tail = Some call } in
          [ it; Impl { trait = "ToStr"; tparams = []; bounds = []; self; fns = [ Fn { f with name = "to_str"; body } ]; ipos } ]
      | it -> [ it ])
    p

(* 检查整个程序。返回解析过类型的程序（impl 换成它的方法），后面接上用到的泛型函数实例（单态化），每个实例照常检查。
   方法调用在检查之后换成解析出的函数名；实例是从检查过的泛型函数复制的，复制时已经换好。
   闭包：提升出的闭包和特化也接在后面、照常检查；调用的改写在全部检查完之后换上。
   带 fn(…) 参数的函数本身不输出（只输出它的特化），函数体里有闭包的泛型函数不在定义处降级。 *)
let program (p : program) : program =
  let p = sugar_to_str p in
  let env0 = prepare p in
  let p = List.concat_map resolve_items p in
  List.iter (function Fn f as it -> Hashtbl.replace fn_items f.name it | _ -> ()) p;
  (* 函数之间互不影响，每个函数报它的第一个错误；有错误时不再生成泛型实例（免得同一个错误报好几次） *)
  let errs = ref [] in
  let guard g x =
    try g x with Error (pos, msg) ->
      regions := [];
      errs := (pos, msg) :: !errs
  in
  List.iter (guard (check_fn env0)) p;
  guard check_main env0;
  if !errs <> [] then raise (Errors (List.rev !errs));
  let rec drain acc =
    if List.length acc >= 1000 then
      err { line = 1; col = 1 } "泛型函数的实例超过 1000 个（可能是多态递归）";
    match Queue.take_opt pending with
    | Some r ->
        let it = instance r in
        guard (check_fn env0) it;
        drain (it :: acc)
    | None -> (
        match Queue.take_opt pending_items with
        | Some (it, clos, fvars, rord) ->
            guard (check_fn ~clos ~fvars ~rord env0) it;
            drain (it :: acc)
        | None -> List.rev acc)
  in
  let insts = drain [] in
  if !errs <> [] then raise (Errors (List.rev !errs));
  List.filter_map
    (function
      | Fn f when List.exists (fun (_, _, t) -> is_fn t) f.params -> None
      | Fn f when f.tparams <> [] && Hashtbl.mem skip_lower f.name -> None
      | Fn f -> Some (Fn { f with body = map_body ~fresh:false (fun _ t -> t) f.body })
      | it -> Some it)
    (p @ insts)

(* 对照测试（--batch）：程序里的函数逐个检查，每个连同它引出的泛型实例、提升出的闭包和特化一起，
   返回要降级的条目（规则和 program 相同）。出错时清空队列再抛出，不影响下一个函数。
   调用前要用 batch_items 登记所有函数（特化时按名字找函数）。 *)
let batch_items (p : program) =
  let p = List.map resolve_item p in
  List.iter (function Fn f as it -> Hashtbl.replace fn_items f.name it | _ -> ()) p;
  p

let check_batch env0 it =
  try
    check_fn env0 it;
    let rec drain acc =
      match Queue.take_opt pending with
      | Some r ->
          let i = instance r in
          check_fn env0 i;
          drain (i :: acc)
      | None -> (
          match Queue.take_opt pending_items with
          | Some (i, clos, fvars, rord) ->
              check_fn ~clos ~fvars ~rord env0 i;
              drain (i :: acc)
          | None -> List.rev acc)
    in
    let insts = drain [] in
    List.filter_map
      (function
        | Fn f when List.exists (fun (_, _, t) -> is_fn t) f.params -> None
        | Fn f when f.tparams <> [] && Hashtbl.mem skip_lower f.name -> None
        | Fn f -> Some (Fn { f with body = map_body ~fresh:false (fun _ t -> t) f.body })
        | it -> Some it)
      (it :: insts)
  with e ->
    Queue.clear pending;
    Queue.clear pending_items;
    regions := [];
    raise e
