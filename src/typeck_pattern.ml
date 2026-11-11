(* 嵌套模式的检查、覆盖性和展开。 *)
open Ast
open Typeck_env
open Typeck_generics

(* ---- 嵌套模式（Lean `Matching.Nested`、`Matching.Coverage`） ---- *)

let pm_count = ref 0
let fresh_pvar () = incr pm_count; "#p" ^ string_of_int !pm_count
let mkx pos e = { e; pos; id = fresh_id () }

let lit_key (l : expr) =
  match l.e with
  | Int n -> "i" ^ Int64.to_string n
  | Unop (Neg, { e = Int n; _ }) -> "i" ^ Int64.to_string (Int64.neg n)
  | Bool b -> "b" ^ string_of_bool b
  | Str s -> "s" ^ s
  | Char c -> "c" ^ string_of_int c
  | _ -> ""

let lit_ty (l : expr) =
  match l.e with Int _ | Unop (Neg, _) -> TI64 | Bool _ -> TBool | Str _ -> TStr | Char _ -> TChar | _ -> TUnit

let show_key k =
  let rest = String.sub k 1 (String.length k - 1) in
  match k.[0] with
  | 's' -> "\"" ^ rest ^ "\""
  | 'c' ->
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b (Uchar.of_int (int_of_string rest));
      "'" ^ Buffer.contents b ^ "'"
  | _ -> rest

let copy_lit (l : expr) =
  mkx l.pos (match l.e with Unop (Neg, a) -> Unop (Neg, mkx a.pos a.e) | d -> d)

let rec pvars (q : pat) =
  match q.p with
  | PBind x -> [ x ]
  | PVariant (_, Some r) -> pvars r
  | PTuple qs -> List.concat_map pvars qs
  | PWild | PLit _ | PVariant (_, None) -> []

(* `Matching.Coverage` 的 depth、lits *)
let rec pat_depth (q : pat) =
  match q.p with
  | PWild | PBind _ -> 0
  | PLit _ | PVariant (_, None) -> 1
  | PVariant (_, Some r) -> pat_depth r + 1
  | PTuple qs -> 1 + List.fold_left (fun m r -> max m (pat_depth r)) 0 qs

let rec pat_lits (q : pat) =
  match q.p with
  | PLit l -> [ lit_key l ]
  | PVariant (_, Some r) -> pat_lits r
  | PTuple qs -> List.concat_map pat_lits qs
  | PWild | PBind _ | PVariant (_, None) -> []

(* 元组类型的各个分量 *)
let tuple_tys env ty =
  match ty with
  | TStruct m -> (
      match inst_of m with
      | Some g when Ast.is_tuple g ->
          Option.map (List.map (fun (f : field) -> f.fty)) (Hashtbl.find_opt env.structs m)
      | _ -> None)
  | _ -> None

(* 检查模式和值的类型相符 *)
let rec check_pat env ty (q : pat) =
  match q.p with
  | PWild | PBind _ -> ()
  | PLit l ->
      let t = lit_ty l in
      if t <> ty then err q.ppos "模式 %s 的类型是 %s，这里的值是 %s" (show_key (lit_key l)) (string_of_ty t) (string_of_ty ty)
  | PVariant (n, sub) -> (
      let w = vname n in
      match variants ty with
      | None -> err q.ppos "%s 不是枚举，不能用变体模式 %s" (string_of_ty ty) w
      | Some vs -> (
          if not (List.mem_assoc n vs) then err q.ppos "%s 没有变体 %s" (string_of_ty ty) w;
          match List.assoc n vs, sub with
          | None, Some _ -> err q.ppos "%s 没有载荷，不能写 %s(…)" w w
          | (None | Some TUnit), None -> ()
          | Some t, None -> err q.ppos "%s 带一个 %s 类型的载荷，要写 %s(…)" w (string_of_ty t) w
          | Some t, Some r -> check_pat env t r))
  | PTuple qs -> (
      match tuple_tys env ty with
      | Some ts when List.length ts = List.length qs -> List.iter2 (check_pat env) ts qs
      | _ -> err q.ppos "%s 不是 %d 个元素的元组，不能用元组模式" (string_of_ty ty) (List.length qs))

(* `Matching.Coverage` 的代表值：Other 代表“不等于任何写出的字面量的值”，Any 代表深度用完以后的任意值。 *)
type aval = AAny | AOther | ALit of string | ATag of string * aval option | ATup of aval list

exception Too_many

let reps env keys depth ty =
  let count = ref 0 in
  let add l =
    count := !count + List.length l;
    if !count > 100000 then raise Too_many;
    l
  in
  let rec go d ty =
    if d = 0 then [ AAny ]
    else
      match ty with
      | TBool -> [ ALit "btrue"; ALit "bfalse" ]
      | TI64 | TChar | TStr ->
          let pre = match ty with TI64 -> 'i' | TChar -> 'c' | _ -> 's' in
          add (List.filter_map (fun k -> if k <> "" && k.[0] = pre then Some (ALit k) else None) keys @ [ AOther ])
      | _ -> (
          match variants ty, tuple_tys env ty with
          | Some vs, _ ->
              add
                (List.concat_map
                   (fun (n, pty) ->
                     match pty with
                     | None -> [ ATag (n, None) ]
                     | Some t -> List.map (fun r -> ATag (n, Some r)) (go (d - 1) t))
                   vs)
          | None, Some ts ->
              let rec prod = function
                | [] -> [ [] ]
                | t :: ts ->
                    let rs = go (d - 1) t and rest = prod ts in
                    List.concat_map (fun r -> List.map (fun l -> r :: l) rest) rs
              in
              add (List.map (fun l -> ATup l) (prod ts))
          | None, None -> [ AAny ])
  in
  go depth ty

let rec amatch (q : pat) r =
  match q.p, r with
  | (PWild | PBind _), _ -> true
  | PLit l, ALit k -> lit_key l = k
  | PVariant (n, None), ATag (m, _) -> n = m
  | PVariant (n, Some q'), ATag (m, Some r') -> n = m && amatch q' r'
  | PTuple qs, ATup rs -> List.length qs = List.length rs && List.for_all2 amatch qs rs
  | _ -> false

(* `Matching.Coverage` 的 aselect：第一个匹配的分支 *)
let first_arm arms r =
  let rec go i = function [] -> None | (q, _) :: rest -> if amatch q r then Some i else go (i + 1) rest in
  go 0 arms

let rec show_aval = function
  | AAny | AOther -> "_"
  | ALit k -> show_key k
  | ATag (n, None) -> vname n
  | ATag (n, Some r) -> vname n ^ "(" ^ show_aval r ^ ")"
  | ATup rs -> "(" ^ String.concat ", " (List.map show_aval rs) ^ ")"

let unreachable pos = mkx pos (Call ("panic", [ mkx pos (Str "match 走到了不可能的分支（编译器的错误）") ]))

(* `Matching.Nested` 的 test：按借用检查是否匹配（match let、==）；None 表示总是匹配 *)
let rec ptest env pos ty (mkp : unit -> expr) (q : pat) : expr option =
  match q.p with
  | PWild | PBind _ -> None
  | PLit l ->
      Some
        (mkx pos
           (match lit_ty l with
           | TStr -> Call ("str_eq", [ mkp (); copy_lit l ])
           | _ -> Binop (Eq, mkp (), copy_lit l)))
  | PVariant (n, sub) ->
      let vs = Option.get (variants ty) in
      let y = fresh_pvar () in
      let inner, binder =
        match List.assoc n vs, sub with
        | Some t, Some r when t <> TUnit -> (ptest env pos t (fun () -> mkx pos (Var y)) r, y)
        | Some t, _ when t <> TUnit -> (None, "_")
        | _ -> (None, "")
      in
      let yes = match inner with Some t -> t | None -> mkx pos (Bool true) in
      let others = if List.length vs > 1 then [ ("_", "", mkx pos (Bool false)) ] else [] in
      Some (mkx pos (Match (true, mkp (), (n, binder, yes) :: others)))
  | PTuple qs -> (
      let ts = Option.get (tuple_tys env ty) in
      let parts =
        List.concat
          (List.mapi
             (fun i (t, r) -> Option.to_list (ptest env pos t (fun () -> mkx pos (Field (mkp (), string_of_int i))) r))
             (List.combine ts qs))
      in
      match parts with
      | [] -> None
      | a :: rest -> Some (List.fold_left (fun acc b -> mkx pos (Binop (And, acc, b))) a rest))

(* `Matching.Nested` 的 extract：只对选中的分支拆开、绑定变量（按移动，match let 时按借用）；标签不对的位置走不到 *)
let rec pextract env pos borrow ty (mkp : unit -> expr) (q : pat) (k : expr) : expr =
  let blockx stmts tail = mkx pos (Block { stmts; tail = Some tail }) in
  match q.p with
  | PWild | PLit _ -> k
  | PBind x -> blockx [ { s = Let (false, x, None, mkp ()); spos = pos } ] k
  | PVariant (n, sub) ->
      let vs = Option.get (variants ty) in
      let binder, inner =
        match List.assoc n vs, sub with
        | Some _, Some { p = PBind x; _ } -> (x, k)
        | Some t, Some r when t <> TUnit ->
            let y = fresh_pvar () in
            (y, pextract env pos borrow t (fun () -> mkx pos (Var y)) r k)
        | Some t, _ when t <> TUnit -> ("_", k)
        | _ -> ("", k)
      in
      let others = if List.length vs > 1 then [ ("_", "", unreachable pos) ] else [] in
      mkx pos (Match (borrow, mkp (), (n, binder, inner) :: others))
  | PTuple qs ->
      let ts = Option.get (tuple_tys env ty) in
      let items = List.mapi (fun i (t, r) -> (i, t, r)) (List.combine ts qs) in
      if borrow then
        List.fold_right
          (fun (i, t, r) acc -> pextract env pos true t (fun () -> mkx pos (Field (mkp (), string_of_int i))) r acc)
          items k
      else
        let names = List.map (fun (_, _, (r : pat)) -> match r.p with PBind x -> x | _ -> fresh_pvar ()) items in
        let body =
          List.fold_right2
            (fun (_, t, (r : pat)) nm acc ->
              match r.p with PBind _ -> acc | _ -> pextract env pos false t (fun () -> mkx pos (Var nm)) r acc)
            items names k
        in
        blockx
          [ { s = Destruct (Ast.tuple_name (List.length qs), List.mapi (fun i x -> (string_of_int i, x)) names, mkp ()); spos = pos } ]
          body

let rec simple_place (x : expr) = match x.e with Var _ -> true | Field (a, _) | Deref a -> simple_place a | _ -> false

let rec copy_place (x : expr) =
  mkx x.pos (match x.e with Field (a, f) -> Field (copy_place a, f) | Deref a -> Deref (copy_place a) | d -> d)

