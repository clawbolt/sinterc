(* 投影锁定与调用排他性（对应 Lean `Calls.NArgs`、`Projection.Lock` –（四））。在 own.ml 展开析构之后运行。
   - 活跃性：后向数据流，算出每个程序点之后还会用到哪些投影（`Projection.Liveness`、`Projection.Dataflow`：
     投影的锁在最后一次使用之后结束，不必等到作用域结束）。
   - 锁定：投影 q 指向局部 z 的内部。q 之后还会用到时，z 不能被移动、修改、
     以 inout 传入或析构；只读使用照常允许（`Projection.Lock`）。调用时 q 本身作为实参，
     同一次调用也不能修改 z（`Projection.CallArg`）。
   - 返回借用（`Projection.Return`）：返回 &T in m 的调用，结果借自 m 对应的实参，和投影一样锁住它；
     结果复制到别的局部、放进 Option、传给返回借用的函数（借用的借用）时，借自的局部跟着走（lent）。
     函数自己返回的引用只能借自签名里写的 let 参数。
   - 排他性：同一次调用里，同一个局部出现两次时，两次都必须是只读的（`Calls.NArgs`）。 *)
open Mir

let err (at : Ast.pos) fmt = Printf.ksprintf (fun m -> raise (Typeck.Error (at, m))) fmt

let is_proj (f : func) l = f.roots.(l) <> l

let succs (b : block) =
  match b.term with Goto j -> [ j ] | Branch (_, a, c) -> [ a; c ] | Return | Halt | Unterminated -> []

let operands = function
  | Use o | Un (_, o) | BoxNew (_, o) | Unbox (_, o) | Consume (_, o) | Alloc (_, _, o) -> [ o ]
  | Bin (_, a, b) -> [ a; b ]
  | Call (_, os) -> os
  | Agg (_, fs) -> List.map snd fs
  | MkEnum (_, _, o) -> Option.to_list o
  | Payload (o, _) | TagIs (o, _) -> [ o ]

let op_place = function Copy p | Move p | Ref p | RefMut p -> Some p | Const _ -> None

let union a b = List.sort_uniq compare (a @ b)

(* 每个局部借自哪些局部（只对类型里有引用的局部；都是拥有值的局部，不是投影）。
   返回借用的调用的结果借自对应的实参；值从一个局部流到另一个类型里有引用的局部时，借自的局部也跟着流过去。
   不区分程序点，取所有赋值的并集，迭代到不动点：多锁一些只会多拒绝，不会放过。 *)
let lent (f : func) =
  let n = Array.length f.locals in
  let b = Array.make n [] in
  let has_ref l = Typeck.region_names f.locals.(l).lty <> [] in
  let owner l = f.roots.(l) in
  let of_place (p : place) = union b.(p.base) b.(owner p.base) in
  let changed = ref true in
  let add l xs =
    let v = union b.(l) xs in
    if v <> b.(l) then begin
      b.(l) <- v;
      changed := true
    end
  in
  let srcs rv =
    let xs = List.concat_map (fun o -> match op_place o with Some p -> of_place p | None -> []) (operands rv) in
    match rv with
    | Call (g, os) -> (
        match Hashtbl.find_opt Typeck.let_regions g with
        | Some l ->
            union xs
              (List.filter_map
                 (fun (_, i) -> Option.bind (List.nth_opt os i) (fun o -> Option.map (fun (p : place) -> owner p.base) (op_place o)))
                 l)
        | None -> xs)
    (* &p：借自 p 所在的局部 *)
    | Use (Ref p) -> union xs [ owner p.base ]
    | _ -> xs
  in
  let flow dst rv =
    let xs = srcs rv in
    (match dst with Some l when has_ref l -> add l xs | _ -> ());
    (* inout 实参的类型里有引用时，被调函数可能把别的实参借来的引用存进去 *)
    match rv with
    | Call (_, os) ->
        List.iter
          (function
            | RefMut q when has_ref q.base ->
                add q.base xs;
                add (owner q.base) xs
            | _ -> ())
          os
    | _ -> ()
  in
  while !changed do
    changed := false;
    Array.iter
      (fun (blk : block) ->
        List.iter
          (function
            | Assign (p, rv) -> flow (Some p.base) rv
            | Eval rv -> flow None rv
            | Proj (q, p) -> add q (of_place p)
            | _ -> ())
          blk.stmts)
      f.blocks
  done;
  b

(* 要跟踪活跃性的局部：投影，以及借自别的局部的引用 *)
let tracked (f : func) bs l = is_proj f l || bs.(l) <> []

(* 活着的 q 锁住哪些局部 *)
let locks (f : func) bs q = union (if is_proj f q then [ f.roots.(q) ] else []) bs.(q)

(* 语句用到的局部（只关心要跟踪的）。 *)
let uses (f : func) bs s =
  let ps =
    match s with
    (* 整个换掉一个引用（不是投影）不算用到它：之前的值从这里起不再用到（见 defs） *)
    | Assign (p, rv) when p.proj = [] && not (is_proj f p.base) -> List.filter_map op_place (operands rv)
    | Assign (p, rv) -> p :: List.filter_map op_place (operands rv)
    | Eval rv -> List.filter_map op_place (operands rv)
    | Proj (_, p) -> [ p ]
    | DropAt p -> [ p ]
    | Drop _ | DropNow _ | DropIf _ | DropField _ | DropFieldIf _ | Live _ | Enter _ | Leave _ | Raise -> []
  in
  List.sort_uniq compare (List.filter (tracked f bs) (List.map (fun (p : place) -> p.base) ps))

(* 投影由 Proj 定义；引用由整个赋值定义 *)
let defs (f : func) = function
  | Proj (q, _) -> [ q ]
  | Assign ({ base; proj = []; _ }, _) when not (is_proj f base) -> [ base ]
  | _ -> []

let term_uses (f : func) bs (b : block) =
  match b.term with
  | Branch (o, _, _) -> (
      match op_place o with Some p when tracked f bs p.base -> [ p.base ] | _ -> [])
  | _ -> []

let back f bs s live = union (List.filter (fun q -> not (List.mem q (defs f s))) live) (uses f bs s)

(* 每个块出口处活跃的投影和引用，迭代到不动点。 *)
let liveness (f : func) bs =
  let nb = Array.length f.blocks in
  let out = Array.make nb [] in
  let live_in i =
    let b = f.blocks.(i) in
    List.fold_right (back f bs) b.stmts (union out.(i) (term_uses f bs b))
  in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = nb - 1 downto 0 do
      let o = List.fold_left (fun acc j -> union acc (live_in j)) [] (succs f.blocks.(i)) in
      if o <> out.(i) then begin
        out.(i) <- o;
        changed := true
      end
    done
  done;
  out

let name (f : func) l = if f.locals.(l).lname = "" then "临时值" else f.locals.(l).lname

(* 语句会修改的局部，以及怎么修改的、在哪里。 *)
let writes s =
  let of_ops rv =
    List.filter_map
      (function
        | Move p -> Some (p.base, "移动", p.at)
        | RefMut p -> Some (p.base, "以 inout 传入", p.at)
        | _ -> None)
      (operands rv)
  in
  match s with
  | Assign (p, rv) -> (p.base, "修改", p.at) :: of_ops rv
  | Eval rv -> of_ops rv
  | DropNow l | DropIf (l, _) | DropField (l, _) | DropFieldIf (l, _, _) -> [ (l, "析构", Mir.nopos) ]
  | DropAt p -> [ (p.base, "修改", p.at) ]   (* 只出现在给字段赋值之前 *)
  | Drop _ | Proj _ | Live _ | Enter _ | Leave _ | Raise -> []

(* 同一次调用里的实参两两检查。 *)
let exclusive (f : func) = function
  | Assign (_, Call (_, os)) | Eval (Call (_, os)) ->
      let acc =
        List.filter_map
          (fun o ->
            match o with
            | Copy p | Ref p -> Some (f.roots.(p.base), false, p.at)
            | Move p | RefMut p -> Some (f.roots.(p.base), true, p.at)
            | Const _ -> None)
          os
      in
      List.iteri
        (fun i (z, w, at) ->
          List.iteri
            (fun j (z', w', _) ->
              if j < i && z = z' && (w || w') then
                err at "%s 在同一次调用里出现两次，其中有移动或 inout，只能都是只读的" (name f z))
            acc)
        acc
  | _ -> ()

(* 函数返回的引用只能借自返回类型里写的 let 参数（`Projection.Return` 的被调函数一边） *)
let check_return (f : func) bs =
  match f.ret_local with
  | Some r ->
      let named = Typeck.region_names f.ret in
      List.iter
        (fun z ->
          if not (List.mem z f.params && List.mem f.locals.(z).lname named) then
            err f.locals.(z).lpos "返回的引用借自 %s，函数结束时它就不在了（只能借自返回类型里写的 let 参数）" (name f z))
        bs.(r)
  | None -> ()

let check (f : func) =
  let bs = lent f in
  check_return f bs;
  let out = liveness f bs in
  Array.iteri
    (fun i (b : block) ->
      (* 从块尾往前，得到每条语句之后活跃的投影和引用 *)
      let after =
        List.fold_right
          (fun s (live, acc) -> (back f bs s live, live :: acc))
          b.stmts
          (union out.(i) (term_uses f bs b), [])
        |> snd
      in
      List.iter2
        (fun s live ->
          exclusive f s;
          (* 调用期间作为实参的投影也算活着 *)
          let live =
            match s with
            | Assign (_, Call _) | Eval (Call _) -> union live (uses f bs s)
            | _ -> live
          in
          List.iter
            (fun (z, how, at) ->
              List.iter
                (fun q ->
                  if q <> z && List.mem z (locks f bs q) then
                    if is_proj f q && f.roots.(q) = z then
                      err at "%s 正被投影 %s 借用（之后还会用到 %s），不能%s" (name f z) (name f q) (name f q) how
                    else if at = Mir.nopos then
                      (* 析构没有位置：作用域结束时。报在变量声明的地方 *)
                      err f.locals.(z).lpos "%s 在作用域结束时析构，但借自它的引用之后还会用到（引用不能带出 %s 的作用域）" (name f z)
                        (name f z)
                    else err at "%s 正被引用 %s 借用（之后还会用到 %s），不能%s" (name f z) (name f q) (name f q) how)
                live)
            (writes s))
        b.stmts after)
    f.blocks;
  f
