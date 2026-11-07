(* 投影锁定与调用排他性（对应 Lean M3（二）、M4（一）–（四））。在 own.ml 展开析构之后运行。
   - 活跃性：后向数据流，算出每个程序点之后还会用到哪些投影（M4（二）、M4（四）：
     投影的锁在最后一次使用之后结束，不必等到作用域结束）。
   - 锁定：投影 q 指向局部 z 的内部。q 之后还会用到时，z 不能被移动、修改、
     以 inout 传入或析构；只读使用照常允许（M4（一））。调用时 q 本身作为实参，
     同一次调用也不能修改 z（M4（三·下））。
   - 排他性：同一次调用里，同一个局部出现两次时，两次都必须是只读的（M3（二））。 *)
open Mir

let err (at : Ast.pos) fmt = Printf.ksprintf (fun m -> raise (Typeck.Error (at, m))) fmt

let is_proj (f : func) l = f.roots.(l) <> l

let succs (b : block) =
  match b.term with Goto j -> [ j ] | Branch (_, a, c) -> [ a; c ] | Return | Unterminated -> []

let operands = function
  | Use o | Un (_, o) | BoxNew (_, o) | Consume (_, o) | Alloc (_, _, o) -> [ o ]
  | Bin (_, a, b) -> [ a; b ]
  | Call (_, os) -> os
  | Agg (_, fs) -> List.map snd fs
  | MkEnum (_, _, o) -> Option.to_list o
  | Payload (o, _) | TagIs (o, _) -> [ o ]

let op_place = function Copy p | Move p | Ref p | RefMut p -> Some p | Const _ -> None

(* 语句用到的局部（只关心投影）。 *)
let uses (f : func) s =
  let ps =
    match s with
    | Assign (p, rv) -> p :: List.filter_map op_place (operands rv)
    | Eval rv -> List.filter_map op_place (operands rv)
    | Proj (_, p) -> [ p ]
    | Drop _ | DropNow _ | DropIf _ | Live _ | Enter _ | Leave _ | Raise -> []
  in
  List.sort_uniq compare (List.filter (is_proj f) (List.map (fun (p : place) -> p.base) ps))

let defs = function Proj (q, _) -> [ q ] | _ -> []

let term_uses (f : func) (b : block) =
  match b.term with
  | Branch (o, _, _) -> (
      match op_place o with Some p when is_proj f p.base -> [ p.base ] | _ -> [])
  | _ -> []

let union a b = List.sort_uniq compare (a @ b)

let back f s live = union (List.filter (fun q -> not (List.mem q (defs s))) live) (uses f s)

(* 每个块出口处活跃的投影，迭代到不动点。 *)
let liveness (f : func) =
  let nb = Array.length f.blocks in
  let out = Array.make nb [] in
  let live_in i =
    let b = f.blocks.(i) in
    List.fold_right (back f) b.stmts (union out.(i) (term_uses f b))
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
  | DropNow l | DropIf (l, _) -> [ (l, "析构", Mir.nopos) ]
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

let check (f : func) =
  let out = liveness f in
  Array.iteri
    (fun i (b : block) ->
      (* 从块尾往前，得到每条语句之后活跃的投影 *)
      let after =
        List.fold_right
          (fun s (live, acc) -> (back f s live, live :: acc))
          b.stmts
          (union out.(i) (term_uses f b), [])
        |> snd
      in
      List.iter2
        (fun s live ->
          exclusive f s;
          (* 调用期间作为实参的投影也算活着 *)
          let live =
            match s with
            | Assign (_, Call _) | Eval (Call _) -> union live (uses f s)
            | _ -> live
          in
          List.iter
            (fun (z, how, at) ->
              List.iter
                (fun q ->
                  if f.roots.(q) = z && q <> z then
                    err at "%s 正被投影 %s 借用（之后还会用到 %s），不能%s" (name f z) (name f q)
                      (name f q) how)
                live)
            (writes s))
        b.stmts after)
    f.blocks;
  f
