(* 所有权检查（对应 Lean M2 / M7（五））和析构展开。
   每个局部变量在每个程序点处于三种状态之一：已初始化、未初始化、可能已初始化（不同路径结果不同）。
   前向数据流，分支汇合取并，循环迭代到不动点。之后：
   - 读取未初始化或可能已初始化的变量报错（移动后使用）；
   - 降级插入的 Drop：已初始化 → 直接析构；未初始化 → 去掉；可能已初始化 → 按 drop flag 析构；
   - 给需要析构的变量重新赋值前，同样先析构旧值。 *)
open Mir

type state = Uninit | Init | Maybe

let join a b = if a = b then a else Maybe

let err (p : place) fmt = Printf.ksprintf (fun m -> raise (Typeck.Error (p.at, m))) fmt

let succs (b : block) =
  match b.term with Goto j -> [ j ] | Branch (_, a, c) -> [ a; c ] | Return | Unterminated -> []

let operands = function
  | Use o | Un (_, o) | BoxNew (_, o) -> [ o ]
  | Bin (_, a, b) -> [ a; b ]
  | Call (_, os) -> os
  | Agg (_, fs) -> List.map snd fs

let moved rv = List.filter_map (function Move p -> Some p.base | _ -> None) (operands rv)

let owned (f : func) l = Typeck.needs_drop f.locals.(l).lty

let name (f : func) l = if f.locals.(l).lname = "" then "临时值" else f.locals.(l).lname

(* 读一个操作数。check 为真时检查它已初始化；移动之后变成未初始化。 *)
let read check f st = function
  | Const _ -> ()
  | (Copy p | Move p) as o ->
      (if check then
         match st.(p.base) with
         | Init -> ()
         | Uninit -> err p "%s 已被移动，不能再使用" (name f p.base)
         | Maybe -> err p "%s 可能已被移动（某条路径上或上一轮循环里），不能再使用" (name f p.base));
      match o with Move _ -> st.(p.base) <- Uninit | _ -> ()

(* 走一条语句，返回展开后的语句。flag.(l) 是 l 的 drop flag（如果有）；
   遇到“可能已初始化”的析构点时在 need 里记下，第二遍给这些变量分配 flag。 *)
let step check f flag need st s =
  let set l v =
    match flag.(l) with Some fl -> [ Assign (whole fl, Use (Const (CBool v))) ] | None -> []
  in
  let release l =
    match st.(l) with
    | Init -> [ DropNow l ]
    | Uninit -> []
    | Maybe -> (
        need.(l) <- true;
        match flag.(l) with Some fl -> [ DropIf (l, fl) ] | None -> [])
  in
  let reads rv =
    List.iter (read check f st) (operands rv);
    List.concat_map (fun l -> set l false) (moved rv)
  in
  match s with
  | Assign (p, rv) ->
      let clear = reads rv in
      if p.proj = [] && owned f p.base then begin
        (* 右值只读临时变量或别的局部，先析构旧值再赋值是安全的 *)
        let pre = release p.base in
        st.(p.base) <- Init;
        clear @ pre @ [ s ] @ set p.base true
      end
      else begin
        if p.proj = [] then st.(p.base) <- Init;
        clear @ [ s ]
      end
  | Eval rv ->
      let clear = reads rv in
      clear @ [ s ]
  | Drop l ->
      let r = release l in
      st.(l) <- Uninit;
      r @ set l false
  | DropNow l | DropIf (l, _) ->
      st.(l) <- Uninit;
      [ s ]

(* 不动点：每个块入口的状态。走不到的块是 None。 *)
let analyze (f : func) =
  let n = Array.length f.locals and nb = Array.length f.blocks in
  let none = Array.make n None and need = Array.make n false in
  let inn = Array.make nb None in
  let entry = Array.make n Uninit in
  List.iter (fun l -> entry.(l) <- Init) f.params;
  inn.(0) <- Some entry;
  let work = Queue.create () in
  Queue.add 0 work;
  while not (Queue.is_empty work) do
    let i = Queue.pop work in
    let st = Array.copy (Option.get inn.(i)) in
    List.iter (fun s -> ignore (step false f none need st s)) f.blocks.(i).stmts;
    List.iter
      (fun j ->
        let nw = match inn.(j) with None -> Array.copy st | Some old -> Array.map2 join old st in
        if inn.(j) <> Some nw then begin
          inn.(j) <- Some nw;
          Queue.add j work
        end)
      (succs f.blocks.(i))
  done;
  inn

(* 对每个可达块从入口状态走一遍。 *)
let pass check f flag need inn =
  Array.mapi
    (fun i (b : block) ->
      match inn.(i) with
      | None -> []
      | Some s ->
          let st = Array.copy s in
          let out = List.concat_map (step check f flag need st) b.stmts in
          (match b.term with Branch (o, _, _) -> read check f st o | _ -> ());
          out)
    f.blocks

let elaborate (f : func) : func =
  let inn = analyze f in
  let n = Array.length f.locals in
  let need = Array.make n false in
  (* 第一遍：报错，并找出需要 drop flag 的变量 *)
  ignore (pass true f (Array.make n None) need inn);
  let extra = ref [] in
  let flag =
    Array.init n (fun l ->
        if need.(l) then begin
          extra := { lname = f.locals.(l).lname ^ "_flag"; lty = Ast.TBool } :: !extra;
          Some (n + List.length !extra - 1)
        end
        else None)
  in
  (* 第二遍：展开 *)
  let stmts = pass false f flag (Array.make n false) inn in
  (* 函数入口给 flag 赋初值：参数一开始拥有，其余没有 *)
  let init =
    List.concat
      (List.init n (fun l ->
           match flag.(l) with
           | Some fl -> [ Assign (whole fl, Use (Const (CBool (List.mem l f.params)))) ]
           | None -> []))
  in
  let blocks =
    Array.mapi
      (fun i (b : block) -> { b with stmts = (if i = 0 then init @ stmts.(i) else stmts.(i)) })
      f.blocks
  in
  { f with locals = Array.append f.locals (Array.of_list (List.rev !extra)); blocks }
