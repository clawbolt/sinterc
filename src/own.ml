(* 所有权检查（对应 Lean M2 / M7（五））和析构展开。
   每个局部变量在每个程序点处于三种状态之一：已初始化、未初始化、可能已初始化（不同路径结果不同）。
   前向数据流，分支汇合取并，循环迭代到不动点。之后：
   - 读取未初始化或可能已初始化的变量报错（移动后使用）；
   - 降级插入的 Drop：已初始化 → 直接析构；未初始化 → 去掉；可能已初始化 → 按 drop flag 析构；
   - 给需要析构的变量重新赋值前，同样先析构旧值。
   linear 值（M6、M7（三））不能被隐式析构：Drop 处如果还拥有它就报错，
   这包括作用域结束、return 和出错返回三种出口；也不能在还拥有时被重新赋值。
   部分移动（Lean M13）：移走过字段（Move p.f）的结构体局部 p，它需要析构的每个字段各占一个槽，
   单独记状态和 drop flag；p 自己的状态表示结构体整体还在不在。移走一个字段只改这个字段的槽；
   整体读取、移动要求每个字段都在；析构、重新赋值前只析构还在的字段。 *)
open Mir

type state = Uninit | Init | Maybe

let join a b = if a = b then a else Maybe

let err (p : place) fmt = Printf.ksprintf (fun m -> raise (Typeck.Error (p.at, m))) fmt

let err_at at fmt = Printf.ksprintf (fun m -> raise (Typeck.Error (at, m))) fmt

let succs (b : block) =
  match b.term with Goto j -> [ j ] | Branch (_, a, c) -> [ a; c ] | Return | Unterminated -> []

let operands = function
  | Use o | Un (_, o) | BoxNew (_, o) | Unbox (_, o) | Consume (_, o) | Alloc (_, _, o) -> [ o ]
  | Bin (_, a, b) -> [ a; b ]
  | Call (_, os) -> os
  | Agg (_, fs) -> List.map snd fs
  | MkEnum (_, _, o) -> Option.to_list o
  | Payload (o, _) | TagIs (o, _) -> [ o ]

(* 这个局部自己拥有一个需要析构的值（不是 let / inout 参数或投影这样的指针）。 *)
let owned (f : func) l = Typeck.needs_drop f.locals.(l).lty && not f.locals.(l).lref

let linear (f : func) l = Typeck.is_linear f.locals.(l).lty

let name (f : func) l = if f.locals.(l).lname = "" then "临时值" else f.locals.(l).lname

let why_text = function
  | End -> "作用域结束时"
  | Ret -> "从这里返回时"
  | Err -> "出错从这里返回时"

(* 字段的槽：sp.(l) 是局部 l 需要析构的字段和它们的槽号（槽号接在局部后面），只有移走过字段的局部才有；
   槽的总数（含局部）。 *)
let field_slots (f : func) =
  let n = Array.length f.locals in
  let sp = Array.make n [] and next = ref n in
  let add l =
    if sp.(l) = [] then
      match f.locals.(l).lty with
      | Ast.TStruct s when Hashtbl.mem Typeck.struct_tbl s ->
          sp.(l) <-
            List.filter_map
              (fun (g : Ast.field) ->
                if Typeck.needs_drop g.fty then begin
                  incr next;
                  Some (g.fname, !next - 1)
                end
                else None)
              (Hashtbl.find Typeck.struct_tbl s)
      | _ -> ()
  in
  Array.iter
    (fun (b : block) ->
      List.iter
        (function
          | Assign (_, rv) | Eval rv ->
              List.iter
                (function Move { base; proj = PField _ :: _; _ } -> add base | _ -> ())
                (operands rv)
          | _ -> ())
        b.stmts)
    f.blocks;
  (sp, !next)

(* 读一个操作数用到的字段槽：整体用到所有槽，用到一个字段时只有这个字段的槽。 *)
let slots_of sp (p : place) =
  match p.proj with
  | [] -> sp.(p.base)
  | PField g :: _ -> List.filter (fun (h, _) -> h = g) sp.(p.base)
  | _ -> []

(* 一个操作数被移走的槽（给 drop flag 用）。 *)
let moved sp rv =
  List.concat_map
    (function
      | Move p -> (
          match p.proj with
          | [] -> p.base :: List.map snd sp.(p.base)
          | PField _ :: _ -> List.map snd (slots_of sp p)
          | _ -> [ p.base ])
      | _ -> [])
    (operands rv)

(* 读一个操作数。check 为真时检查它已初始化；移动之后变成未初始化。 *)
let read check f sp st = function
  | Const _ -> ()
  | (Copy p | Move p | Ref p | RefMut p) as o -> (
      if check then begin
        (match st.(p.base) with
        | Init -> ()
        | Uninit -> err p "%s 已被移动，不能再使用" (name f p.base)
        | Maybe -> err p "%s 可能已被移动（某条路径上或上一轮循环里），不能再使用" (name f p.base));
        List.iter
          (fun (g, k) ->
            let what =
              if p.proj = [] then Printf.sprintf "，不能整体使用 %s" (name f p.base) else "，不能再使用"
            in
            match st.(k) with
            | Init -> ()
            | Uninit -> err p "%s.%s 已被移走%s" (name f p.base) g what
            | Maybe -> err p "%s.%s 可能已被移走（某条路径上或上一轮循环里）%s" (name f p.base) g what)
          (slots_of sp p)
      end;
      match o, p.proj with
      | Move _, [] ->
          st.(p.base) <- Uninit;
          List.iter (fun (_, k) -> st.(k) <- Uninit) sp.(p.base)
      | Move _, PField _ :: _ -> List.iter (fun (_, k) -> st.(k) <- Uninit) (slots_of sp p)
      | Move _, _ -> st.(p.base) <- Uninit
      | _ -> ())

(* 走一条语句，返回展开后的语句。flag.(i) 是局部或字段槽 i 的 drop flag（如果有）；
   遇到“可能已初始化”的析构点时在 need 里记下，第二遍给它们分配 flag。 *)
let step check f sp flag need st s =
  let set i v =
    match flag.(i) with Some fl -> [ Assign (whole fl, Use (Const (CBool v))) ] | None -> []
  in
  (* 局部 l 连同它的字段槽一起变成 v *)
  let set_all l v =
    let s = if v then Init else Uninit in
    st.(l) <- s;
    List.iter (fun (_, k) -> st.(k) <- s) sp.(l);
    set l v @ List.concat_map (fun (_, k) -> set k v) sp.(l)
  in
  let release l =
    match sp.(l) with
    | [] -> (
        match st.(l) with
        | Init -> [ DropNow l ]
        | Uninit -> []
        | Maybe -> (
            need.(l) <- true;
            match flag.(l) with Some fl -> [ DropIf (l, fl) ] | None -> []))
    | fs when List.for_all (fun (_, k) -> st.(k) = Init) fs -> [ DropNow l ]
    | fs ->
        (* 部分移动过：只析构还在的字段（字段都在时结构体本身一定也在） *)
        List.concat_map
          (fun (g, k) ->
            match st.(k) with
            | Init -> [ DropField (l, g) ]
            | Uninit -> []
            | Maybe -> (
                need.(k) <- true;
                match flag.(k) with Some fl -> [ DropFieldIf (l, g, fl) ] | None -> []))
          fs
  in
  (* linear 值不能被隐式析构 *)
  let no_linear l at why =
    if check then
      match st.(l) with
      | Uninit -> ()
      | Init ->
          err_at at "%s 是 linear 值，%s还没有被消费%s" (name f l) (why_text why)
            (if why = Err then "（可以加 errdefer）" else "")
      | Maybe -> err_at at "%s 是 linear 值，%s在某些路径上还没有被消费" (name f l) (why_text why)
  in
  let reads rv =
    List.iter (read check f sp st) (operands rv);
    List.concat_map (fun i -> set i false) (moved sp rv)
  in
  match s with
  | Assign (p, rv) when p.proj <> [] -> (
      (* 给字段赋值：结构体本身要还在。部分移动过的字段按它的槽析构旧值（M13 的 freeField + 移进这个字段）；
         其他位置的旧值一定还在，需要析构的直接析构 *)
      let clear = reads rv in
      if check then begin
        match st.(p.base) with
        | Init -> ()
        | Uninit -> err p "%s 已被移动，不能给它的字段赋值" (name f p.base)
        | Maybe -> err p "%s 可能已被移动（某条路径上或上一轮循环里），不能给它的字段赋值" (name f p.base)
      end;
      match p.proj, slots_of sp p with
      | [ PField g ], [ (_, k) ] ->
          let pre =
            match st.(k) with
            | Init -> [ DropField (p.base, g) ]
            | Uninit -> []
            | Maybe -> (
                need.(k) <- true;
                match flag.(k) with Some fl -> [ DropFieldIf (p.base, g, fl) ] | None -> [])
          in
          st.(k) <- Init;
          clear @ pre @ [ s ] @ set k true
      | _, slots ->
          if check then
            List.iter
              (fun (g, k) ->
                if st.(k) <> Init then err p "%s.%s 已被移走或可能已被移走，不能给它里面的位置赋值" (name f p.base) g)
              slots;
          let pre = if Typeck.needs_drop (place_ty f p) then [ DropAt p ] else [] in
          clear @ pre @ [ s ])
  | Assign (p, rv) ->
      let clear = reads rv in
      if p.proj = [] && owned f p.base && linear f p.base then begin
        if check && st.(p.base) <> Uninit then
          err p "%s 是 linear 值，重新赋值前必须先消费旧值" (name f p.base);
        st.(p.base) <- Init;
        clear @ [ s ]
      end
      else if p.proj = [] && owned f p.base then begin
        (* 右值只读临时变量或别的局部，先析构旧值再赋值是安全的 *)
        let pre = release p.base in
        let flags = set_all p.base true in
        clear @ pre @ [ s ] @ flags
      end
      else if p.proj = [] && Typeck.needs_drop f.locals.(p.base).lty then begin
        (* inout 参数：一直拥有值（不能移走），赋值前直接析构旧值 *)
        if check && linear f p.base then
          err p "不能给 linear 的 inout 参数 %s 整体赋值（旧值没有被消费）" (name f p.base);
        clear @ [ DropNow p.base; s ]
      end
      else begin
        if p.proj = [] then st.(p.base) <- Init;
        clear @ [ s ]
      end
  | Eval rv ->
      let clear = reads rv in
      clear @ [ s ]
  | Drop (l, at, why) when linear f l ->
      no_linear l at why;
      st.(l) <- Uninit;
      []
  | Drop (l, _, _) ->
      let r = release l in
      r @ set_all l false
  | Raise -> [ s ]
  | Enter l ->
      st.(l) <- Init;
      [ s ]
  | Leave l ->
      st.(l) <- Uninit;
      [ s ]
  | Live (l, at, msg) ->
      if check && st.(l) <> Init then raise (Typeck.Error (at, msg));
      []
  | Proj (q, p) ->
      read check f sp st (Copy p);
      st.(q) <- Init;
      [ s ]
  | DropNow l | DropIf (l, _) ->
      st.(l) <- Uninit;
      [ s ]
  | DropField _ | DropFieldIf _ | DropAt _ -> [ s ]

(* 不动点：每个块入口的状态。走不到的块是 None。 *)
let analyze (f : func) sp total =
  let nb = Array.length f.blocks in
  let none = Array.make total None and need = Array.make total false in
  let inn = Array.make nb None in
  let entry = Array.make total Uninit in
  List.iter
    (fun l ->
      entry.(l) <- Init;
      List.iter (fun (_, k) -> entry.(k) <- Init) sp.(l))
    f.params;
  inn.(0) <- Some entry;
  let work = Queue.create () in
  Queue.add 0 work;
  while not (Queue.is_empty work) do
    let i = Queue.pop work in
    let st = Array.copy (Option.get inn.(i)) in
    List.iter (fun s -> ignore (step false f sp none need st s)) f.blocks.(i).stmts;
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
let pass check f sp flag need inn =
  Array.mapi
    (fun i (b : block) ->
      match inn.(i) with
      | None -> []
      | Some s ->
          let st = Array.copy s in
          let out = List.concat_map (step check f sp flag need st) b.stmts in
          (match b.term with Branch (o, _, _) -> read check f sp st o | _ -> ());
          out)
    f.blocks

(* check 为假时不报错（--unchecked：把编译器会拒绝的程序也导出给 Lean，看 Lean 是否独立地拒绝）。 *)
let elaborate ?(check = true) (f : func) : func =
  let n = Array.length f.locals in
  let sp, total = field_slots f in
  let inn = analyze f sp total in
  (* 槽属于哪个局部、叫什么 *)
  let owner = Array.init total (fun i -> i) and label = Array.init total (fun i -> if i < n then f.locals.(i).lname else "") in
  Array.iteri
    (fun l fs ->
      List.iter
        (fun (g, k) ->
          owner.(k) <- l;
          label.(k) <- f.locals.(l).lname ^ "_" ^ g)
        fs)
    sp;
  let need = Array.make total false in
  (* 第一遍：报错，并找出需要 drop flag 的变量和字段 *)
  ignore (pass check f sp (Array.make total None) need inn);
  let extra = ref [] in
  let flag =
    Array.init total (fun i ->
        if need.(i) then begin
          extra := { lname = label.(i) ^ "_flag"; lty = Ast.TBool; lref = false; lpos = nopos } :: !extra;
          Some (n + List.length !extra - 1)
        end
        else None)
  in
  (* 第二遍：展开 *)
  let stmts = pass false f sp flag (Array.make total false) inn in
  (* 函数入口给 flag 赋初值：参数（连同它的字段）一开始拥有，其余没有 *)
  let init =
    List.concat
      (List.init total (fun i ->
           match flag.(i) with
           | Some fl -> [ Assign (whole fl, Use (Const (CBool (List.mem owner.(i) f.params)))) ]
           | None -> []))
  in
  let blocks =
    Array.mapi
      (fun i (b : block) -> { b with stmts = (if i = 0 then init @ stmts.(i) else stmts.(i)) })
      f.blocks
  in
  let extra = Array.of_list (List.rev !extra) in
  {
    f with
    locals = Array.append f.locals extra;
    blocks;
    roots = Array.append f.roots (Array.init (Array.length extra) (fun i -> n + i));
  }
