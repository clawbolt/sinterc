(* --emit=core：把检查、展开之后的 MIR 翻译成 Lean M7（五·下）的 CFG（Core.M7h），
   交给 Lean 用已证明健全的 cfgOk 复核（附录 F 的 C5，翻译验证）。注解由 Lean 自己推断（M7（六）的 acceptsF），
   编译器的推断结果不参与判断。

   跟踪三类局部，各自重新编号成 Lean 的变量 0 … N-1：
   - 所有权（M2b）：自己拥有、需要析构、不是 linear 的局部。读 → use，移走 → free 或调用里的 sink，
     析构 → free，得到新值 → alloc，整个移进另一个局部 → move。调用整体导出成带约定的 call（M3）。
     按 drop flag 析构（DropIf）导出成 nop：状态保持“可能拥有”，由模型在出口按 drop flag 释放；
     之后如果要重新赋值，Lean 会保守地拒绝。
   - 线性值（M6）：自己拥有的 linear 局部。得到新值 → lnew，移走 → consume，整个移进另一个局部 → lmove。
   - 区域引用（M5（二））：引用类型的局部。在它的区域进入后 decl，分配 → ralloc，复制 → copy，读 → read。
     区域参数在函数入口按顺序 enter（Lean 里有了顺序，比编译器的 D3 宽），每个出口前 leave。
   投影不导出锁表：通过投影读取导出成对源对象的 use。Result 里的引用不跟踪。返回值局部不跟踪（值交给了调用者）。 *)
open Ast
open Mir

type kind = Own | Lin | Rf of int   (* Rf k：引用，区域深度 k *)

let op_base s = Printf.sprintf ".cmd (.base (.own (.c (.base (%s)))))" s
let op_call args =
  Printf.sprintf ".cmd (.base (.own (.call [%s] [])))"
    (String.concat ", " (List.map (fun (m, x) -> Printf.sprintf "⟨.%s, %d⟩" m x) args))
let op_lin s = Printf.sprintf ".cmd (.base (.lin (%s)))" s
let op_reg s = Printf.sprintf ".cmd (.reg (%s))" s

let reachable (f : func) =
  let seen = Array.make (Array.length f.blocks) false in
  let rec go i =
    if not seen.(i) then begin
      seen.(i) <- true;
      match f.blocks.(i).term with
      | Goto j -> go j
      | Branch (_, a, b) -> go a; go b
      | Return | Unterminated -> ()
    end
  in
  go 0;
  seen

let operands = function
  | Use o | Un (_, o) | BoxNew (_, o) | Consume (_, o) | Alloc (_, _, o) -> [ o ]
  | Bin (_, a, b) -> [ a; b ]
  | Call (_, os) -> os
  | Agg (_, fs) -> List.map snd fs
  | MkEnum (_, _, o) -> Option.to_list o
  | Payload (o, _) | TagIs (o, _) -> [ o ]

(* 一个函数导出成 (变量个数, 节点列表)；节点是 (操作, 后继, 是否出错出口)。 *)
let func (f : func) =
  let n = Array.length f.locals in
  (* 区域参数：参数类型里出现的区域名，和类型检查的顺序相同 *)
  let pregions =
    List.sort_uniq compare (List.concat_map (fun l -> Typeck.region_names f.locals.(l).lty) f.params)
  in
  let np = List.length pregions in
  let depth_of_ref l =
    match List.assoc_opt l f.refarena with
    | Some a -> np + List.assoc a f.arenas
    | None -> (
        match f.locals.(l).lty with
        | TRef (_, r) -> (
            match List.find_index (fun x -> x = r) pregions with Some i -> i | None -> 0)
        | _ -> 0)
  in
  let kinds =
    Array.init n (fun l ->
        let li = f.locals.(l) in
        if Some l = f.ret_local then None
        else
          match li.lty with
          | TRef _ -> Some (Rf (depth_of_ref l))
          | _ when li.lref -> None
          | t when Typeck.is_linear t -> Some Lin
          | t when Typeck.needs_drop t -> Some Own
          | _ -> None)
  in
  let idx = Array.make n (-1) and next = ref 0 in
  Array.iteri (fun l k -> if k <> None then begin idx.(l) <- !next; incr next end) kinds;
  (* 通过投影访问时，算作访问它的源对象 *)
  let base l = f.roots.(l) in
  let kind l = kinds.(base l) and ix l = idx.(base l) in
  (* 读一个位置 *)
  let touch (p : place) =
    match kind p.base with
    | Some Own -> [ op_base (Printf.sprintf ".use %d" (ix p.base)) ]
    | Some (Rf _) -> [ op_reg (Printf.sprintf ".read %d" (ix p.base)) ]
    | Some Lin | None -> []
  in
  (* 移走一个局部（交给调用、结构体、Box、Result 等） *)
  let give (p : place) =
    match kind p.base with
    | Some Own -> [ op_base (Printf.sprintf ".free %d" (ix p.base)) ]
    | Some Lin -> [ op_lin (Printf.sprintf ".consume %d" (ix p.base)) ]
    | Some (Rf _) | None -> []
  in
  let operand = function
    | Const _ -> []
    | Copy p | Ref p | RefMut p -> touch p
    | Move p -> give p
  in
  let call os =
    let args =
      List.filter_map
        (fun o ->
          match o with
          | Move p when kind p.base = Some Own -> Some ("sink", ix p.base)
          | (Copy p | Ref p) when kind p.base = Some Own -> Some ("read", ix p.base)
          | RefMut p when kind p.base = Some Own -> Some ("inout", ix p.base)
          | _ -> None)
        os
    in
    let others =
      List.concat_map
        (fun o ->
          match o with
          | (Move p | Copy p | Ref p | RefMut p) when kind p.base = Some Own -> []
          | o -> operand o)
        os
    in
    (if args = [] then [] else [ op_call args ]) @ others
  in
  let rvalue = function Call (_, os) -> call os | rv -> List.concat_map operand (operands rv) in
  let whole_target (p : place) = p.proj = [] && not f.locals.(p.base).lref in
  let stmt = function
    | Assign (p, Use (Move q)) when whole_target p && kind p.base = Some Own && kind q.base = Some Own ->
        [ op_base (Printf.sprintf ".move %d %d" (ix q.base) (ix p.base)) ]
    | Assign (p, Use (Move q)) when whole_target p && kind p.base = Some Lin && kind q.base = Some Lin ->
        [ op_lin (Printf.sprintf ".lmove %d %d" (ix q.base) (ix p.base)) ]
    | Assign (p, Use (Copy q))
      when whole_target p && q.proj = []
           && (match kind p.base, kind q.base with Some (Rf _), Some (Rf _) -> true | _ -> false) ->
        [ op_reg (Printf.sprintf ".copy %d %d" (ix q.base) (ix p.base)) ]
    | Assign (p, rv) ->
        let pre = rvalue rv in
        let target =
          if not (whole_target p) then []
          else
            match kinds.(p.base) with
            | Some Own -> [ op_base (Printf.sprintf ".alloc %d" idx.(p.base)) ]
            | Some Lin -> [ op_lin (Printf.sprintf ".lnew %d" idx.(p.base)) ]
            | Some (Rf k) -> [ op_reg (Printf.sprintf ".ralloc %d %d" idx.(p.base) k) ]
            | None -> []
        in
        pre @ target
    | Eval rv -> rvalue rv
    | Proj (_, p) -> touch p
    | DropNow l -> (
        match kinds.(l) with Some Own -> [ op_base (Printf.sprintf ".free %d" idx.(l)) ] | _ -> [])
    | Enter a ->
        let k = np + List.assoc a f.arenas in
        ".enter"
        :: List.filter_map
             (fun (l, a') ->
               if a' = a && kinds.(l) <> None then
                 Some (op_reg (Printf.sprintf ".decl %d %d" idx.(l) k))
               else None)
             f.refarena
    | Leave _ -> [ ".leave" ]
    | DropIf _ | Live _ | Raise | Drop _ -> []
  in
  (* 入口：区域参数依次 enter；参数里的引用 decl 并视为已赋值；
     拥有值的参数一开始就拥有；以区域参数为区域的其他引用局部也在这里 decl *)
  let prologue =
    List.map (fun _ -> ".enter") pregions
    @ List.concat
        (List.init n (fun l ->
             match kinds.(l) with
             | Some (Rf k) when not (List.mem_assoc l f.refarena) ->
                 op_reg (Printf.sprintf ".decl %d %d" idx.(l) k)
                 :: (if List.mem l f.params then [ op_reg (Printf.sprintf ".ralloc %d %d" idx.(l) k) ]
                     else [])
             | Some Own when List.mem l f.params -> [ op_base (Printf.sprintf ".alloc %d" idx.(l)) ]
             | Some Lin when List.mem l f.params -> [ op_lin (Printf.sprintf ".lnew %d" idx.(l)) ]
             | _ -> []))
  in
  let seen = reachable f in
  (* 每个可达块：语句的操作，加一个结尾节点（跳转、分支，或出口前离开区域参数再结束） *)
  let body (b : block) =
    let ops = List.concat_map stmt b.stmts in
    match b.term with
    | Goto j -> (ops, `Goto [ j ])
    | Branch (o, a, c) -> (ops @ operand o, `Goto [ a; c ])
    | Return | Unterminated ->
        (ops @ List.map (fun _ -> ".leave") pregions, `Exit (List.mem Raise b.stmts))
  in
  let blocks = Array.mapi (fun i b -> if seen.(i) then Some (body b) else None) f.blocks in
  (* 编号：入口代码在前，然后各块依次排开；每块的节点数是操作数加 1 *)
  let start = Array.make (Array.length f.blocks) 0 in
  let pos = ref (List.length prologue) in
  Array.iteri
    (fun i b ->
      match b with
      | Some (ops, _) ->
          start.(i) <- !pos;
          pos := !pos + List.length ops + 1
      | None -> ())
    blocks;
  let nodes = ref [] in
  let add op succ raise = nodes := (op, succ, raise) :: !nodes in
  List.iteri (fun k op -> add op [ (if k + 1 = List.length prologue then start.(0) else k + 1) ] false) prologue;
  Array.iteri
    (fun i b ->
      match b with
      | Some (ops, last) ->
          List.iteri (fun k op -> add op [ start.(i) + k + 1 ] false) ops;
          (match last with
          | `Goto js -> add ".nop" (List.map (fun j -> start.(j)) js) false
          | `Exit raise -> add ".nop" [] raise)
      | None -> ())
    blocks;
  (max 1 !next, List.rev !nodes)

(* 生成一个 Lean 文件：每个函数一个 CFG，最后用 Core.C5.report 复核。
   expect_ok：编译器正常检查过时，要求全部接受；--unchecked 时，要求 Lean 至少拒绝一个函数。 *)
let program ~expect_ok (fs : Mir.func list) =
  let b = Buffer.create 4096 in
  let add = Buffer.add_string b in
  add "-- 由 sinterc --emit=core 生成，不要手工修改。\n";
  add "import Core.C5\nset_option maxRecDepth 100000\nopen Core.M7h\n\n";
  List.iteri
    (fun i (f : Mir.func) ->
      let _, nodes = func f in
      add (Printf.sprintf "/-- %s -/\ndef f%d : CFG := [\n" f.name i);
      add
        (String.concat ",\n"
           (List.map
              (fun (op, succ, raise) ->
                Printf.sprintf "  ⟨%s, [%s], %b⟩" op
                  (String.concat ", " (List.map string_of_int succ))
                  raise)
              nodes));
      add "]\n\n")
    fs;
  add "#eval Core.C5.report [";
  add
    (String.concat ", "
       (List.mapi
          (fun i (f : Mir.func) -> Printf.sprintf "(\"%s\", %d, f%d)" f.name (fst (func f)) i)
          fs));
  add (Printf.sprintf "] %b\n" expect_ok);
  Buffer.contents b
