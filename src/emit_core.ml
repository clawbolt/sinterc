(* --emit=core：把检查、展开之后的 MIR 翻译成 Lean `FlowGraph.Region` 的 CFG（Core.FlowGraph.Region），
   交给 Lean 用已证明健全的 cfgOk 复核（附录 F 的 `Checking.Validate`，翻译验证）。注解由 Lean 自己推断（`FlowGraph.Lower` 的 acceptsF），
   编译器的推断结果不参与判断。

   跟踪三类局部，各自重新编号成 Lean 的变量 0 … N-1：
   - 所有权（`Ownership.Branch`）：自己拥有、需要析构、不是 linear 的局部。读 → use，移走 → free 或调用里的 sink，
     析构 → free，得到新值 → alloc，整个移进另一个局部 → move。调用整体导出成带约定的 call（`Calls.Modes`）。
     按 drop flag 析构（DropIf、DropFieldIf）导出成 `Projection.CallArg` 的 dropIf：flag 为真才释放，之后状态是“没有”，
     所以只在某些路径上移走之后还可以重新赋值。
     部分移动过的结构体每个需要析构的字段各是一个所有权变量（Lean `Ownership.Partial` 的位置编码）：
     整体的读、移走、析构、得到新值对每个字段各做一次，移走、放回一个字段只动这个字段。
   - 线性值（`Linearity.Linear`）：自己拥有的 linear 局部。得到新值 → lnew，移走 → consume，整个移进另一个局部 → lmove。
   - 区域引用（`Regions.Variance`）：引用类型的局部。在它的区域进入后 decl，分配 → ralloc，复制 → copy，读 → read。
     区域参数在函数入口按顺序 enter（Lean 里有了顺序，比编译器的 D3 宽），每个出口前 leave。
     含引用的结构体、枚举（Option<&I64 in a>、Pair<…>）：每个引用的位置各是一个引用变量
     （Lean `Regions.Data` 的“值的类型里的区域都算上”）。整体复制、取载荷、造结构体和变体时按位置 copy；
     来源不知道的位置（调用的结果、没有载荷的变体等）当作在这个位置的类型的区域里 ralloc，只会更严。
   投影不导出锁表：通过投影读取导出成对源对象的 use。Box、Vec、Result 里的引用不跟踪。返回值局部不跟踪（值交给了调用者）。
   借自变量的引用（返回借用、&p：区域名是变量的名字）也不跟踪，和投影一样由 borrow.ml 检查（Lean `Projection.Return`）。 *)
open Ast
open Mir

type kind = Own | Lin | Rf of int | Data of (proj list * int) list
(* Rf k：引用，区域深度 k；Data：含引用的结构体、枚举，每个引用的位置和它的区域深度 *)

let op_base s = Printf.sprintf ".cmd (.base (.own (.c (.base (%s)))))" s
let op_drop x = Printf.sprintf ".cmd (.base (.own (.dropIf %d)))" x
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
      | Return | Halt | Unterminated -> ()
    end
  in
  go 0;
  seen

let operands = function
  | Use o | Un (_, o) | BoxNew (_, o) | Unbox (_, o) | Consume (_, o) | Alloc (_, _, o) -> [ o ]
  | Bin (_, a, b) -> [ a; b ]
  | Call (_, os) -> os
  | Agg (_, fs) -> List.map snd fs
  | MkEnum (_, _, o) -> Option.to_list o
  | Payload (o, _) | TagIs (o, _) -> [ o ]

(* 类型里的引用的位置（字段、变体载荷组成的路径）和它的区域名。经过 Box、Vec、Result 的不算（不跟踪）。 *)
let rec ref_leaves ty =
  let under pre ls = List.map (fun (p, r) -> (pre :: p, r)) ls in
  match ty with
  | TRef (_, r) -> [ ([], r) ]
  | TStruct n -> (
      match Hashtbl.find_opt Typeck.struct_tbl n, Typeck.variants ty with
      | Some fs, _ -> List.concat_map (fun (fd : field) -> under (PField fd.fname) (ref_leaves fd.fty)) fs
      | None, Some vs ->
          List.concat
            (List.mapi (fun i (_, t) -> match t with Some t -> under (PVar i) (ref_leaves t) | None -> []) vs)
      | None, None -> [])
  | _ -> []

(* pre 是 path 的前缀时，返回剩下的部分。 *)
let rec strip pre path =
  match pre, path with
  | [], rest -> Some rest
  | a :: pre, b :: path when a = b -> strip pre path
  | _ -> None

(* 一个函数导出成 (变量个数, 节点列表)；节点是 (操作, 后继, 是否出错出口)。 *)
let func (f : func) =
  let n = Array.length f.locals in
  (* 区域参数：参数类型里出现的区域名，和类型检查的顺序相同 *)
  let pregions =
    List.sort_uniq compare (List.concat_map (fun l -> Typeck.region_names f.locals.(l).lty) f.params)
  in
  let np = List.length pregions in
  (* 局部 l 的类型里的区域名 r 的深度：arena 块排在区域参数之后。
     既不是区域参数、局部创建时也不在 r 的块里：r 已经结束，值是从块里带出来的（违反 R3）。
     这时给一个比任何时候的深度都大的值，Lean 的 decl 规则（区域必须比变量先存在）一定拒绝 *)
  let ended = np + List.length f.arenas in
  let depth l r =
    match List.assoc_opt l f.regarena with
    | Some m when List.mem_assoc r m -> np + List.assoc (List.assoc r m) f.arenas
    | _ -> ( match List.find_index (fun x -> x = r) pregions with Some i -> i | None -> ended)
  in
  (* 借自变量的引用（返回借用、&p，区域名是变量的名字或临时值）：和投影一样不导出锁表，不作为区域引用跟踪；
     它们由 borrow.ml 检查（Lean `Projection.Return`：就是投影） *)
  let borrowed r =
    r = "临时值"
    || Array.exists (fun (li : local_info) -> li.lname = r && match li.lty with TArena _ -> false | _ -> true) f.locals
  in
  let kinds =
    Array.init n (fun l ->
        let li = f.locals.(l) in
        let leaves = List.filter (fun (_, r) -> not (borrowed r)) (ref_leaves li.lty) in
        if Some l = f.ret_local then None
        else
          match li.lty with
          | TRef (_, r) when borrowed r -> None
          | TRef (_, r) -> Some (Rf (depth l r))
          | t
            when ((not li.lref) || List.mem l f.params)
                 && (not (Typeck.needs_drop t))
                 && (not (Typeck.is_linear t))
                 && leaves <> [] ->
              Some (Data (List.map (fun (p, r) -> (p, depth l r)) leaves))
          | _ when li.lref -> None
          | t when Typeck.is_linear t -> Some Lin
          | t when Typeck.needs_drop t -> Some Own
          | _ -> None)
  in
  (* 部分移动：移走过字段的结构体局部不占一个 Lean 变量，而是它需要析构的每个字段各占一个
     （Lean `Ownership.Partial` 的位置编码：整体操作对每个字段各做一次）。槽和 own.ml 的相同。
     含引用的结构体、枚举：每个引用的位置各占一个引用变量，区域是这个位置的类型里的区域 *)
  let sp, _ = Own.field_slots f in
  let idx = Array.make n (-1) and fidx = Hashtbl.create 8 and didx = Hashtbl.create 8 and next = ref 0 in
  Array.iteri
    (fun l k ->
      match k, sp.(l) with
      | Some Own, (_ :: _ as fs) ->
          List.iter
            (fun (g, _) ->
              Hashtbl.replace fidx (l, g) !next;
              incr next)
            fs
      | Some (Data ls), _ ->
          List.iter
            (fun (p, _) ->
              Hashtbl.replace didx (l, p) !next;
              incr next)
            ls
      | Some _, _ ->
          idx.(l) <- !next;
          incr next
      | None, _ -> ())
    kinds;
  (* 通过投影访问时，算作访问它的源对象 *)
  let base l = f.roots.(l) in
  let kind l = kinds.(base l) and ix l = idx.(base l) in
  let src = Hashtbl.create 8 in
  Array.iter
    (fun (b : block) -> List.iter (function Proj (q, p) -> Hashtbl.replace src q p | _ -> ()) b.stmts)
    f.blocks;
  (* 一个位置涉及的所有权变量：投影换成它指向的位置；部分移动过的结构体整体是所有字段，
     一个字段（及里面的位置）是这个字段，不需要析构的字段没有 *)
  let rec owners (p : place) =
    match Hashtbl.find_opt src p.base with
    | Some (s : place) -> owners { s with proj = s.proj @ p.proj }
    | None -> (
        match kinds.(p.base), sp.(p.base) with
        | Some Own, [] -> [ idx.(p.base) ]
        | Some Own, _ -> List.map (fun (g, _) -> Hashtbl.find fidx (p.base, g)) (Own.slots_of sp p)
        | _ -> [])
  in
  let own_ops cmd p = List.map (fun i -> op_base (Printf.sprintf ".%s %d" cmd i)) (owners p) in
  let drop_ops p = List.map op_drop (owners p) in
  (* 引用变量：引用局部本身，或含引用的结构体、枚举里路径 path 上的引用 *)
  let leaf_var b path =
    match kinds.(b) with
    | Some (Rf _) when path = [] -> Some idx.(b)
    | Some (Data _) -> Hashtbl.find_opt didx (b, path)
    | _ -> None
  in
  (* 一个位置涉及的引用变量（读它时都要读）：这个位置下面的，或者它所在的那个 *)
  let reg_vars (p : place) =
    match kinds.(p.base) with
    | Some (Rf _) -> [ idx.(p.base) ]
    | Some (Data ls) when base p.base = p.base ->
        List.filter_map
          (fun (path, _) ->
            if strip p.proj path <> None || strip path p.proj <> None then
              Some (Hashtbl.find didx (p.base, path))
            else None)
          ls
    | _ -> []
  in
  let reg_decl x k = op_reg (Printf.sprintf ".decl %d %d" x k) in
  let reg_alloc x k = op_reg (Printf.sprintf ".ralloc %d %d" x k) in
  (* 局部 l 的引用变量和它们的区域深度 *)
  let reg_slots l =
    match kinds.(l) with
    | Some (Rf k) -> [ (idx.(l), k) ]
    | Some (Data ls) -> List.map (fun (p, k) -> (Hashtbl.find didx (l, p), k)) ls
    | _ -> []
  in
  (* 读一个位置 *)
  let touch (p : place) =
    match kind p.base with
    | Some Own -> own_ops "use" p
    | Some (Rf _ | Data _) -> List.map (fun x -> op_reg (Printf.sprintf ".read %d" x)) (reg_vars p)
    | Some Lin | None -> []
  in
  (* 移走一个局部或字段（交给调用、结构体、Box、Result 等） *)
  let give (p : place) =
    match kind p.base with
    | Some Own -> own_ops "free" p
    | Some Lin -> [ op_lin (Printf.sprintf ".consume %d" (ix p.base)) ]
    | Some (Rf _ | Data _) | None -> []
  in
  let operand = function
    | Const _ -> []
    | Copy p | Ref p | RefMut p -> touch p
    | Move p -> give p
  in
  let call os =
    let args =
      List.concat_map
        (fun o ->
          let as_ m p = List.map (fun i -> (m, i)) (owners p) in
          match o with
          | Move p when kind p.base = Some Own -> as_ "sink" p
          | (Copy p | Ref p) when kind p.base = Some Own -> as_ "read" p
          | RefMut p when kind p.base = Some Own -> as_ "inout" p
          | _ -> [])
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
  let split l = sp.(l) <> [] in
  let field_place l path = { (whole l) with proj = List.map (fun g -> PField g) path } in
  let stmt = function
    | Assign (p, Use (Move q))
      when whole_target p && kind p.base = Some Own && kind q.base = Some Own && q.proj = []
           && (not (split p.base)) && not (split (base q.base)) ->
        [ op_base (Printf.sprintf ".move %d %d" (ix q.base) (ix p.base)) ]
    | Assign (p, Use (Move q)) when whole_target p && kind p.base = Some Lin && kind q.base = Some Lin ->
        [ op_lin (Printf.sprintf ".lmove %d %d" (ix q.base) (ix p.base)) ]
    | Assign (p, rv) when (not f.locals.(p.base).lref) && reg_slots p.base <> [] ->
        (* 引用的目标：路径 sfx 上的引用从右边对应位置的引用变量复制（copy），
           找不到时（调用的结果、取自 Box / Vec、没有载荷的变体）当作在它的类型的区域里新分配（ralloc） *)
        let from_op o sfx =
          match o with Copy q | Move q -> leaf_var q.base (q.proj @ sfx) | _ -> None
        in
        let src sfx =
          match rv, sfx with
          | Use o, _ -> from_op o sfx
          | Payload (o, i), _ -> from_op o (PVar i :: sfx)
          | Agg (_, fs), PField g :: rest -> Option.bind (List.assoc_opt g fs) (fun o -> from_op o rest)
          | MkEnum (_, i, Some o), PVar j :: rest when i = j -> from_op o rest
          | _ -> None
        in
        let targets =
          match kinds.(p.base) with
          | Some (Rf k) when p.proj = [] -> [ ([], idx.(p.base), k) ]
          | Some (Data ls) ->
              List.filter_map
                (fun (path, k) ->
                  Option.map (fun sfx -> (sfx, Hashtbl.find didx (p.base, path), k)) (strip p.proj path))
                ls
          | _ -> []
        in
        let pre =
          match rv with
          | Use (Copy q | Move q) when reg_slots q.base <> [] -> []
          | rv -> rvalue rv
        in
        pre
        @ List.concat_map
            (fun (sfx, x, k) ->
              match src sfx with
              | Some y when y = x -> []
              | Some y -> [ op_reg (Printf.sprintf ".copy %d %d" y x) ]
              | None -> [ reg_alloc x k ])
            targets
    | Assign (p, rv) ->
        let pre = rvalue rv in
        let target =
          match p.proj with
          | [] when whole_target p -> (
              match kinds.(p.base) with
              | Some Own -> own_ops "alloc" p
              | Some Lin -> [ op_lin (Printf.sprintf ".lnew %d" idx.(p.base)) ]
              | Some (Rf _ | Data _) | None -> [])
          | proj when split p.base && List.for_all (function PField _ -> true | _ -> false) proj ->
              own_ops "alloc" p   (* 给部分移动过的字段（或它上层的结构体）放回新值 *)
          | _ -> []
        in
        pre @ target
    | Eval rv -> rvalue rv
    | Proj (_, p) -> touch p
    | DropNow l -> ( match kinds.(l) with Some Own -> own_ops "free" (whole l) | _ -> [])
    | DropField (l, path) -> own_ops "free" (field_place l path)
    | Enter a ->
        ".enter"
        :: List.concat_map
             (fun (l, a') -> if a' = a then List.map (fun (x, k) -> reg_decl x k) (reg_slots l) else [])
             f.refarena
    | Leave _ -> [ ".leave" ]
    | DropAt _ -> []   (* 给没有部分移动过的字段赋值前析构旧值：模型里结构体整体仍然拥有值 *)
    | DropIf (l, _) -> ( match kinds.(l) with Some Own -> drop_ops (whole l) | _ -> [])
    | DropFieldIf (l, path, _) -> drop_ops (field_place l path)
    | Live _ | Raise | Drop _ -> []
  in
  (* 入口：区域参数依次 enter；参数里的引用 decl 并视为已赋值；
     拥有值的参数一开始就拥有；以区域参数为区域的其他引用局部也在这里 decl *)
  let prologue =
    List.map (fun _ -> ".enter") pregions
    @ List.concat
        (List.init n (fun l ->
             match kinds.(l) with
             | Some (Rf _ | Data _) when not (List.mem_assoc l f.refarena) ->
                 List.concat_map
                   (fun (x, k) -> reg_decl x k :: (if List.mem l f.params then [ reg_alloc x k ] else []))
                   (reg_slots l)
             | Some Own when List.mem l f.params -> own_ops "alloc" (whole l)
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
    | Halt -> (ops, `Halt)   (* panic：写成指向自己的空节点（Lean `FlowGraph.Panic` 的编码），不是出口 *)
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
          | `Exit raise -> add ".nop" [] raise
          | `Halt -> add ".nop" [ start.(i) + List.length ops ] false)
      | None -> ())
    blocks;
  (max 1 !next, List.rev !nodes)

(* 生成一个 Lean 文件：每个函数一个 CFG，最后用 Core.Checking.Validate.report 复核。
   expect_ok：编译器正常检查过时，要求全部接受；--unchecked 时，要求 Lean 至少拒绝一个函数。 *)
let program ~expect_ok (fs : Mir.func list) =
  let b = Buffer.create 4096 in
  let add = Buffer.add_string b in
  add "-- 由 sinterc --emit=core 生成，不要手工修改。\n";
  add "import Core.Checking.Validate\nset_option maxRecDepth 100000\nopen Core.FlowGraph.Region\n\n";
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
  add "#eval Core.Checking.Validate.report [";
  add
    (String.concat ", "
       (List.mapi
          (fun i (f : Mir.func) -> Printf.sprintf "(\"%s\", %d, f%d)" f.name (fst (func f)) i)
          fs));
  add (Printf.sprintf "] %b\n" expect_ok);
  Buffer.contents b
