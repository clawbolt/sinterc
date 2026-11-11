从 Box 里移出值（Lean `Datatypes.Recursive` 的 unbox）与拆开结构体。
unbox(b) 按 sink 交出 Box，释放堆内存，得到里面的值；let Node { v, next } = n 把每个字段移进各自的变量，结构体本身被移走。
这样链表可以按移动一个节点一个节点地拆开；Box 计入泄漏检查，程序正常退出就说明没有泄漏。

去掉头一个节点（pop）、按移动反转（rev）：

  $ cat > ok.sn <<'EOF'
  > struct Node { v: I64, next: Box<List> }
  > enum List { Cons(Node), Nil }
  > 
  > fn push(sink l: List, v: I64) -> List {
  >     .Cons(Node { v: v, next: box(l) })
  > }
  > 
  > fn sum(l: List) -> I64 {
  >     match l {
  >         .Cons(n) => n.v + sum(*n.next),
  >         .Nil => 0
  >     }
  > }
  > 
  > fn pop(sink l: List) -> List {
  >     match l {
  >         .Cons(n) => {
  >             let Node { v, next } = n
  >             print_i64(v)
  >             unbox(next)
  >         }
  >         .Nil => .Nil
  >     }
  > }
  > 
  > fn rev(sink l: List, sink acc: List) -> List {
  >     match l {
  >         .Cons(n) => {
  >             let Node { v: x, next: rest } = n
  >             rev(unbox(rest), push(acc, x))
  >         }
  >         .Nil => acc
  >     }
  > }
  > 
  > fn main() {
  >     let l = push(push(push(.Nil, 1), 2), 3)
  >     let r = rev(l, .Nil)
  >     print_i64(sum(r))
  >     let t = pop(r)
  >     print_i64(sum(t))
  >     let b = box(box(7))
  >     let c = unbox(b)
  >     print_i64(*c)
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  6
  1
  5
  7

拆开之后结构体已经被移走：

  $ cat > after.sn <<'EOF'
  > struct P { a: Box<I64>, b: I64 }
  > fn main() {
  >     let p = P { a: box(1), b: 2 }
  >     let P { a, b } = p
  >     print_i64(p.b)
  > }
  > EOF
  $ sinterc after.sn
  after.sn:5:15: 错误：p 已被移动，不能再使用
   5 |     print_i64(p.b)
     |               ^
  [1]

linear 结构体不能拆开（只能 consume）：

  $ cat > lin.sn <<'EOF'
  > linear struct Tok { id: I64 }
  > fn main() {
  >     let t = Tok { id: 1 }
  >     let Tok { id } = t
  > }
  > EOF
  $ sinterc lin.sn
  lin.sn:4:5: 错误：linear 结构体 Tok 不能拆开（要用 consume）
   4 |     let Tok { id } = t
     |     ^
  [1]

unbox 之后 Box 已经被交出：

  $ cat > unboxed.sn <<'EOF'
  > fn main() {
  >     let b = box(5)
  >     let x = unbox(b)
  >     print_i64(*b)
  > }
  > EOF
  $ sinterc unboxed.sn
  unboxed.sn:4:15: 错误：b 已被移动，不能再使用
   4 |     print_i64(*b)
     |               ^
  [1]
