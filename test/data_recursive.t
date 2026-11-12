递归类型（Lean `Datatypes.Recursive`）。经过 Box 的递归可以：链表、树；不经过 Box 包含自己的类型大小无限，在定义处拒绝。
析构按类型生成递归的析构函数，整条链都被释放；Box 计入泄漏检查，程序正常退出就说明没有泄漏。

链表（非泛型和泛型）：构造、按借用的 match 递归求和、求长度，作用域结束时整条链析构：

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
  > struct GNode<T> { v: T, next: Box<GList<T>> }
  > enum GList<T> { Cons(GNode<T>), Nil }
  > 
  > fn gpush<T>(sink l: GList<T>, sink v: T) -> GList<T> {
  >     .Cons(GNode { v: v, next: box(l) })
  > }
  > 
  > fn glen<T>(l: GList<T>) -> I64 {
  >     match l {
  >         .Cons(n) => 1 + glen(*n.next),
  >         .Nil => 0
  >     }
  > }
  > 
  > fn main() {
  >     var l: List = .Nil
  >     l = push(l, 1)
  >     l = push(l, 2)
  >     l = push(l, 3)
  >     println("{sum(l)}")
  >     let g = gpush(gpush(.Nil, box(10)), box(20))
  >     println("{glen(g)}")
  >     let h = gpush(gpush(gpush(.Nil, 1), 2), 3)
  >     println("{glen(h)}")
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  6
  2
  3

不经过 Box 包含自己：

  $ cat > inf.sn <<'EOF'
  > struct Node { v: I64, next: List }
  > enum List { Cons(Node), Nil }
  > fn main() {
  > }
  > EOF
  $ sinterc inf.sn
  inf.sn:1:1: 错误：类型 Node 不经过 Box 包含自己，大小是无限的（递归要经过 Box，例如 next: Box<List>）
   1 | struct Node { v: I64, next: List }
     | ^
  [1]
