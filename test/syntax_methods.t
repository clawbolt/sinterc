方法调用语法（UFCS）。e.f(a, …) 就是 f(e, a, …)：接收者是第一个实参，按 f 的第一个参数的约定传入（let、inout、sink）。
接收者是 Vec 时 push、pop、len、get 是内建的 vec_push 等，其他情况就是同名的函数（包括 trait 的方法）。
只是写法不同，之后的所有权、借用检查和直接调用一样。

普通函数、inout 参数、trait 方法（含泛型代码里）、Vec，以及和 Vec 的 push 同名的用户函数：

  $ cat > ok.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > 
  > impl Show for I64 {
  >     fn show(x: I64) -> I64 { x }
  > }
  > 
  > struct Counter { n: I64 }
  > 
  > fn bump(inout c: Counter, k: I64) {
  >     c = Counter { n: c.n + k }
  > }
  > 
  > fn get_n(c: Counter) -> I64 { c.n }
  > 
  > impl Show for Counter {
  >     fn show(x: Counter) -> I64 { x.get_n() * 10 }
  > }
  > 
  > fn twice<T: Show>(a: T) -> I64 { a.show() + a.show() }
  > 
  > struct Node { v: I64, next: Box<List> }
  > enum List { Cons(Node), Nil }
  > 
  > fn push(sink l: List, v: I64) -> List {
  >     .Cons(Node { v: v, next: box(l) })
  > }
  > 
  > fn sum(l: List) -> I64 {
  >     match l {
  >         .Cons(n) => n.v + (*n.next).sum(),
  >         .Nil => 0
  >     }
  > }
  > 
  > fn main() {
  >     var c = Counter { n: 1 }
  >     c.bump(2)
  >     c.bump(3)
  >     println("{c.get_n()}")
  >     println("{c.show()}")
  >     println("{twice(c)}")
  >     println("{4.show()}")
  >     var v: Vec<Box<I64>> = vec_new()
  >     v.push(box(1))
  >     v.push(box(2))
  >     println("{v.len()}")
  >     println("{*v.get(1)}")
  >     match v.pop() {
  >         .Some(b) => println("{*b}"),
  >         .None => println("{0}")
  >     }
  >     let l: List = .Nil
  >     let l2 = l.push(5).push(6)
  >     println("{l2.sum()}")
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  6
  60
  120
  4
  2
  2
  2
  11

inout 的接收者也必须可修改：

  $ cat > inout.sn <<'EOF'
  > struct Counter { n: I64 }
  > fn bump(inout c: Counter) {
  >     c = Counter { n: c.n + 1 }
  > }
  > fn main() {
  >     let c = Counter { n: 0 }
  >     c.bump()
  > }
  > EOF
  $ sinterc inout.sn
  inout.sn:7:5: 错误：c 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）
   7 |     c.bump()
     |     ^
  [1]

sink 的接收者被移走：

  $ cat > sink.sn <<'EOF'
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > fn main() {
  >     let b = box(1)
  >     println("{b.take()}")
  >     println("{*b}")
  > }
  > EOF
  $ sinterc sink.sn
  sink.sn:5:15: 错误：b 已被移动，不能再使用
   5 |     println("{*b}")
     |               ^
  [1]

get 的投影还活着时 push（和 vec_get / vec_push 一样）：

  $ cat > lock.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Box<I64>> = vec_new()
  >     v.push(box(1))
  >     let e = v.get(0)
  >     v.push(box(2))
  >     println("{*e}")
  > }
  > EOF
  $ sinterc lock.sn
  lock.sn:5:5: 错误：v 正被投影 e 借用（之后还会用到 e），不能以 inout 传入
   5 |     v.push(box(2))
     |     ^
  [1]

没有这个函数：

  $ cat > undef.sn <<'EOF'
  > fn main() {
  >     let n = 1
  >     println("{n.foo()}")
  > }
  > EOF
  $ sinterc undef.sn
  undef.sn:3:17: 错误：未定义的函数 foo
   3 |     println("{n.foo()}")
     |                 ^
  [1]
