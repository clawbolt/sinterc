C15：给字段赋值 a.f = e。a 是以可修改的变量（var 变量、inout / sink 参数）为根的位置，可以经过字段、Box 的解引用和 Vec 的元素，
不能经过引用。先算右边，再算位置；字段原来的值如果需要析构，赋值前先析构（部分移动后放回来的字段按它的 drop flag，Lean M13）。
linear 字段不能直接赋值；字段的投影还活着时不能赋值。Box 计入泄漏检查，程序正常退出就说明没有泄漏。

inout 参数的字段、旧值析构、移走后放回、只在一个分支里移走后放回、多层字段、Box 里的结构体：

  $ cat > ok.sn <<'EOF'
  > struct Counter { n: I64 }
  > 
  > fn bump(inout c: Counter) {
  >     c.n = c.n + 1
  > }
  > 
  > struct P { a: Box<I64>, b: Box<I64>, n: I64 }
  > struct Q { p: P, k: I64 }
  > 
  > fn sum(p: P) -> I64 { *p.a + *p.b }
  > 
  > fn main() {
  >     var c = Counter { n: 0 }
  >     bump(c)
  >     c.bump()
  >     c.n = c.n * 10
  >     print_i64(c.n)
  >     var p = P { a: box(1), b: box(2), n: 0 }
  >     p.a = box(5)
  >     print_i64(*p.a + *p.b)
  >     var x = p.b
  >     p.b = box(*x + 100)
  >     print_i64(*p.b)
  >     print_i64(sum(p))
  >     let yes = true
  >     if yes { var y = p.a }
  >     p.a = box(9)
  >     print_i64(sum(p))
  >     var q = Q { p: P { a: box(1), b: box(2), n: 3 }, k: 0 }
  >     q.p.n = 4
  >     q.p.a = box(40)
  >     print_i64(q.p.n + *q.p.a)
  >     var b = box(P { a: box(1), b: box(2), n: 0 })
  >     (*b).n = 7
  >     print_i64((*b).n)
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  20
  7
  102
  107
  111
  44
  7

let 变量不可修改：

  $ cat > letv.sn <<'EOF'
  > struct Counter { n: I64 }
  > fn main() {
  >     let c = Counter { n: 0 }
  >     c.n = 1
  > }
  > EOF
  $ sinterc letv.sn
  letv.sn:4:5: 错误：c 不可修改，不能给它的字段赋值（需要 var 变量或 inout / sink 参数）
  [1]

let 参数不可修改：

  $ cat > letp.sn <<'EOF'
  > struct Counter { n: I64 }
  > fn f(c: Counter) {
  >     c.n = 1
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc letp.sn
  letp.sn:3:5: 错误：c 不可修改，不能给它的字段赋值（需要 var 变量或 inout / sink 参数）
  [1]

结构体整个被移走之后：

  $ cat > moved.sn <<'EOF'
  > struct P { a: Box<I64>, n: I64 }
  > fn main() {
  >     var p = P { a: box(1), n: 0 }
  >     let q = p
  >     p.n = 1
  > }
  > EOF
  $ sinterc moved.sn
  moved.sn:5:5: 错误：p 已被移动，不能给它的字段赋值
  [1]

字段的投影还活着时：

  $ cat > lock.sn <<'EOF'
  > struct P { a: Box<I64>, n: I64 }
  > fn main() {
  >     var p = P { a: box(1), n: 0 }
  >     let r = p.a
  >     p.a = box(3)
  >     print_i64(*r)
  > }
  > EOF
  $ sinterc lock.sn
  lock.sn:5:5: 错误：p 正被投影 r 借用（之后还会用到 r），不能修改
  [1]
