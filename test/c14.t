C14：部分移动（Lean M13）。var x = p.f、把 p.f 按 sink 传入、放进结构体字面量等，都可以移走变量 p 的一个字段；
p 的其他字段照常可以用、可以再移走。所有权检查按字段记状态和 drop flag：整体使用、整体移动 p 要求每个字段都在；
作用域结束、重新赋值之前只析构还在的字段。let a = p.f 仍然是投影（不移动，见 c2.t）。
Box 计入泄漏检查，程序正常退出就说明没有泄漏，也没有重复释放。

移走一个字段后用其他字段；只在一个分支里移走，之后重新赋值；sink 参数的字段；两个字段换位置：

  $ cat > ok.sn <<'EOF'
  > struct P { a: Box<I64>, b: Box<I64>, n: I64 }
  > 
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > 
  > fn mk(k: I64) -> P { P { a: box(k), b: box(k + 1), n: k } }
  > 
  > fn split(sink p: P) -> I64 {
  >     var x = p.a
  >     *x + *p.b
  > }
  > 
  > fn main() {
  >     let p = mk(1)
  >     var a = p.a
  >     print_i64(*a + *p.b + p.n)
  >     print_i64(take(p.b))
  >     print_i64(p.n)
  >     var q = mk(10)
  >     if q.n > 5 {
  >         print_i64(take(q.a))
  >     }
  >     print_i64(*q.b)
  >     q = mk(20)
  >     print_i64(*q.a + *q.b)
  >     let r = mk(30)
  >     let t = P { a: r.b, b: r.a, n: 0 }
  >     print_i64(*t.a)
  >     print_i64(split(mk(5)))
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  4
  2
  1
  10
  11
  41
  31
  11

移走一个字段后整体使用：

  $ cat > whole.sn <<'EOF'
  > struct P { a: Box<I64>, b: Box<I64> }
  > fn show(p: P) -> I64 { *p.b }
  > fn main() {
  >     let p = P { a: box(1), b: box(2) }
  >     var a = p.a
  >     print_i64(show(p))
  > }
  > EOF
  $ sinterc whole.sn
  whole.sn:6:20: 错误：p.a 已被移走，不能整体使用 p
   6 |     print_i64(show(p))
     |                    ^
  [1]

同一个字段移走两次：

  $ cat > twice.sn <<'EOF'
  > struct P { a: Box<I64>, b: Box<I64> }
  > fn main() {
  >     let p = P { a: box(1), b: box(2) }
  >     var a = p.a
  >     var b = p.a
  > }
  > EOF
  $ sinterc twice.sn
  twice.sn:5:13: 错误：p.a 已被移走，不能再使用
   5 |     var b = p.a
     |             ^
  [1]

只在一个分支里移走，之后使用这个字段：

  $ cat > maybe.sn <<'EOF'
  > struct P { a: Box<I64>, b: Box<I64> }
  > fn main() {
  >     let p = P { a: box(1), b: box(2) }
  >     let c = true
  >     if c { var a = p.a }
  >     print_i64(*p.a)
  > }
  > EOF
  $ sinterc maybe.sn
  maybe.sn:6:15: 错误：p.a 可能已被移走（某条路径上或上一轮循环里），不能再使用
   6 |     print_i64(*p.a)
     |               ^
  [1]

let 参数不拥有值，不能移走它的字段：

  $ cat > param.sn <<'EOF'
  > struct P { a: Box<I64>, b: Box<I64> }
  > fn f(p: P) -> I64 {
  >     var a = p.a
  >     *a
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc param.sn
  param.sn:3:13: 错误：p 不拥有值（是 let / inout 参数或投影），不能移走它的字段
   3 |     var a = p.a
     |             ^
  [1]

linear 结构体不能部分移动：

  $ cat > lin.sn <<'EOF'
  > linear struct T { a: Box<I64> }
  > fn main() {
  >     let t = T { a: box(1) }
  >     var a = t.a
  > }
  > EOF
  $ sinterc lin.sn
  lin.sn:4:13: 错误：linear 结构体 T 不能部分移动（要用 consume）
   4 |     var a = t.a
     |             ^
  [1]
