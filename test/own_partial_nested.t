多层部分移动（Lean `Ownership.Partial` 的位置编码按字段路径推广）。var x = q.p.a 移走字段的字段，var p = q.p 移走一个结构体字段；
所有权检查按“需要析构的叶子路径”（q.p.a、q.p.b、q.c）记状态和 drop flag：用到一个位置时，它下面的叶子都要在；
给一条字段路径赋值时，按它下面每个叶子的状态析构旧值再放回。Box 计入泄漏检查，程序正常退出就说明没有泄漏。

移走叶子、移走中间的结构体后放回、只在一个分支里移走叶子后放回：

  $ cat > ok.sn <<'EOF'
  > struct P { a: Box<I64>, b: Box<I64> }
  > struct Q { p: P, c: Box<I64>, n: I64 }
  > 
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > 
  > fn sum_p(p: P) -> I64 { *p.a + *p.b }
  > 
  > fn mk(k: I64) -> Q { Q { p: P { a: box(k), b: box(k + 1) }, c: box(k + 2), n: k } }
  > 
  > fn main() {
  >     let q = mk(1)
  >     var a = q.p.a
  >     println("{*a + *q.p.b + *q.c}")
  >     println("{take(q.p.b)}")
  >     println("{take(q.c)}")
  >     var r = mk(10)
  >     var p = r.p
  >     println("{sum_p(p)}")
  >     println("{*r.c}")
  >     r.p = P { a: box(100), b: box(200) }
  >     println("{sum_p(r.p)}")
  >     var s = mk(20)
  >     let yes = true
  >     if yes { println("{take(s.p.a)}") }
  >     s.p.a = box(7)
  >     println("{sum_p(s.p) + *s.c}")
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  6
  2
  3
  21
  12
  300
  20
  50

移走叶子之后整体使用它上层的结构体：

  $ cat > whole.sn <<'EOF'
  > struct P { a: Box<I64>, b: Box<I64> }
  > struct Q { p: P, c: Box<I64> }
  > fn sum_p(p: P) -> I64 { *p.a + *p.b }
  > fn main() {
  >     let q = Q { p: P { a: box(1), b: box(2) }, c: box(3) }
  >     var a = q.p.a
  >     println("{sum_p(q.p)}")
  > }
  > EOF
  $ sinterc whole.sn
  whole.sn:7:21: 错误：q.p.a 已被移走，不能整体使用 q.p
   7 |     println("{sum_p(q.p)}")
     |                     ^
  [1]

移走中间的结构体之后使用它里面的叶子：

  $ cat > inner.sn <<'EOF'
  > struct P { a: Box<I64>, b: Box<I64> }
  > struct Q { p: P, c: Box<I64> }
  > fn sum_p(p: P) -> I64 { *p.a + *p.b }
  > fn main() {
  >     let q = Q { p: P { a: box(1), b: box(2) }, c: box(3) }
  >     var p = q.p
  >     println("{*q.p.a}")
  > }
  > EOF
  $ sinterc inner.sn
  inner.sn:7:15: 错误：q.p.a 已被移走，不能再使用
   7 |     println("{*q.p.a}")
     |               ^
  [1]
