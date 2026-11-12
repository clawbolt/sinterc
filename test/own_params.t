参数约定（let 默认 / inout / sink）、投影、排他性。

let 参数只读借用、调用后实参还能用；inout 修改调用方的变量和字段；
给 inout 参数重新赋值会析构旧值；同一个值可以同时以 let 传两次；
let 绑定字段得到投影，投影存在时可以只读使用源对象，投影最后一次使用之后源对象可以重新赋值：

  $ cat > ok.sn <<'EOF'
  > struct P { a: Box<I64>, n: I64 }
  > 
  > fn get(b: Box<I64>) -> I64 { *b }
  > 
  > fn inc(inout x: I64) { x = x + 1 }
  > 
  > fn reset(inout b: Box<I64>, sink v: I64) { b = box(v) }
  > 
  > fn sum(p: P, q: P) -> I64 { *p.a + *q.a }
  > 
  > fn main() {
  >     var b = box(1)
  >     println("{get(b)}")
  >     reset(b, 5)
  >     println("{*b}")
  >     var n = 10
  >     inc(n)
  >     println("{n}")
  >     let p = P { a: box(2), n: 3 }
  >     println("{sum(p, p)}")
  >     let a = p.a
  >     println("{*a + p.n}")
  >     var x = P { a: box(8), n: 0 }
  >     let pa = x.a
  >     println("{*pa}")
  >     x = P { a: box(9), n: 1 }
  >     println("{*x.a}")
  >     inc(x.n)
  >     println("{x.n}")
  >     println("{get(box(3))}")
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  1
  5
  11
  4
  5
  8
  9
  2
  3

排他性：同一个变量既以 inout、又以 let 传入同一次调用（Lean `Calls.Modes` 的反例）：

  $ cat > alias.sn <<'EOF'
  > fn both(inout a: I64, b: I64) { a = a + b }
  > 
  > fn main() {
  >     var n = 1
  >     both(n, n)
  > }
  > EOF
  $ sinterc alias.sn
  alias.sn:5:13: 错误：n 在同一次调用里出现两次，其中有移动或 inout，只能都是只读的
   5 |     both(n, n)
     |             ^
  [1]

投影之后还会用到时移走源对象（Lean `Projection.Lock` 的反例）：

  $ cat > lock.sn <<'EOF'
  > struct P { a: Box<I64>, n: I64 }
  > 
  > fn eat(sink p: P) -> I64 { p.n }
  > 
  > fn main() {
  >     let p = P { a: box(1), n: 2 }
  >     let a = p.a
  >     println("{eat(p)}")
  >     println("{*a}")
  > }
  > EOF
  $ sinterc lock.sn
  lock.sn:8:19: 错误：p 正被投影 a 借用（之后还会用到 a），不能移动
   8 |     println("{eat(p)}")
     |                   ^
  [1]

同一个程序，投影最后一次使用之后再移走源对象：锁按活跃性结束，接受（Lean `Projection.Liveness`）：

  $ cat > nll.sn <<'EOF'
  > struct P { a: Box<I64>, n: I64 }
  > 
  > fn eat(sink p: P) -> I64 { p.n }
  > 
  > fn main() {
  >     let p = P { a: box(1), n: 2 }
  >     let a = p.a
  >     println("{*a}")
  >     println("{eat(p)}")
  > }
  > EOF
  $ sinterc nll.sn -o nll.c && cc -std=gnu11 -w nll.c -o nll && ./nll
  1
  2

投影还活着时把源对象的字段以 inout 传入。锁住的是整个源对象，和 Lean 模型一致，
所以即使字段不重叠也拒绝：

  $ cat > inout.sn <<'EOF'
  > struct P { a: Box<I64>, n: I64 }
  > 
  > fn inc(inout x: I64) { x = x + 1 }
  > 
  > fn main() {
  >     var p = P { a: box(1), n: 2 }
  >     let a = p.a
  >     inc(p.n)
  >     println("{*a}")
  > }
  > EOF
  $ sinterc inout.sn
  inout.sn:8:9: 错误：p 正被投影 a 借用（之后还会用到 a），不能以 inout 传入
   8 |     inc(p.n)
     |         ^
  [1]

let 参数不拥有值，不能移走：

  $ cat > keep.sn <<'EOF'
  > fn keep(b: Box<I64>) -> Box<I64> { b }
  > 
  > fn main() {
  >     let x = keep(box(1))
  > }
  > EOF
  $ sinterc keep.sn
  keep.sn:1:36: 错误：b 不拥有值（是 let / inout 参数或投影），不能移动
   1 | fn keep(b: Box<I64>) -> Box<I64> { b }
     |                                    ^
  [1]

inout 实参必须可修改：

  $ cat > immut.sn <<'EOF'
  > fn inc(inout x: I64) { x = x + 1 }
  > 
  > fn main() {
  >     let n = 1
  >     inc(n)
  > }
  > EOF
  $ sinterc immut.sn
  immut.sn:5:9: 错误：n 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）
   5 |     inc(n)
     |         ^
  [1]
