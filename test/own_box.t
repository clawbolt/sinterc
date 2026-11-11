Box、移动、析构。程序结束时如果还有 Box 没释放，打印 leak 并以退出码 102 结束。

移动进函数、从函数返回、重新赋值时析构旧值、循环里每轮析构、含 Box 的结构体、
只在一个分支里移动（用 drop flag 决定结尾要不要析构）、移动后重新赋值：

  $ cat > ok.sn <<'EOF'
  > struct Pair { a: Box<I64>, n: I64 }
  > 
  > fn take(sink b: Box<I64>) -> I64 {
  >     *b + 1
  > }
  > 
  > fn make(x: I64) -> Box<I64> {
  >     box(x * 10)
  > }
  > 
  > fn main() {
  >     let b = box(41)
  >     print_i64(take(b))
  >     var c = make(1)
  >     c = make(2)
  >     print_i64(*c)
  >     var i = 0
  >     while i < 3 {
  >         let t = box(i)
  >         i = *t + 1
  >     }
  >     let p = Pair { a: box(7), n: 3 }
  >     print_i64(*p.a + p.n)
  >     let d = box(5)
  >     if i > 100 { print_i64(take(d)) }
  >     let e = box(6)
  >     if i < 100 { print_i64(take(e)) }
  >     var m = box(1)
  >     print_i64(take(m))
  >     m = box(2)
  >     print_i64(*m)
  >     print_i64(*make(4))
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  42
  20
  10
  7
  2
  2
  40

移动之后再使用：

  $ cat > move.sn <<'EOF'
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > 
  > fn main() {
  >     let b = box(1)
  >     print_i64(take(b))
  >     print_i64(*b)
  > }
  > EOF
  $ sinterc move.sn
  move.sn:6:15: 错误：b 已被移动，不能再使用
   6 |     print_i64(*b)
     |               ^
  [1]

只在一个分支里移动，汇合之后再使用：

  $ cat > branch.sn <<'EOF'
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > 
  > fn main() {
  >     let b = box(1)
  >     if true { print_i64(take(b)) }
  >     print_i64(*b)
  > }
  > EOF
  $ sinterc branch.sn
  branch.sn:6:15: 错误：b 可能已被移动（某条路径上或上一轮循环里），不能再使用
   6 |     print_i64(*b)
     |               ^
  [1]

在循环里移动循环外的值：

  $ cat > loop.sn <<'EOF'
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > 
  > fn main() {
  >     let b = box(1)
  >     var i = 0
  >     while i < 2 {
  >         print_i64(take(b))
  >         i = i + 1
  >     }
  > }
  > EOF
  $ sinterc loop.sn
  loop.sn:7:24: 错误：b 可能已被移动（某条路径上或上一轮循环里），不能再使用
   7 |         print_i64(take(b))
     |                        ^
  [1]

let 声明的变量不能重新赋值：

  $ cat > letmut.sn <<'EOF'
  > fn main() {
  >     let x = 1
  >     x = 2
  > }
  > EOF
  $ sinterc letmut.sn
  letmut.sn:3:5: 错误：x 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）
   3 |     x = 2
     |     ^
  [1]

不能从 Box 里直接移出值（要用 unbox；移走变量的字段见 own_partial_move.t）：

  $ cat > partial.sn <<'EOF'
  > struct P { a: Box<Box<I64>> }
  > 
  > fn main() {
  >     let p = P { a: box(box(1)) }
  >     var q = *p.a
  > }
  > EOF
  $ sinterc partial.sn
  partial.sn:5:13: 错误：不能从 Box 或 Vec 里移出值（Box 用 unbox，Vec 的元素用 vec_pop）
   5 |     var q = *p.a
     |             ^
  [1]
