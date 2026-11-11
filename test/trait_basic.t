trait（Lean `Traits.Static`）。trait 声明方法签名（可以用 Self），impl 给具体类型实现，约束写 T: Show + Eq。
泛型代码里只能调用约束里写了的 trait 的方法；调用处要求每个约束都有 impl；impl 的参数约定和类型必须和 trait 一致。
单态化时方法调用直接换成 impl 里的函数（静态派发）。

Eq、Show 用在 I64、结构体、Box 上；impl 里可以写 Self；Box 计入泄漏检查：

  $ cat > ok.sn <<'EOF'
  > trait Eq {
  >     fn eq(a: Self, b: Self) -> Bool
  > }
  > 
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > 
  > struct Point { x: I64, y: I64 }
  > 
  > impl Eq for I64 {
  >     fn eq(a: I64, b: I64) -> Bool { a == b }
  > }
  > 
  > impl Eq for Point {
  >     fn eq(a: Self, b: Self) -> Bool { a.x == b.x && a.y == b.y }
  > }
  > 
  > impl Show for Box<I64> {
  >     fn show(x: Box<I64>) -> I64 { *x }
  > }
  > 
  > impl Show for I64 {
  >     fn show(x: I64) -> I64 { x }
  > }
  > 
  > fn same<T: Eq>(a: T, b: T) -> Bool { eq(a, b) }
  > 
  > fn count<T: Eq + Show>(a: T, b: T, c: T) -> I64 {
  >     var n = 0
  >     if eq(a, b) { n = n + show(a) }
  >     if eq(a, c) { n = n + show(c) }
  >     n
  > }
  > 
  > fn total<T: Show>(sink x: T) -> I64 { show(x) }
  > 
  > fn main() {
  >     print_bool(same(3, 3))
  >     print_bool(same(Point { x: 1, y: 2 }, Point { x: 1, y: 3 }))
  >     print_i64(count(5, 5, 6))
  >     print_i64(total(box(7)))
  >     print_bool(eq(4, 4))
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  true
  false
  5
  7
  true

没写约束就不能调用方法（定义处报错）：

  $ cat > nobound.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > fn f<T>(x: T) -> I64 { show(x) }
  > fn main() {
  > }
  > EOF
  $ sinterc nobound.sn
  nobound.sn:4:24: 错误：T 没有写约束 Show，不能调用 show（要写 T: Show）
   4 | fn f<T>(x: T) -> I64 { show(x) }
     |                        ^
  [1]

实例类型没有 impl（调用处报错）：

  $ cat > noimpl.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > fn total<T: Show>(x: T) -> I64 { show(x) }
  > fn main() {
  >     print_i64(total(true))
  > }
  > EOF
  $ sinterc noimpl.sn
  noimpl.sn:6:15: 错误：Bool 没有实现 Show（total 要求 T: Show）
   6 |     print_i64(total(true))
     |               ^
  [1]

impl 把 let 参数改成 sink（Lean `Traits.Static` 的反例：泛型代码会读到已交出的值）：

  $ cat > mode.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > impl Show for I64 {
  >     fn show(sink x: I64) -> I64 { x }
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc mode.sn
  mode.sn:5:5: 错误：impl Show for I64 的方法 show 的第 1 个参数约定是 sink，trait 里声明的是 let
   5 |     fn show(sink x: I64) -> I64 { x }
     |     ^
  [1]

同一个类型对同一个 trait 只能有一个 impl：

  $ cat > dup.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > impl Show for I64 {
  >     fn show(x: I64) -> I64 { x }
  > }
  > impl Show for I64 {
  >     fn show(x: I64) -> I64 { 0 }
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc dup.sn
  dup.sn:7:1: 错误：I64 已经实现过 Show
   7 | impl Show for I64 {
     | ^
  [1]
