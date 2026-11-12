trait（Lean `Traits.Static`）。trait 声明方法签名（可以用 Self），impl 给具体类型实现，约束写 T: Show + Eq。
泛型代码里只能调用约束里写了的 trait 的方法；调用处要求每个约束都有 impl；impl 的参数约定和类型必须和 trait 一致。
trait 的方法用方法调用的写法 a.eq(b)，按接收者的类型在约束和 impl 里找（Lean `Traits.Inherent` 的 resolve_tra），不占用全局的名字。
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
  > fn same<T: Eq>(a: T, b: T) -> Bool { a.eq(b) }
  > 
  > fn count<T: Eq + Show>(a: T, b: T, c: T) -> I64 {
  >     var n = 0
  >     if a.eq(b) { n = n + a.show() }
  >     if a.eq(c) { n = n + c.show() }
  >     n
  > }
  > 
  > fn total<T: Show>(sink x: T) -> I64 { x.show() }
  > 
  > fn main() {
  >     println("{same(3, 3)}")
  >     println(same(Point { x: 1, y: 2 }, Point { x: 1, y: 3 }).to_str())
  >     println("{count(5, 5, 6)}")
  >     println("{total(box(7))}")
  >     let four = 4
  >     println("{four.eq(4)}")
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
  > fn f<T>(x: T) -> I64 { x.show() }
  > fn main() {
  > }
  > EOF
  $ sinterc nobound.sn
  nobound.sn:4:26: 错误：T 没有写约束 Show，不能调用 show（要写 T: Show）
   4 | fn f<T>(x: T) -> I64 { x.show() }
     |                          ^
  [1]

实例类型没有 impl（调用处报错）：

  $ cat > noimpl.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > fn total<T: Show>(x: T) -> I64 { x.show() }
  > fn main() {
  >     println("{total(true)}")
  > }
  > EOF
  $ sinterc noimpl.sn
  noimpl.sn:6:15: 错误：Bool 没有实现 Show（total 要求 T: Show）
   6 |     println("{total(true)}")
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

trait 的方法不占用全局的名字：自己的函数可以和 trait 的方法同名，两个 trait 也可以有同名的方法，按接收者的类型选：

  $ cat > names.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > trait Size {
  >     fn show(x: Self) -> I64
  > }
  > impl Show for I64 {
  >     fn show(x: I64) -> I64 { x }
  > }
  > impl Size for Bool {
  >     fn show(x: Bool) -> I64 { 1 }
  > }
  > fn show(s: Str) -> I64 { s.len() }
  > fn main() {
  >     let k = 5
  >     let b = true
  >     println("{k.show()}")
  >     println("{b.show()}")
  >     println(show("abc").to_str())
  > }
  > EOF
  $ sinterc names.sn -o names.c && cc -std=gnu11 -w names.c -o names && ./names
  5
  1
  3

直接写 show(3) 不是 trait 的方法：

  $ cat > direct.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > impl Show for I64 {
  >     fn show(x: I64) -> I64 { x }
  > }
  > fn main() {
  >     println("{show(3)}")
  > }
  > EOF
  $ sinterc direct.sn
  direct.sn:8:15: 错误：show 是 trait Show 的方法，要写成方法调用 x.show(…)
   8 |     println("{show(3)}")
     |               ^
  [1]

接收者的类型两个 trait 都满足时不确定，报错：

  $ cat > ambig.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > trait Size {
  >     fn show(x: Self) -> I64
  > }
  > impl Show for I64 {
  >     fn show(x: I64) -> I64 { x }
  > }
  > impl Size for I64 {
  >     fn show(x: I64) -> I64 { 8 }
  > }
  > fn main() {
  >     let k = 5
  >     println("{k.show()}")
  > }
  > EOF
  $ sinterc ambig.sn
  ambig.sn:15:17: 错误：I64 的方法 show 不确定：trait Show 和 Size 都有这个方法，I64 两个都满足
   15 |     println("{k.show()}")
      |                 ^
  [1]
