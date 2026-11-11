trait 的三个扩展（Lean `Traits.ImplSearch`）。内建的 trait ToStr（字符串插值 "{x}" 用它），fn to_str(x: P) -> Str 是
impl ToStr for P 的简写，可以给多种类型各写一个；模块里的 m.to_str 也为它的类型实现 ToStr。
泛型 impl：impl<T: ToStr> ToStr for Vec<T>，查找时递归地查约束（`Traits.ImplSearch` 的 solve，instance_resolves 保证单态化查得到）。
默认方法：trait 里写了函数体的方法，impl 可以不写（它是约束 Self: trait 的泛型函数，`Traits.Static`）。

  $ cat > ok.sn <<'EOF'
  > use std.bigint
  > 
  > struct Point { x: I64, y: I64 }
  > enum Color { Red, Green }
  > struct Pair<A> { a: A, b: A }
  > 
  > fn to_str(p: Point) -> Str { "({p.x}, {p.y})" }
  > 
  > fn to_str(c: Color) -> Str {
  >     match c {
  >         .Red => "red",
  >         .Green => "green"
  >     }
  > }
  > 
  > impl<T: ToStr> ToStr for Vec<T> {
  >     fn to_str(v: Vec<T>) -> Str {
  >         var s = "["
  >         for i in 0..v.len() {
  >             if i > 0 { s.push(", ") }
  >             s.push(v[i].to_str())
  >         }
  >         s.push("]")
  >         s
  >     }
  > }
  > 
  > impl<A: ToStr> ToStr for Pair<A> {
  >     fn to_str(p: Pair<A>) -> Str { "<{p.a} | {p.b}>" }
  > }
  > 
  > impl<T: ToStr> ToStr for Option<T> {
  >     fn to_str(o: Option<T>) -> Str {
  >         match let o {
  >             .Some(x) => "Some({x})",
  >             .None => "None"
  >         }
  >     }
  > }
  > 
  > trait Shape {
  >     fn area(s: Self) -> I64
  >     fn name(s: Self) -> Str { "shape" }
  >     fn describe(s: Self) -> Str { "{s.name()} of area {s.area()}" }
  > }
  > 
  > struct Square { side: I64 }
  > struct Circle { r: I64 }
  > 
  > impl Shape for Square {
  >     fn area(s: Square) -> I64 { s.side * s.side }
  >     fn name(s: Square) -> Str { "square" }
  > }
  > 
  > impl Shape for Circle {
  >     fn area(s: Circle) -> I64 { 3 * s.r * s.r }
  > }
  > 
  > fn show<T: ToStr>(x: T) { print("<{x}>") }
  > 
  > fn report<T: Shape>(x: T) { print(x.describe()) }
  > 
  > fn main() {
  >     let p = Point { x: 1, y: 2 }
  >     let c: Color = .Green
  >     print("p = {p}, c = {c}")
  >     var v: Vec<I64> = vec_new()
  >     for i in 1..4 { v.push(i * i) }
  >     print("{v}")
  >     var ps: Vec<Point> = vec_new()
  >     ps.push(Point { x: 3, y: 4 })
  >     ps.push(p)
  >     print("{ps}")
  >     var m: Vec<Vec<I64>> = vec_new()
  >     m.push(v)
  >     let w: Vec<I64> = vec_new()
  >     m.push(w)
  >     print("{m}")
  >     show(Pair { a: 1, b: 2 })
  >     show(Pair { a: "x", b: "y" })
  >     let o: Option<Color> = .Some(.Red)
  >     show(o)
  >     let n: Option<I64> = .None
  >     show(n)
  >     show(true)
  >     print((7).to_str())
  >     let b = bigint.from_i64(-12345)
  >     print("b = {b}")
  >     let sq = Square { side: 2 }
  >     print(sq.describe())
  >     report(Circle { r: 1 })
  >     report(sq)
  > }
  > EOF
  $ sinterc run ok.sn
  p = (1, 2), c = green
  [1, 4, 9]
  [(3, 4), (1, 2)]
  [[1, 4, 9], []]
  <<1 | 2>>
  <<x | y>>
  <Some(red)>
  <None>
  <true>
  7
  b = -12345
  square of area 4
  shape of area 3
  square of area 4

泛型 impl 的约束不满足时，报最里面那个类型：

  $ cat > noimpl.sn <<'EOF'
  > struct P { x: I64 }
  > impl<T: ToStr> ToStr for Vec<T> {
  >     fn to_str(v: Vec<T>) -> Str { "[{v.len()}]" }
  > }
  > fn main() {
  >     let v: Vec<P> = vec_new()
  >     print("{v}")
  > }
  > EOF
  $ sinterc noimpl.sn
  noimpl.sn:7:12: 错误：P 没有实现 ToStr，而 Vec<P> 的 ToStr 要用到它
   7 |     print("{v}")
     |            ^
  [1]

两个 impl 有共同的实例（`Traits.ImplSearch` 的 Coherent，否则实例不唯一）：

  $ cat > overlap.sn <<'EOF'
  > impl<T: ToStr> ToStr for Vec<T> {
  >     fn to_str(v: Vec<T>) -> Str { "vec" }
  > }
  > impl ToStr for Vec<I64> {
  >     fn to_str(v: Vec<I64>) -> Str { "ints" }
  > }
  > fn main() { print("x") }
  > EOF
  $ sinterc overlap.sn
  overlap.sn:4:1: 错误：impl ToStr for Vec<I64> 和已有的 impl ToStr for Vec<T> 重叠（有的类型两个都能匹配）
   4 | impl ToStr for Vec<I64> {
     | ^
  [1]

头部不能是单独的类型参数（`Traits.ImplSearch` 的 WF，否则查找不会停）：

  $ cat > bare.sn <<'EOF'
  > impl<T> ToStr for T {
  >     fn to_str(x: T) -> Str { "?" }
  > }
  > fn main() { print("x") }
  > EOF
  $ sinterc bare.sn
  bare.sn:1:1: 错误：不能为类型参数 T 本身实现 trait（impl<T> … for T 会和所有的 impl 重叠）
   1 | impl<T> ToStr for T {
     | ^
  [1]

泛型代码里插值要写约束 T: ToStr：

  $ cat > bound.sn <<'EOF'
  > fn show<T>(x: T) {
  >     print("{x}")
  > }
  > fn main() { show(1) }
  > EOF
  $ sinterc bound.sn
  bound.sn:2:12: 错误：T 没有写约束 ToStr，不能调用 to_str（要写 T: ToStr）
   2 |     print("{x}")
     |            ^
  [1]

没有默认函数体的方法 impl 必须写：

  $ cat > missing.sn <<'EOF'
  > trait Shape {
  >     fn area(s: Self) -> I64
  >     fn name(s: Self) -> Str { "shape" }
  > }
  > struct Sq { side: I64 }
  > impl Shape for Sq {
  >     fn name(s: Sq) -> Str { "sq" }
  > }
  > fn main() { print("x") }
  > EOF
  $ sinterc missing.sn
  missing.sn:6:1: 错误：impl Shape for Sq 缺少方法 area
   6 | impl Shape for Sq {
     | ^
  [1]
