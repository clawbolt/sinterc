C34：模块。一个文件一个模块：use geom 引入同目录的 geom.sn，之后用 geom.area(…)、geom.Point 访问它标了 pub 的条目；
use geom { area, Point } 另外把这几个名字直接引入。没标 pub 的只能在本文件里用；trait 的方法名跟着 trait 走，不加前缀。
consume 只能在定义这个 linear 类型的文件里用，别的模块只能把值交给它的 pub 函数。
错误报在出错的那个文件里。只改名字的解析，不改检查规则，不需要新的 Lean 里程碑。

  $ cat > geom.sn <<'EOF'
  > pub struct Point { x: I64, y: I64 }
  > 
  > pub trait Show {
  >     fn show(x: Self) -> I64
  > }
  > 
  > impl Show for Point {
  >     fn show(x: Point) -> I64 { x.x * 10 + x.y }
  > }
  > 
  > pub fn area(w: I64, h: I64) -> I64 { scale(w * h) }
  > 
  > fn scale(n: I64) -> I64 { n }
  > 
  > fn abs(n: I64) -> I64 { if n < 0 { 0 - n } else { n } }
  > 
  > pub fn norm1(p: Point) -> I64 { abs(p.x) + abs(p.y) }
  > 
  > pub linear struct Tok { id: I64 }
  > 
  > pub fn open(id: I64) -> Tok { Tok { id: id } }
  > 
  > pub fn close(sink t: Tok) -> I64 {
  >     let n = t.id
  >     consume(t)
  >     n
  > }
  > EOF

函数、结构体、trait 和 linear 类型跨模块使用；本文件里的 area 和模块里的同名也不冲突：

  $ cat > ok.sn <<'EOF'
  > use geom { Point }
  > 
  > fn area(n: I64) -> I64 { n * 100 }
  > 
  > fn twice<T: geom.Show>(a: T) -> I64 { a.show() + a.show() }
  > 
  > fn main() {
  >     print_i64(geom.area(3, 4))
  >     print_i64(area(2))
  >     let p = Point { x: 0 - 3, y: 4 }
  >     print_i64(geom.norm1(p))
  >     let q = geom.Point { x: 1, y: 2 }
  >     print_i64(q.show())
  >     print_i64(twice(q))
  >     let t = geom.open(7)
  >     print_i64(geom.close(t))
  > }
  > EOF
  $ sinterc run ok.sn
  12
  200
  7
  12
  24
  7

没标 pub 的不能在模块外用：

  $ cat > private.sn <<'EOF'
  > use geom
  > fn main() {
  >     print_i64(geom.scale(1))
  > }
  > EOF
  $ sinterc private.sn
  private.sn:3:20: 错误：geom.scale 不是 pub 的，不能在模块 geom 之外使用
   3 |     print_i64(geom.scale(1))
     |                    ^
  [1]

linear 值只能在定义它的模块里 consume：

  $ cat > consume.sn <<'EOF'
  > use geom
  > fn main() {
  >     let t = geom.open(1)
  >     consume(t)
  > }
  > EOF
  $ sinterc consume.sn
  consume.sn:4:5: 错误：geom.Tok 定义在模块 geom 里，只能在那里 consume（可以让模块 geom 提供一个 pub 的函数来消费它）
   4 |     consume(t)
     |     ^
  [1]

没有 use 就用：

  $ cat > nouse.sn <<'EOF'
  > fn main() {
  >     let p = geom.Point { x: 1, y: 2 }
  > }
  > EOF
  $ sinterc nouse.sn
  nouse.sn:2:13: 错误：没有引入模块 geom（在文件开头写 use geom）
   2 |     let p = geom.Point { x: 1, y: 2 }
     |             ^
  [1]

找不到模块：

  $ cat > missing.sn <<'EOF'
  > use shapes
  > fn main() {
  > }
  > EOF
  $ sinterc missing.sn
  missing.sn:1:1: 错误：找不到模块 shapes（应当是文件 shapes.sn）
   1 | use shapes
     | ^
  [1]

模块里的错误报在模块的文件里：

  $ cat > broken.sn <<'EOF'
  > pub fn f() -> I64 {
  >     true
  > }
  > EOF
  $ cat > usebroken.sn <<'EOF'
  > use broken
  > fn main() {
  >     print_i64(broken.f())
  > }
  > EOF
  $ sinterc usebroken.sn
  broken.sn:2:5: 错误：类型不匹配：需要 I64，实际是 Bool
   2 |     true
     |     ^
  [1]

变量不能和引入的模块同名：

  $ cat > shadow.sn <<'EOF'
  > use geom
  > fn main() {
  >     let geom = 1
  > }
  > EOF
  $ sinterc shadow.sn
  shadow.sn:3:5: 错误：变量 geom 和引入的模块同名，换一个名字
   3 |     let geom = 1
     |     ^
  [1]
