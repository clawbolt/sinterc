类型自己的方法（Lean `Traits.Inherent`）。impl 类型 { … } 给一个类型加方法，类型可以是自己的结构体，也可以是内建的
（I64、Str、Vec<T>、&str in a），还可以是具体的实例（Vec<I64> 和 Vec<F64> 各有一个 total）。第一个参数是接收者，
参数约定照常写；签名里可以用 Self。方法调用按接收者的类型查找，找不到再按原来的规则（内建的方法、trait 的方法、同名函数）。

结构体、I64、Vec 的两个实例、泛型的 Vec<T>（方法可以有 fn(…) 参数）、Str、字符串切片（区域跟着接收者走）：

  $ cat > ok.sn <<'EOF'
  > struct Point { x: I64, y: I64 }
  > 
  > impl I64 {
  >     fn abs1(x: I64) -> I64 { if x < 0 { -x } else { x } }
  > }
  > 
  > impl Point {
  >     fn norm1(p: Self) -> I64 { p.x.abs1() + p.y.abs1() }
  >     fn shift(inout p: Point, d: I64) { p = Point { x: p.x + d, y: p.y + d } }
  > }
  > 
  > impl Vec<I64> {
  >     fn total(v: Vec<I64>) -> I64 {
  >         var s = 0
  >         for x in v { s = s + x }
  >         s
  >     }
  > }
  > 
  > impl Vec<F64> {
  >     fn total(v: Vec<F64>) -> F64 {
  >         var s = 0.0
  >         for x in v { s = s + x }
  >         s
  >     }
  > }
  > 
  > impl<T> Vec<T> {
  >     fn size(v: Vec<T>) -> I64 { v.len() }
  >     fn count_if(v: Vec<T>, f: fn(T) -> Bool) -> I64 {
  >         var n = 0
  >         for x in v {
  >             if f(x) { n = n + 1 }
  >         }
  >         n
  >     }
  > }
  > 
  > impl Str {
  >     fn shout(s: Str) -> Str { "{s}!" }
  > }
  > 
  > impl &str in a {
  >     fn first(s: &str in a) -> I64 { s.byte(0) }
  >     fn tail(s: &str in a) -> &str in a { s.sub(1, s.len()) }
  > }
  > 
  > fn main() {
  >     var p = Point { x: 3, y: -4 }
  >     print_i64(p.norm1())
  >     p.shift(10)
  >     print_i64(p.norm1())
  >     var v: Vec<I64> = vec_new()
  >     v.push(1)
  >     v.push(2)
  >     v.push(3)
  >     print_i64(v.total())
  >     var w: Vec<F64> = vec_new()
  >     w.push(1.5)
  >     w.push(2.0)
  >     print_f64(w.total())
  >     print_i64(v.size())
  >     print_i64(v.count_if(|x| x > 1))
  >     let s = "hi"
  >     print(s.shout())
  >     arena |a| {
  >         let t = text(a, "xyz")
  >         print_i64(t.first())
  >         print_i64(t.tail().first())
  >     }
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  7
  19
  6
  3.5
  3
  2
  hi!
  120
  121

在定义处查一次：泛型函数里 x: T 的 x.show() 是 trait 的方法，实例里沿用，不会因为 T 是 I64 换成 impl I64 的 show；
具体类型的接收者先找类型自己的方法：

  $ cat > trait.sn <<'EOF'
  > trait Show {
  >     fn show(x: Self) -> I64
  > }
  > 
  > impl Show for I64 {
  >     fn show(x: I64) -> I64 { x }
  > }
  > 
  > impl I64 {
  >     fn show(x: I64) -> I64 { x * 100 }
  > }
  > 
  > fn via<T: Show>(x: T) -> I64 { x.show() }
  > 
  > fn main() {
  >     let k = 7
  >     print_i64(k.show())
  >     print_i64(via(k))
  > }
  > EOF
  $ sinterc trait.sn -o trait.c && cc -std=gnu11 -w trait.c -o trait && ./trait
  700
  7

泛型函数里 Vec<T> 的接收者看不到只给 Vec<I64> 写的方法（同上，在定义处查）：

  $ cat > generic.sn <<'EOF'
  > impl Vec<I64> {
  >     fn total(v: Vec<I64>) -> I64 { 0 }
  > }
  > fn f<T>(v: Vec<T>) -> I64 { v.total() }
  > fn main() {
  > }
  > EOF
  $ sinterc generic.sn
  generic.sn:4:31: 错误：未定义的函数 total
   4 | fn f<T>(v: Vec<T>) -> I64 { v.total() }
     |                               ^
  [1]

同名方法的头部重叠（`Traits.Inherent` 的 CoherentM 不成立）时，同一个调用按书写顺序会找到不同的函数，所以拒绝：

  $ cat > overlap.sn <<'EOF'
  > impl<T> Vec<T> {
  >     fn first2(v: Vec<T>) -> I64 { 0 }
  > }
  > impl Vec<I64> {
  >     fn first2(v: Vec<I64>) -> I64 { 1 }
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc overlap.sn
  overlap.sn:5:5: 错误：impl Vec<I64> 和 impl Vec<T> 都有方法 first2，头部重叠（有的类型两个都能匹配）
   5 |     fn first2(v: Vec<I64>) -> I64 { 1 }
     |     ^
  [1]

第一个参数是接收者：

  $ cat > recv.sn <<'EOF'
  > struct P { x: I64 }
  > impl P {
  >     fn get(x: I64) -> I64 { x }
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc recv.sn
  recv.sn:3:5: 错误：方法 get 的第一个参数是接收者，类型要写成 P（或 Self）
   3 |     fn get(x: I64) -> I64 { x }
     |     ^
  [1]

不能给类型参数本身写方法：

  $ cat > param.sn <<'EOF'
  > impl<T> T {
  >     fn me(x: T) -> I64 { 0 }
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc param.sn
  param.sn:2:5: 错误：不能给类型参数 T 本身写方法（impl<T> T 会和所有的头部重叠）
   2 |     fn me(x: T) -> I64 { 0 }
     |     ^
  [1]
