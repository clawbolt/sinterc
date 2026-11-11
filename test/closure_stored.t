可以存起来的闭包 Fn(T) -> U（Lean `Closures.Stored`）。闭包写在需要 Fn(…) 的地方（let 标注、返回值、实参、字段、Vec 元素）时，
用到的外面的变量按值拿进来（会移动的值被移走，可复制的值复制），存在堆上的环境里；Fn 值可以存进变量、字段、Vec，也可以返回，
需要析构、只能移动。调用 f(x) 或 f.call(x) 时闭包按 inout：闭包可以改自己的环境（计数器），所以要在 var 变量或 inout 位置里。
Fn 变量也可以传给 fn(…) 参数。环境里不能有引用和区域。

  $ cat > ok.sn <<'EOF'
  > use std.vec
  > 
  > struct Button { label: Str, on_click: Fn(I64) -> I64 }
  > 
  > fn adder(k: I64) -> Fn(I64) -> I64 {
  >     |x| x + k
  > }
  > 
  > // 计数器：环境里的 n 每次调用加 1
  > fn counter() -> Fn() -> I64 {
  >     var n = 0
  >     || {
  >         n = n + 1
  >         n
  >     }
  > }
  > 
  > fn apply(inout f: Fn(I64) -> I64, x: I64) -> I64 {
  >     f(x)
  > }
  > 
  > fn compose<T>(sink f: Fn(T) -> T, sink g: Fn(T) -> T) -> Fn(T) -> T {
  >     |x| g(f(x))
  > }
  > 
  > fn main() {
  >     var add5 = adder(5)
  >     print_i64(add5(1))
  >     print_i64(apply(add5, 10))
  >     var c = counter()
  >     print_i64(c())
  >     print_i64(c())
  >     var d = counter()
  >     print_i64(d())
  >     print_i64(c())
  >     var fs: Vec<Fn(I64) -> I64> = vec_new()
  >     fs.push(adder(1))
  >     fs.push(adder(100))
  >     let m = 3
  >     fs.push(|x| x * m)
  >     for i in 0..fs.len() {
  >         print_i64(fs[i].call(7))
  >     }
  >     // 按值拿进来的 Str：移进环境，闭包析构时一起析构
  >     let name = "ann"
  >     var greet: Fn() -> () = || print(name)
  >     greet()
  >     greet()
  >     var b = Button { label: "ok", on_click: |n| n * 2 }
  >     print_i64(b.on_click.call(21))
  >     print(b.label)
  >     var v: Vec<I64> = vec_new()
  >     v.push(1)
  >     v.push(2)
  >     let w = vec.map(v, add5)
  >     for x in w {
  >         print_i64(x)
  >     }
  >     var h = compose(adder(1), adder(10))
  >     print_i64(h(5))
  >     var h2 = h
  >     print_i64(h2(0))
  > }
  > EOF
  $ sinterc run ok.sn
  6
  15
  1
  2
  1
  3
  8
  107
  21
  ann
  ann
  42
  ok
  6
  7
  16
  11

按值拿进来的变量被移走了：

  $ cat > moved.sn <<'EOF'
  > fn main() {
  >     let s = "hi"
  >     var f: Fn() -> () = || print(s)
  >     print(s)
  > }
  > EOF
  $ sinterc moved.sn
  moved.sn:4:11: 错误：s 已被移动，不能再使用
   4 |     print(s)
     |           ^
  [1]

调用时闭包按 inout，let 声明的不能调用：

  $ cat > letcall.sn <<'EOF'
  > fn main() {
  >     let f: Fn(I64) -> I64 = |x| x + 1
  >     print_i64(f(1))
  > }
  > EOF
  $ sinterc letcall.sn
  letcall.sn:3:15: 错误：f 不可修改：调用 Fn(…) 闭包时闭包按 inout（它可以改自己的环境），要用 var 声明，或者作为 inout / sink 参数
   3 |     print_i64(f(1))
     |               ^
  [1]

环境里不能有引用：

  $ cat > ref.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         let r = alloc(a, 1)
  >         var f: Fn() -> I64 = || *r
  >     }
  > }
  > EOF
  $ sinterc ref.sn
  ref.sn:4:30: 错误：Fn(…) 闭包暂不能用含引用或区域的变量 r：闭包可以存起来，引用可能活不过它的区域
   4 |         var f: Fn() -> I64 = || *r
     |                              ^
  [1]

闭包所在的位置不能同时当实参（`Closures.Stored` 的 env_disjoint 的前提）：

  $ cat > overlap.sn <<'EOF'
  > struct W { g: Fn(W) -> I64 }
  > fn main() {
  >     var w = W { g: |v| 1 }
  >     print_i64(w.g.call(w))
  > }
  > EOF
  $ sinterc overlap.sn
  overlap.sn:4:24: 错误：w 在同一次调用里出现两次，其中有移动或 inout，只能都是只读的
   4 |     print_i64(w.g.call(w))
     |                        ^
  [1]

不写类型的闭包不能存：

  $ cat > untyped.sn <<'EOF'
  > fn main() {
  >     var f = |x| x + 1
  > }
  > EOF
  $ sinterc untyped.sn
  untyped.sn:2:13: 错误：无法确定闭包的类型：写在调用的实参里传给 fn(…) 参数，或者写在知道类型的地方（例如 var f: Fn(I64) -> I64 = |x| …）
   2 |     var f = |x| x + 1
     |             ^
  [1]
