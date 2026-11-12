for x in inout v（Lean `Datatypes.VecInout`）。x 是第 i 个元素本身（可变借用）：可以读、改字段、借给 inout 参数、整体赋值
（旧值先析构）；循环体里不能用 v，不能把 x 移走。

  $ cat > ok.sn <<'EOF'
  > struct P { x: I64, name: Str }
  > 
  > fn bump(inout n: I64) { n = n + 100 }
  > 
  > fn main() {
  >     var v: Vec<I64> = Vec.new()
  >     v.push(1)
  >     v.push(2)
  >     v.push(3)
  >     for x in inout v {
  >         x = x * 10
  >         bump(x)
  >     }
  >     for x in v {
  >         println("{x}")
  >     }
  >     var ps: Vec<P> = Vec.new()
  >     ps.push(P { x: 1, name: "a" })
  >     ps.push(P { x: 2, name: "b" })
  >     for p in inout ps {
  >         p.x = p.x + 1
  >         p.name.push("!")
  >     }
  >     var names: Vec<Str> = Vec.new()
  >     names.push("old")
  >     names.push("old")
  >     var i = 0
  >     for s in inout names {
  >         s = i.to_str()
  >         i = i + 1
  >     }
  >     for p in ps {
  >         println("{p.x}")
  >         println(p.name)
  >     }
  >     for s in names {
  >         println(s)
  >     }
  >     var m: Vec<Vec<I64>> = Vec.new()
  >     m.push(Vec.new())
  >     m.push(Vec.new())
  >     for row in inout m {
  >         row.push(7)
  >         for y in inout row {
  >             y = y + 1
  >         }
  >     }
  >     for row in m {
  >         for y in row {
  >             println("{y}")
  >         }
  >     }
  > }
  > EOF
  $ sinterc run ok.sn
  110
  120
  130
  2
  a!
  3
  b!
  0
  1
  8
  8

循环体里不能用 v：

  $ cat > push.sn <<'EOF'
  > fn main() {
  >     var v: Vec<I64> = Vec.new()
  >     v.push(1)
  >     for x in inout v {
  >         v.push(x)
  >     }
  > }
  > EOF
  $ sinterc push.sn
  push.sn:5:9: 错误：for x in inout v 的循环体里不能用 v（v 整个借给了循环，不能 push、pop、读或再借）
   5 |         v.push(x)
     |         ^
  [1]

不能把 x 移走：

  $ cat > move.sn <<'EOF'
  > fn take(sink s: Str) { println(s) }
  > 
  > fn main() {
  >     var v: Vec<Str> = Vec.new()
  >     v.push("a")
  >     for x in inout v {
  >         take(x)
  >     }
  > }
  > EOF
  $ sinterc move.sn 2>&1 | grep -c 错误
  1

let 声明的 Vec 不能改：

  $ cat > let.sn <<'EOF'
  > fn main() {
  >     let v: Vec<I64> = Vec.new()
  >     for x in inout v {
  >         x = 1
  >     }
  > }
  > EOF
  $ sinterc let.sn
  let.sn:4:9: 错误：v 不可修改，不能给它的元素赋值（需要 var 变量或 inout / sink 参数）
   4 |         x = 1
     |         ^
  [1]
