下标 v[i]（就是 vec_get(v, i)）和按下标赋值 v[i] = x。和 for x in inout v 里的 x = e 一样：
先算右边，再查下标（越界时 panic）、析构旧值、放进新值（Lean `Datatypes.VecInout`（二））；v 要能修改，不能在右边被移走。
不换行的输出 print(s)、原始字节 print_byte(b)。

  $ cat > ok.sn <<'EOF'
  > use std.io
  > 
  > struct P { x: I64, name: Str }
  > fn sieve(n: I64) -> Vec<Bool> {
  >     var s: Vec<Bool> = Vec.new()
  >     for i in 0..n + 1 { s.push(true) }
  >     s[0] = false
  >     s[1] = false
  >     var i = 2
  >     while i * i <= n {
  >         if s[i] {
  >             var j = i * i
  >             while j <= n {
  >                 s[j] = false
  >                 j = j + i
  >             }
  >         }
  >         i = i + 1
  >     }
  >     s
  > }
  > fn main() {
  >     let s = sieve(30)
  >     var out = ""
  >     for i in 0..31 {
  >         if s[i] {
  >             if out.len() > 0 { out.push(" ") }
  >             out.push(i.to_str())
  >         }
  >     }
  >     println(out)
  >     var names: Vec<Str> = Vec.new()
  >     names.push("a")
  >     names.push("b")
  >     names[1] = "B"
  >     names[0] = names[0].concat("!")
  >     println(names[0])
  >     println(names[1])
  >     var ps: Vec<P> = Vec.new()
  >     ps.push(P { x: 1, name: "p" })
  >     ps[0].x = ps[0].x + 41
  >     ps[0] = P { x: ps[0].x + 1, name: "q" }
  >     println("{ps[0].x} {ps[0].name}")
  >     var m: Vec<Vec<I64>> = Vec.new()
  >     for i in 0..2 {
  >         var row: Vec<I64> = Vec.new()
  >         row.push(i)
  >         m.push(row)
  >     }
  >     m[1][0] = 7
  >     println("{m[0][0]} {m[1][0]}")
  >     print("ab")
  >     print("c\n")
  >     io.write_byte(72)
  >     io.write_byte(105)
  >     io.write_byte(10)
  > }
  > EOF
  $ sinterc run ok.sn
  2 3 5 7 11 13 17 19 23 29
  a!
  B
  43 q
  0 7
  abc
  Hi

下标越界时 panic（右边已经算好，不会写到缓冲区外面）：

  $ cat > oob.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Str> = Vec.new()
  >     v.push("a")
  >     v[3] = "x"
  > }
  > EOF
  $ sinterc run oob.sn
  panic: 下标越界
  [101]

let 声明的 Vec 不能改：

  $ cat > let.sn <<'EOF'
  > fn main() {
  >     let v: Vec<I64> = Vec.new()
  >     v[0] = 1
  > }
  > EOF
  $ sinterc let.sn
  let.sn:3:5: 错误：v 不可修改，不能给它的元素赋值（需要 var 变量或 inout / sink 参数）
   3 |     v[0] = 1
     |     ^
  [1]

不能从 Vec 里移出元素（要拿走用 vec_pop，或者用 vec_swap 换到最后再 pop）：

  $ cat > move.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Str> = Vec.new()
  >     v.push("a")
  >     var w: Vec<Str> = Vec.new()
  >     w.push("b")
  >     w[0] = v[0]
  > }
  > EOF
  $ sinterc move.sn 2>&1 | grep -c 错误
  1

只能给位置赋值：

  $ cat > notplace.sn <<'EOF'
  > fn one() -> I64 { 1 }
  > fn main() {
  >     one() = 2
  > }
  > EOF
  $ sinterc notplace.sn
  notplace.sn:3:5: 错误：只能给变量、字段或 Vec 的元素（v[i]）赋值
   3 |     one() = 2
     |     ^
  [1]

print_byte 只接受 0 到 255：

  $ cat > byte.sn <<'EOF'
  > use std.io
  > 
  > fn main() {
  >     print("x")
  >     io.write_byte(256)
  > }
  > EOF
  $ sinterc run byte.sn
  xpanic: print_byte 的参数不在 0 到 255 之间
  [101]

英文：

  $ sinterc --lang=en notplace.sn
  notplace.sn:3:5: error: only a variable, a field or an element of a Vec (v[i]) can be assigned to
   3 |     one() = 2
     |     ^
  [1]
