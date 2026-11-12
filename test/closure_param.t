闭包（Lean `Closures.Param`）。参数类型写 fn(T) -> U，实参写闭包 |x| e；闭包只能当实参传（不能存、不能返回），
可以读外面的变量（按 let 捕获），也可以改外面的 var 变量（按 inout 捕获）。实现是按闭包特化：调用处把捕获当作额外的实参，
和别的实参一起检查排他性。std.vec 有了 map、filter、any、all、count、for_each、fold。

  $ cat > ok.sn <<'EOF'
  > 
  > fn apply_twice(x: I64, f: fn(I64) -> I64) -> I64 {
  >     f(f(x))
  > }
  > 
  > // 把闭包原样传下去
  > fn total_of(v: Vec<I64>, f: fn(I64) -> I64) -> I64 {
  >     v.fold(0, |a, x| a + x) + v.count(|x| x > 100) + apply_twice(0, f)
  > }
  > 
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     for i in 1..6 {
  >         v.push(i)
  >     }
  >     let k = 10
  >     let w = v.map(|x| x * k)
  >     for x in w {
  >         println("{x}")
  >     }
  >     println("{v.any(|x| x > 4)}")
  >     println("{v.all(|x| x > 4)}")
  >     var total = 0
  >     v.for_each(|x| { total = total + x })
  >     println("{total}")
  >     println("{v.fold(0, |a, x| a * 10 + x)}")
  >     let odd = w.filter(|x| x % 20 != 0)
  >     for x in odd {
  >         println("{x}")
  >     }
  >     println("{apply_twice(3, |x| x + k)}")
  >     println("{total_of(v, |x| x + 1)}")
  >     var names: Vec<Str> = vec_new()
  >     names.push("ann")
  >     names.push("bob")
  >     let shout = names.map(|s| str_concat(s, "!"))
  >     for s in shout {
  >         println(s)
  >     }
  >     println(names.count(|s| str_eq(s, "bob")).to_str())
  >     let long = names.filter(|s| s.len() > 2)
  >     println("{vec_len(long)}")
  > }
  > EOF
  $ sinterc run ok.sn
  10
  20
  30
  40
  50
  true
  false
  15
  12345
  10
  30
  50
  23
  17
  ann!
  bob!
  1
  2

闭包改 v，同时 v 又作为实参传给 for_each：排他性冲突（`Closures.Param` 的反例）：

  $ cat > conflict.sn <<'EOF'
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     v.push(1)
  >     v.for_each(|x| { v.push(x) })
  > }
  > EOF
  $ sinterc conflict.sn
  conflict.sn:4:16: 错误：v 在同一次调用里出现两次，其中有移动或 inout，只能都是只读的
   4 |     v.for_each(|x| { v.push(x) })
     |                ^
  [1]

闭包不写类型时不能存进变量（要存起来用 Fn(…)）：

  $ cat > store.sn <<'EOF'
  > fn main() {
  >     let f = |x| x + 1
  > }
  > EOF
  $ sinterc store.sn
  store.sn:2:13: 错误：无法确定闭包的类型：写在调用的实参里传给 fn(…) 参数，或者写在知道类型的地方（例如 var f: Fn(I64) -> I64 = |x| …）
   2 |     let f = |x| x + 1
     |             ^
  [1]

也不能返回：

  $ cat > ret.sn <<'EOF'
  > fn mk() -> fn(I64) -> I64 {
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc ret.sn
  ret.sn:1:1: 错误：函数类型 fn(…) 只能作为函数参数的类型（闭包不能存进变量、字段、Vec，也不能返回）
   1 | fn mk() -> fn(I64) -> I64 {
     | ^
  [1]

函数参数只能调用或传下去：

  $ cat > fnvar.sn <<'EOF'
  > fn h(f: fn(I64) -> I64) -> I64 {
  >     let g = f
  >     0
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc fnvar.sn
  fnvar.sn:2:13: 错误：f 是函数参数，只能调用它（f(…)），或者作为实参传给 fn(…) 类型的参数
   2 |     let g = f
     |             ^
  [1]

只有 var 变量能在闭包里改：

  $ cat > letcap.sn <<'EOF'
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     let t = 0
  >     v.for_each(|x| { t = t + x })
  > }
  > EOF
  $ sinterc letcap.sn
  letcap.sn:4:22: 错误：t 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）
   4 |     v.for_each(|x| { t = t + x })
     |                      ^
  [1]

闭包里暂不能再写闭包：

  $ cat > nested.sn <<'EOF'
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     println("{v.any(|x| v.any(|y| y == x))}")
  > }
  > EOF
  $ sinterc nested.sn
  nested.sn:3:31: 错误：闭包里暂不能再写闭包
   3 |     println("{v.any(|x| v.any(|y| y == x))}")
     |                               ^
  [1]
