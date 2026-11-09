C40：闭包（Lean M21）。参数类型写 fn(T) -> U，实参写闭包 |x| e；闭包只能当实参传（不能存、不能返回），
可以读外面的变量（按 let 捕获），也可以改外面的 var 变量（按 inout 捕获）。实现是按闭包特化：调用处把捕获当作额外的实参，
和别的实参一起检查排他性。std.vec 有了 map、filter、any、all、count、for_each、fold。

  $ cat > ok.sn <<'EOF'
  > use std.vec
  > 
  > fn apply_twice(x: I64, f: fn(I64) -> I64) -> I64 {
  >     f(f(x))
  > }
  > 
  > // 把闭包原样传下去
  > fn total_of(v: Vec<I64>, f: fn(I64) -> I64) -> I64 {
  >     vec.fold(v, 0, |a, x| a + x) + vec.count(v, |x| x > 100) + apply_twice(0, f)
  > }
  > 
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     for i in 1..6 {
  >         v.push(i)
  >     }
  >     let k = 10
  >     let w = vec.map(v, |x| x * k)
  >     for x in w {
  >         print_i64(x)
  >     }
  >     print_bool(vec.any(v, |x| x > 4))
  >     print_bool(vec.all(v, |x| x > 4))
  >     var total = 0
  >     vec.for_each(v, |x| { total = total + x })
  >     print_i64(total)
  >     print_i64(vec.fold(v, 0, |a, x| a * 10 + x))
  >     let odd = vec.filter(w, |x| x % 20 != 0)
  >     for x in odd {
  >         print_i64(x)
  >     }
  >     print_i64(apply_twice(3, |x| x + k))
  >     print_i64(total_of(v, |x| x + 1))
  >     var names: Vec<Str> = vec_new()
  >     names.push("ann")
  >     names.push("bob")
  >     let shout = vec.map(names, |s| str_concat(s, "!"))
  >     for s in shout {
  >         print(s)
  >     }
  >     print_i64(vec.count(names, |s| str_eq(s, "bob")))
  >     let long = vec.filter(names, |s| s.len() > 2)
  >     print_i64(vec_len(long))
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

闭包改 v，同时 v 又作为实参传给 for_each：排他性冲突（M21 的反例）：

  $ cat > conflict.sn <<'EOF'
  > use std.vec
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     v.push(1)
  >     vec.for_each(v, |x| { v.push(x) })
  > }
  > EOF
  $ sinterc conflict.sn
  conflict.sn:5:21: 错误：v 在同一次调用里出现两次，其中有移动或 inout，只能都是只读的
  [1]

闭包不能存进变量：

  $ cat > store.sn <<'EOF'
  > fn main() {
  >     let f = |x| x + 1
  > }
  > EOF
  $ sinterc store.sn
  store.sn:2:13: 错误：闭包只能直接写在调用的实参里，传给类型是 fn(…) 的参数
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
  [1]

只有 var 变量能在闭包里改：

  $ cat > letcap.sn <<'EOF'
  > use std.vec
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     let t = 0
  >     vec.for_each(v, |x| { t = t + x })
  > }
  > EOF
  $ sinterc letcap.sn
  letcap.sn:5:27: 错误：t 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）
  [1]

闭包里暂不能再写闭包：

  $ cat > nested.sn <<'EOF'
  > use std.vec
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     print_bool(vec.any(v, |x| vec.any(v, |y| y == x)))
  > }
  > EOF
  $ sinterc nested.sn
  nested.sn:4:42: 错误：闭包里暂不能再写闭包
  [1]
