返回借用（Lean `Projection.Return`）。返回类型 &T in m 里的 m 是 let 参数的名字：结果借自 m 对应的实参。
调用处结果还活着时，实参被锁住，规则和投影 let a = p.a 相同：只读使用照常，不能修改、移动、析构、以 inout 传入。
s.view() 是借自 s 的整段切片，不复制；切片的方法都能用。锁只持续到结果最后一次使用。

  $ cat > ok.sn <<'EOF'
  > fn first(line: Str) -> &str in line {
  >     line.view().sub(0, 3)
  > }
  > fn main() {
  >     var s = "abc,de,f"
  >     let v = s.view()
  >     println("{v.len()} {v.split(",").len()}")
  >     let f = first(s)
  >     println(f.trim().to_str())
  >     s.push("!")
  >     println(s)
  >     var total = 0
  >     let rows = "1,2\n30,4"
  >     for line in rows.view().split("\n") {
  >         for x in line.split(",") {
  >             match x.parse_i64() {
  >                 .Some(n) => { total = total + n },
  >                 .None => ()
  >             }
  >         }
  >     }
  >     println("{total}")
  > }
  > EOF
  $ sinterc run ok.sn
  8 3
  abc
  abc,de,f!
  37

切片还要用时修改原来的字符串：拒绝（push 可能换掉缓冲区）：

  $ cat > push.sn <<'EOF'
  > fn main() {
  >     var s = "abc"
  >     let v = s.view()
  >     s.push("x")
  >     println("{v.len()}")
  > }
  > EOF
  $ sinterc push.sn
  push.sn:4:5: 错误：s 正被引用 v 借用（之后还会用到 v），不能以 inout 传入
   4 |     s.push("x")
     |     ^
  [1]

移走原来的字符串、借自它的引用经过函数传出来之后也一样：

  $ cat > move.sn <<'EOF'
  > fn first(line: Str) -> &str in line {
  >     line.view().sub(0, 1)
  > }
  > fn main() {
  >     let s = "abc"
  >     let f = first(s).trim()
  >     let t = s
  >     println(f.to_str())
  > }
  > EOF
  $ sinterc move.sn
  move.sn:7:13: 错误：s 正被引用 f 借用（之后还会用到 f），不能移动
   7 |     let t = s
     |             ^
  [1]

返回的引用只能借自返回类型里写的 let 参数：

  $ cat > local.sn <<'EOF'
  > fn bad(n: I64) -> &str in n {
  >     let s = "x"
  >     s.view()
  > }
  > fn main() {}
  > EOF
  $ sinterc local.sn
  local.sn:3:7: 错误：类型不匹配：需要 &str in n，实际是 &str in s
   3 |     s.view()
     |       ^
  [1]

  $ cat > inout.sn <<'EOF'
  > fn bad(inout s: Str) -> &str in s {
  >     s.view()
  > }
  > fn main() {}
  > EOF
  $ sinterc inout.sn
  inout.sn:1:1: 错误：返回的引用只能借自 let 参数，s 是 inout 参数
   1 | fn bad(inout s: Str) -> &str in s {
     | ^
  [1]

借自的字符串在块结束时析构，引用不能带出块：

  $ cat > scope.sn <<'EOF'
  > fn main() {
  >     let w = {
  >         let s = "abc".clone()
  >         s.view()
  >     }
  >     println("{w.len()}")
  > }
  > EOF
  $ sinterc scope.sn
  scope.sn:3:9: 错误：s 在作用域结束时析构，但借自它的引用之后还会用到（引用不能带出 s 的作用域）
   3 |         let s = "abc".clone()
     |         ^
  [1]

借用 &p：p 是变量、字段、元素或解引用，得到借自 p 所在变量的引用。泛型函数也可以返回借用：
Vec 的 get、first、last 返回 Option<&T in v>，Map、HashMap 的 get 返回 Option<&V in m>：

  $ cat > generic.sn <<'EOF2'
  > use std.map
  > use std.hashmap
  > 
  > struct P { x: I64, name: Str }
  > 
  > fn longest(v: Vec<Str>) -> &Str in v {
  >     var best = 0
  >     for i in 0..v.len() {
  >         if v[i].len() > v[best].len() { best = i }
  >     }
  >     &v[best]
  > }
  > 
  > fn main() {
  >     var v: Vec<Str> = Vec.new()
  >     v.push("ab")
  >     v.push("abcd")
  >     v.push("c")
  >     let l = longest(v)
  >     println(*l)
  >     match v.first() {
  >         .Some(s) => println(*s),
  >         .None => {}
  >     }
  >     println("{v.get(5).is_none()} {v.last().is_some()}")
  >     let p = P { x: 1, name: "p" }
  >     let r = &p.name
  >     println("{(*r).len()}")
  >     var m: map.Map<Str, I64> = map.Map.new()
  >     let old = m.insert("a", 1)
  >     match m.get("a") {
  >         .Some(n) => println("{*n + 1}"),
  >         .None => println("没有")
  >     }
  >     var h: hashmap.HashMap<I64, Str> = hashmap.HashMap.new()
  >     let old2 = h.insert(7, "seven")
  >     match h.get(7) {
  >         .Some(s) => println(*s),
  >         .None => println("没有")
  >     }
  >     v.push("d")
  >     println("{v.len()}")
  > }
  > EOF2
  $ sinterc run generic.sn
  abcd
  ab
  true true
  1
  2
  seven
  4

借用还在时修改 Map：

  $ cat > maplock.sn <<'EOF2'
  > use std.map
  > 
  > fn main() {
  >     var m: map.Map<I64, Str> = map.Map.new()
  >     let a = m.insert(1, "x")
  >     let s = m.get(1)
  >     let b = m.insert(2, "y")
  >     println("{s.is_some()}")
  > }
  > EOF2
  $ sinterc maplock.sn
  maplock.sn:7:13: 错误：m 正被引用 s 借用（之后还会用到 s），不能以 inout 传入
   7 |     let b = m.insert(2, "y")
     |             ^
  [1]

& 后面要是位置；返回借用的泛型函数的类型实参还不能含引用：

  $ cat > notplace.sn <<'EOF2'
  > fn main() {
  >     let r = &"x".len()
  > }
  > EOF2
  $ sinterc notplace.sn
  notplace.sn:2:13: 错误：& 后面必须是变量、字段、元素或解引用（要借用的位置）
   2 |     let r = &"x".len()
     |             ^
  [1]

  $ cat > refref.sn <<'EOF2'
  > fn main() {
  >     let s = "a,b"
  >     let parts = s.view().split(",")
  >     let f = parts.first()
  > }
  > EOF2
  $ sinterc refref.sn
  refref.sn:4:19: 错误：Vec<T>.first 返回借用，类型实参还不能是含引用的类型 &str in s
   4 |     let f = parts.first()
     |                   ^
  [1]
