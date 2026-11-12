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
