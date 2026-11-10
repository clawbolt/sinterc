C49：字符串插值。"x = {x}" 里 { } 中间是一个表达式，按类型转成字符串（x.to_str()）：
I64、F64、Char、Bool 和 Str 是内建的，别的类型调用户写的 to_str 函数（按借用传入，不移走）。
{{ 和 }} 是花括号本身。只是 str_push 和 to_str 的组合，不需要新的推理。

  $ cat > ok.sn <<'EOF'
  > struct Point { x: I64, y: I64 }
  > 
  > fn to_str(p: Point) -> Str {
  >     "({p.x}, {p.y})"
  > }
  > 
  > fn main() {
  >     let name = "世界"
  >     let n = 42
  >     print("你好，{name}！")
  >     print("n = {n}, n * 2 = {n * 2}")
  >     print("{1.5} {'中'} {n > 40} {true}")
  >     let p = Point { x: 3, y: -4 }
  >     print("p = {p}")
  >     print("{{原样}} {n}")
  >     print("嵌套：{"[{n}]"}")
  >     var s = "{n}"
  >     s.push("!")
  >     print(s)
  >     print(name)
  >     print("{str_len(name)} 字节")
  >     print(n.to_str())
  > }
  > EOF
  $ sinterc run ok.sn
  你好，世界！
  n = 42, n * 2 = 84
  1.5 中 true true
  p = (3, -4)
  {原样} 42
  嵌套：[42]
  42!
  世界
  6 字节
  42

单独的 } 要写成 }}：

  $ cat > brace.sn <<'EOF'
  > fn main() {
  >     print("a } b")
  > }
  > EOF
  $ sinterc brace.sn
  brace.sn:2:14: 错误：字符串里的 } 要写成 }}
   2 |     print("a } b")
     |              ^
  [1]

{} 中间要有表达式：

  $ cat > empty.sn <<'EOF'
  > fn main() {
  >     print("a {} b")
  > }
  > EOF
  $ sinterc empty.sn
  empty.sn:2:14: 错误：字符串里的 {} 中间要写一个表达式
   2 |     print("a {} b")
     |              ^
  [1]

没有 to_str 的类型不能插值：

  $ cat > noshow.sn <<'EOF'
  > struct P { x: I64 }
  > fn main() {
  >     let p = P { x: 1 }
  >     print("{p}")
  > }
  > EOF
  $ sinterc noshow.sn
  noshow.sn:4:12: 错误：P 没有实现 ToStr，不能在字符串里插值或调用 to_str（先写 fn to_str(x: P) -> Str）
   4 |     print("{p}")
     |            ^
  [1]
