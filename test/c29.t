C29：字符串类型 Str（Lean M17）。字符串拥有一块堆内存，和 Box 一样只能移动、作用域结束时自动析构；
字面量每次求值分配一个新的 Str。内建函数（都可以写成方法调用）：
str_len(s)、str_eq(a, b)、str_concat(a, b)（新的字符串）、str_push(inout s, t)（原地追加）、
str_clone(s)、str_from_i64(n)；print(s) 打印并换行。实参都按 let 借用，只有 str_push 的第一个参数是 inout。

  $ cat > ok.sn <<'EOF'
  > struct Person { name: Str, age: I64 }
  > 
  > fn greet(p: Person) -> Str {
  >     var s = "你好，"
  >     s.push(p.name)
  >     s.push("！")
  >     s
  > }
  > 
  > fn main() {
  >     let a = "ab"
  >     let b = a.concat("cd")
  >     print(b)
  >     print_i64(b.len())
  >     let p = Person { name: "小明", age: 3 }
  >     print(greet(p))
  >     var v: Vec<Str> = vec_new()
  >     v.push("x")
  >     v.push(str_from_i64(42))
  >     print_i64(v.len())
  >     match v.pop() {
  >         .Some(s) => print(s),
  >         .None => print("空")
  >     }
  >     print_bool(a.eq("ab"))
  >     print_bool(str_eq(a, b))
  >     let c = a.clone()
  >     let d = c
  >     print(d)
  >     print(str_from_i64(-7))
  > }
  > EOF
  $ sinterc run ok.sn
  abcd
  4
  你好，小明！
  2
  42
  true
  false
  ab
  -7

移动之后再使用：

  $ cat > move.sn <<'EOF'
  > fn main() {
  >     let s = "a"
  >     let t = s
  >     print(s)
  > }
  > EOF
  $ sinterc move.sn
  move.sn:4:11: 错误：s 已被移动，不能再使用
  [1]

字符串不能用 == 比较：

  $ cat > eq.sn <<'EOF'
  > fn main() {
  >     let a = "a"
  >     print_bool(a == "a")
  > }
  > EOF
  $ sinterc eq.sn
  eq.sn:3:16: 错误：字符串用 str_eq(a, b) 或 a.eq(b) 比较
  [1]
  $ sinterc --lang=en eq.sn
  eq.sn:3:16: error: compare strings with str_eq(a, b) or a.eq(b)
  [1]

let 变量不能追加：

  $ cat > push.sn <<'EOF'
  > fn main() {
  >     let s = "a"
  >     s.push("b")
  > }
  > EOF
  $ sinterc push.sn
  push.sn:3:5: 错误：s 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）
  [1]
