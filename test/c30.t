C30：for 循环。for i in a..b 依次取 a、a+1、…、b-1（a、b 各求值一次，i 不能改）；
for x in v 按借用依次取 Vec 的每个元素：可复制的元素复制，需要析构的元素借用并锁住 v。
解析时展开成 while，检查规则和翻译验证都沿用已有的（循环 Lean M2b、投影 M4、Vec M12）。

  $ cat > ok.sn <<'EOF'
  > fn sum(v: Vec<I64>) -> I64 {
  >     var s = 0
  >     for x in v {
  >         s = s + x
  >     }
  >     s
  > }
  > 
  > fn main() {
  >     for i in 0..3 {
  >         print_i64(i)
  >     }
  >     var v: Vec<I64> = vec_new()
  >     for i in 1..5 { v.push(i * i) }
  >     print_i64(sum(v))
  >     var names: Vec<Str> = vec_new()
  >     names.push("甲")
  >     names.push("乙")
  >     for n in names {
  >         print(n)
  >     }
  >     for i in 0..2 {
  >         for j in 0..2 {
  >             print_i64(i * 10 + j)
  >         }
  >     }
  >     for i in 5..5 { print_i64(i) }
  >     print("完")
  > }
  > EOF
  $ sinterc run ok.sn
  0
  1
  2
  30
  甲
  乙
  0
  1
  10
  11
  完

循环变量不能改：

  $ cat > set.sn <<'EOF'
  > fn main() {
  >     for i in 0..3 {
  >         i = 5
  >     }
  > }
  > EOF
  $ sinterc set.sn
  set.sn:3:9: 错误：i 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）
  [1]

元素还被借用时不能修改 Vec：

  $ cat > lock.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Str> = vec_new()
  >     v.push("a")
  >     for s in v {
  >         v.push("b")
  >         print(s)
  >     }
  > }
  > EOF
  $ sinterc lock.sn
  lock.sn:5:9: 错误：v 正被投影 s 借用（之后还会用到 s），不能以 inout 传入
  [1]

只能遍历变量或字段：

  $ cat > tmp.sn <<'EOF'
  > fn make() -> Vec<I64> { vec_new() }
  > fn main() {
  >     for x in make() { print_i64(x) }
  > }
  > EOF
  $ sinterc tmp.sn
  tmp.sn:3:14: 错误：for 只能遍历变量或字段里的 Vec，或者写成 a..b
  [1]
