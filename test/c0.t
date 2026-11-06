C0 骨架的端到端测试：编译成 C，用系统的 cc 编译运行。

结构体、函数、递归、if 表达式、while、赋值：

  $ cat > ok.sn <<'EOF'
  > struct Point { x: I64, y: I64 }
  > 
  > fn add(a: Point, b: Point) -> Point {
  >     Point { x: a.x + b.x, y: a.y + b.y }
  > }
  > 
  > fn fact(n: I64) -> I64 {
  >     if n <= 1 { 1 } else { n * fact(n - 1) }
  > }
  > 
  > fn main() {
  >     let p = add(Point { x: 1, y: 2 }, Point { x: 10, y: 20 })
  >     print_i64(p.x)
  >     print_i64(p.y)
  >     print_i64(fact(20))
  >     let i = 0
  >     let s = 0
  >     while i < 5 {
  >         i = i + 1
  >         s = s + i
  >     }
  >     print_i64(s)
  >     print_bool(s == 15 && !false)
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  11
  22
  2432902008176640000
  15
  true

溢出检查：21! 超出 I64，panic，退出码 101：

  $ cat > ovf.sn <<'EOF'
  > fn fact(n: I64) -> I64 {
  >     if n <= 1 { return 1 }
  >     n * fact(n - 1)
  > }
  > 
  > fn main() {
  >     print_i64(fact(21))
  > }
  > EOF
  $ sinterc ovf.sn -o ovf.c && cc -std=gnu11 -w ovf.c -o ovf && ./ovf
  panic: 整数溢出
  [101]

参数从左到右求值：

  $ cat > order.sn <<'EOF'
  > fn show(x: I64) -> I64 {
  >     print_i64(x)
  >     x
  > }
  > 
  > fn sub(a: I64, b: I64) -> I64 {
  >     a - b
  > }
  > 
  > fn main() {
  >     print_i64(sub(show(1), show(2)))
  > }
  > EOF
  $ sinterc order.sn -o order.c && cc -std=gnu11 -w order.c -o order && ./order
  1
  2
  -1

类型错误：

  $ cat > bad.sn <<'EOF'
  > fn main() {
  >     let x = 1
  >     let y = x + true
  > }
  > EOF
  $ sinterc bad.sn
  bad.sn:3:17: 错误：类型不匹配：需要 I64，实际是 Bool
  [1]

结构体缺字段：

  $ cat > field.sn <<'EOF'
  > struct Point { x: I64, y: I64 }
  > 
  > fn main() {
  >     let p = Point { x: 1 }
  > }
  > EOF
  $ sinterc field.sn
  field.sn:4:13: 错误：Point 缺少字段 y
  [1]

函数返回类型和函数体不符：

  $ cat > ret.sn <<'EOF'
  > fn f() -> I64 {
  >     true
  > }
  > 
  > fn main() {
  > }
  > EOF
  $ sinterc ret.sn
  ret.sn:1:1: 错误：类型不匹配：需要 I64，实际是 Bool
  [1]
