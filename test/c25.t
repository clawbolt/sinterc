C25：试用版的命令行。sinterc run 编译并运行，sinterc build 生成可执行文件，--version、--help；
原来的 sinterc 源文件.sn -o 输出.c 不变。build 和 run 调用环境变量 CC 指定的 C 编译器（默认 cc）。

  $ cat > tour.sn <<'EOF'
  > // Sinter 快速一览：结构体、参数约定、Box、Vec、match、arena。运行：sinterc run tour.sn
  > 
  > struct Point { x: I64, y: I64 }
  > 
  > fn dist2(p: Point) -> I64 { p.x * p.x + p.y * p.y }
  > 
  > // inout：修改调用方的变量或字段
  > fn inc(inout x: I64) { x = x + 1 }
  > 
  > // let 参数（默认）只读借用；循环里用下标读 Vec 的元素
  > fn sum(v: Vec<Box<I64>>) -> I64 {
  >     var s = 0
  >     var i = 0
  >     while i < v.len() {
  >         s = s + *vec_get(v, i)
  >         i = i + 1
  >     }
  >     s
  > }
  > 
  > // 引用只能来自 arena，类型写出它属于哪个区域
  > fn make(a: Arena, n: I64) -> &I64 in a { alloc(a, n) }
  > 
  > fn main() {
  >     print("== Sinter 一览 ==")
  >     var p = Point { x: 3, y: 4 }
  >     print_i64(dist2(p))
  >     inc(p.x)
  >     print_i64(p.x)
  > 
  >     var v: Vec<Box<I64>> = vec_new()
  >     v.push(box(1))
  >     v.push(box(2))
  >     v.push(box(3))
  >     print_i64(sum(v))
  >     match v.pop() {
  >         .Some(b) => print_i64(*b),
  >         .None => print_i64(0)
  >     }
  > 
  >     arena |a| {
  >         let r = make(a, 42)
  >         print_i64(*r)
  >     }
  > }
  > EOF
  $ sinterc run tour.sn
  == Sinter 一览 ==
  25
  4
  6
  3
  42

build 默认输出到去掉 .sn 的同名文件：

  $ sinterc build tour.sn && ./tour
  == Sinter 一览 ==
  25
  4
  6
  3
  42

运行时出错（panic）的退出码原样传出来：

  $ cat > oob.sn <<'EOF'
  > fn main() {
  >     var w: Vec<I64> = vec_new()
  >     print_i64(vec_get(w, 3))
  > }
  > EOF
  $ sinterc run oob.sn
  panic: 下标越界
  [101]

编译错误：

  $ cat > move.sn <<'EOF'
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > fn main() {
  >     let b = box(1)
  >     print_i64(take(b))
  >     print_i64(*b)
  > }
  > EOF
  $ sinterc run move.sn
  move.sn:5:15: 错误：b 已被移动，不能再使用
   5 |     print_i64(*b)
     |               ^
  [1]

  $ sinterc --version
  sinterc 0.2.0-pre
