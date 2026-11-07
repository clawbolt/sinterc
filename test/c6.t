C6：枚举（Lean M8（一））。enum E { A(T), B }，.A(e) / .B 按上下文定类型；
match 按标签选分支，必须穷尽，按移动拆开：载荷交给分支变量，枚举本身变成已移走。
没有被 match 的枚举在作用域结束时按运行时标签析构；Box 计入泄漏检查，程序正常退出就说明没有泄漏。

构造、按移动 match、按标签析构、分支里有时移走载荷（drop flag）：

  $ cat > ok.sn <<'EOF'
  > enum Shape {
  >     Circle(I64),
  >     Rect(w: I64),
  >     Dot
  > }
  > 
  > enum Opt { Some(Box<I64>), None }
  > 
  > fn area(s: Shape) -> I64 {
  >     match s {
  >         .Circle(r) => 3 * r * r,
  >         .Rect(w) => w * w,
  >         .Dot => 0
  >     }
  > }
  > 
  > fn take(sink o: Opt) -> I64 {
  >     match o {
  >         .Some(b) => *b,
  >         .None => -1
  >     }
  > }
  > 
  > fn wrap(n: I64) -> Opt {
  >     if n > 0 { .Some(box(n)) } else { .None }
  > }
  > 
  > fn main() {
  >     print_i64(area(.Circle(2)))
  >     print_i64(area(.Rect(5)))
  >     print_i64(area(.Dot))
  >     print_i64(take(wrap(7)))
  >     print_i64(take(wrap(0)))
  >     let o: Opt = .Some(box(3))
  >     let p = wrap(4)
  >     var q: Opt = .None
  >     q = .Some(box(9))
  >     let r = match p {
  >         .Some(b) => {
  >             if *b > 3 { take(.Some(b)) } else { 0 }
  >         }
  >         .None => 0
  >     }
  >     print_i64(r)
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  12
  25
  0
  7
  -1
  4

漏掉分支（Lean M8 的反例：不经检查时值是 .B 就没有分支可走）：

  $ cat > nonexh.sn <<'EOF'
  > enum E { A(I64), B }
  > fn main() {
  >     let e: E = .A(1)
  >     let n = match e {
  >         .A(x) => x
  >     }
  > }
  > EOF
  $ sinterc nonexh.sn
  nonexh.sn:4:13: 错误：match 没有覆盖 .B（枚举的 match 必须穷尽）
  [1]

载荷移进枚举之后不能再用：

  $ cat > moved.sn <<'EOF'
  > enum Opt { Some(Box<I64>), None }
  > fn main() {
  >     let b = box(1)
  >     let o: Opt = .Some(b)
  >     print_i64(*b)
  > }
  > EOF
  $ sinterc moved.sn
  moved.sn:5:15: 错误：b 已被移动，不能再使用
  [1]

match 之后枚举已被拆开，不能再用：

  $ cat > after.sn <<'EOF'
  > enum Opt { Some(Box<I64>), None }
  > fn main() {
  >     let o: Opt = .Some(box(1))
  >     let n = match o {
  >         .Some(b) => *b,
  >         .None => 0
  >     }
  >     let m = match o {
  >         .Some(b) => *b,
  >         .None => 0
  >     }
  > }
  > EOF
  $ sinterc after.sn
  after.sn:8:19: 错误：o 已被移动，不能再使用
  [1]

只有一个分支给 k 赋值，合并后 k 可能没有值（Lean M8 的 maybe）：

  $ cat > maybe.sn <<'EOF'
  > enum Opt { Some(Box<I64>), None }
  > fn main() {
  >     let o: Opt = .Some(box(1))
  >     var k: Box<I64>
  >     match o {
  >         .Some(b) => { k = b }
  >         .None => { }
  >     }
  >     print_i64(*k)
  > }
  > EOF
  $ sinterc maybe.sn
  maybe.sn:9:15: 错误：k 可能已被移动（某条路径上或上一轮循环里），不能再使用
  [1]

构造时标签和载荷必须对得上：

  $ cat > pay.sn <<'EOF'
  > enum E { A(I64), B }
  > fn main() {
  >     let e: E = .B(1)
  > }
  > EOF
  $ sinterc pay.sn
  pay.sn:3:16: 错误：.B 没有载荷，不能写 .B(…)
  [1]
