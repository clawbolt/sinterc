C4：区域。arena |a| { … } 引入区域 a，alloc(a, e) 在 a 里分配，得到 &T in a；块结束时整块释放。
区域的内存块也计入泄漏检查，所以下面程序正常退出就说明 return、? 穿过区域块时区域都被释放了。

区域参数（a: Arena）、参数类型里的区域名按实参实例化；外层区域的引用可以当作内层的用（D2）；
? 在区域块里出错返回：

  $ cat > ok.sn <<'EOF'
  > struct P { x: I64, y: I64 }
  > 
  > fn first(a: Arena, n: I64) -> &I64 in a {
  >     alloc(a, n * 2)
  > }
  > 
  > fn add(r: &I64 in x, s: &I64 in y) -> I64 { *r + *s }
  > 
  > fn try_alloc(a: Arena, n: I64) -> Result<&I64 in a, I64> {
  >     if n < 0 { return Err(n) }
  >     Ok(alloc(a, n))
  > }
  > 
  > fn run(n: I64) -> Result<I64, I64> {
  >     arena |a| {
  >         let base = alloc(a, 1)
  >         let r = try_alloc(a, n)?
  >         Ok(*r + *base)
  >     }
  > }
  > 
  > fn main() {
  >     let total = arena |a| {
  >         let r = first(a, 5)
  >         let p = alloc(a, P { x: 1, y: 2 })
  >         arena |b| {
  >             var s = alloc(b, 100)
  >             print_i64(add(r, s))
  >             s = r
  >             print_i64(add(r, s))
  >         }
  >         *r + (*p).x + (*p).y
  >     }
  >     print_i64(total)
  >     print_i64(match run(4) { Ok(v) => v, Err(e) => e })
  >     print_i64(match run(-1) { Ok(v) => v, Err(e) => e })
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  110
  20
  13
  5
  -1

区域里的引用作为块的值带出区域（R3，设计稿的 R03）：

  $ cat > escape.sn <<'EOF'
  > fn main() {
  >     let x = arena |a| {
  >         let r = alloc(a, 1)
  >         r
  >     }
  > }
  > EOF
  $ sinterc escape.sn
  escape.sn:4:9: 错误：块的值的类型 &I64 in a 含有区域 a，不能带出这个区域（R3）
  [1]

内层区域的引用赋给外层变量（Lean M5 的反例：内层区域结束后外层再读就悬空）：

  $ cat > outer.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         var r = alloc(a, 1)
  >         arena |b| {
  >             r = alloc(b, 2)
  >         }
  >         print_i64(*r)
  >     }
  > }
  > EOF
  $ sinterc outer.sn
  outer.sn:5:17: 错误：类型不匹配：需要 &I64 in a，实际是 &I64 in b
  [1]

区域里放需要析构的值（D4，Lean M5（三）的反例：区域结束时不运行析构，Box 泄漏）：

  $ cat > d4.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         let r = alloc(a, box(1))
  >     }
  > }
  > EOF
  $ sinterc d4.sn
  d4.sn:3:17: 错误：区域里不能放需要析构或 linear 的值 Box<I64>：区域结束时不运行析构（D4）
  [1]

函数的两个区域参数之间没有顺序，不能互相转换（D3，Lean M5（四）的反例）：

  $ cat > d3.sn <<'EOF'
  > fn pick(a: Arena, b: Arena, r: &I64 in a) -> &I64 in b {
  >     r
  > }
  > 
  > fn main() { }
  > EOF
  $ sinterc d3.sn
  d3.sn:2:5: 错误：类型不匹配：需要 &I64 in b，实际是 &I64 in a
  [1]
