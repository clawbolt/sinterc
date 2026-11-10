C55：嵌套模式（Lean M24、M24b）。模式可以是 _、变量、字面量、变体 .V / .V(p)、元组 (p, q, …)，可以任意嵌套。
展开成两步：按借用依次测试各个分支，只对选中的分支拆开、绑定变量（M24 的 run_correct）。覆盖性按代表值检查：
每种情况都要有分支匹配，每个分支都要能匹配到某种情况（M24b）。借来的参数和 match let 按借用拆开。

  $ cat > ok.sn <<'EOF'
  > enum Shape { Circle(I64), Rect((I64, I64)), Dot }
  > 
  > fn area(s: Shape) -> I64 {
  >     match s {
  >         .Circle(r) => 3 * r * r,
  >         .Rect((w, 0)) => 0,
  >         .Rect((w, h)) => w * h,
  >         .Dot => 0
  >     }
  > }
  > 
  > fn classify(o: Option<(I64, Str)>) -> Str {
  >     match o {
  >         .Some((0, s)) => "zero {s}",
  >         .Some((n, s)) => if n < 0 { "neg {s}" } else { "pos {s}" },
  >         .None => "none"
  >     }
  > }
  > 
  > fn take(sink s: Str) -> Str { s }
  > 
  > fn first(sink p: Option<(Str, Str)>) -> Str {
  >     match p {
  >         .Some((a, b)) => take(a),
  >         .None => "-"
  >     }
  > }
  > 
  > fn fizz(n: I64) -> Str {
  >     match (n % 3, n % 5) {
  >         (0, 0) => "FizzBuzz",
  >         (0, _) => "Fizz",
  >         (_, 0) => "Buzz",
  >         _ => n.to_str()
  >     }
  > }
  > 
  > fn depth(o: Option<Option<I64>>) -> I64 {
  >     match o {
  >         .Some(.Some(x)) => x,
  >         .Some(.None) => -1,
  >         .None => -2
  >     }
  > }
  > 
  > fn both(a: Bool, b: Bool) -> I64 {
  >     match (a, b) {
  >         (true, true) => 1,
  >         (true, false) => 2,
  >         (false, _) => 3
  >     }
  > }
  > 
  > fn r(x: Result<Option<I64>, Str>) -> I64 {
  >     match x {
  >         Ok(.Some(v)) => v,
  >         Ok(.None) => 0,
  >         Err(e) => -1
  >     }
  > }
  > 
  > fn main() {
  >     print_i64(area(.Circle(2)))
  >     print_i64(area(.Rect((3, 4))))
  >     print_i64(area(.Rect((3, 0))))
  >     print(classify(.Some((0, "a"))))
  >     print(classify(.Some((-2, "b"))))
  >     print(classify(.None))
  >     print(first(.Some(("x", "y"))))
  >     var out = ""
  >     for i in 1..16 {
  >         if i > 1 { out.push(" ") }
  >         out.push(fizz(i))
  >     }
  >     print(out)
  >     print_i64(depth(.Some(.Some(7))))
  >     print_i64(depth(.Some(.None)))
  >     print_i64(depth(.None))
  >     print_i64(both(true, false))
  >     let q: Option<(Str, I64)> = .Some(("k", 5))
  >     match let q {
  >         .Some((k, 5)) => print("five {k}"),
  >         .Some((k, n)) => print("{k} {n}"),
  >         .None => print("none")
  >     }
  >     match q {
  >         .Some((k, n)) => print(take(k)),
  >         .None => print("none")
  >     }
  >     print_i64(r(Ok(.Some(4))))
  >     print_i64(r(Ok(.None)))
  >     print_i64(r(Err("e")))
  > }
  > EOF
  $ sinterc run ok.sn
  12
  12
  0
  zero a
  neg b
  none
  x
  1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz
  7
  -1
  -2
  2
  five k
  k
  4
  0
  -1

没有覆盖的情况，报出一个例子：

  $ cat > miss.sn <<'EOF'
  > fn f(o: Option<Option<I64>>) -> I64 {
  >     match o {
  >         .Some(.Some(x)) => x,
  >         .None => 0
  >     }
  > }
  > 
  > fn main() { print_i64(f(.None)) }
  > EOF
  $ sinterc miss.sn 2>&1 | head -1
  miss.sn:2:5: 错误：match 没有覆盖所有情况，例如 .Some(.None)

字面量没有写全时要有 _：

  $ cat > lit.sn <<'EOF'
  > fn f(o: Option<I64>) -> I64 {
  >     match o {
  >         .Some(0) => 1,
  >         .None => 0
  >     }
  > }
  > 
  > fn main() { print_i64(f(.None)) }
  > EOF
  $ sinterc lit.sn 2>&1 | head -1
  lit.sn:2:5: 错误：match 没有覆盖所有情况，例如 .Some(_)

前面的分支已经覆盖了后面的：

  $ cat > dead.sn <<'EOF'
  > fn f(o: Option<I64>) -> I64 {
  >     match o {
  >         .Some(x) => x,
  >         .Some(0) => 1,
  >         .None => 0
  >     }
  > }
  > 
  > fn main() { print_i64(f(.None)) }
  > EOF
  $ sinterc dead.sn 2>&1 | head -1
  dead.sn:4:9: 错误：这个分支永远匹配不到（前面的分支已经覆盖了它能匹配的值）

模式的类型不对、同一个变量绑定两次：

  $ cat > ty.sn <<'EOF'
  > fn f(o: Option<I64>) -> I64 {
  >     match o {
  >         .Some("a") => 1,
  >         _ => 0
  >     }
  > }
  > 
  > fn main() { print_i64(f(.None)) }
  > EOF
  $ sinterc ty.sn 2>&1 | head -1
  ty.sn:3:15: 错误：模式 "a" 的类型是 Str，这里的值是 I64

  $ cat > dup.sn <<'EOF'
  > fn f(p: (I64, I64)) -> I64 {
  >     match p {
  >         (x, x) => x
  >     }
  > }
  > 
  > fn main() { print_i64(f((1, 2))) }
  > EOF
  $ sinterc dup.sn 2>&1 | head -1
  dup.sn:3:9: 错误：变量 x 在同一个模式里出现了两次
