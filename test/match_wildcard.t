match 的通配分支 _ 和字面量分支（Lean `Matching.Wildcard`）。
_ 覆盖前面没写到的变体：按移动匹配时载荷在分支结束时析构；按借用匹配时不绑定任何东西。
字面量分支（整数、负整数、true/false、字符串）展开成 if 链，最后必须有 _ 分支。

  $ cat > ok.sn <<'EOF'
  > enum Shape { Circle(I64), Square(I64), Tri(Box<I64>), Dot }
  > 
  > // 按借用匹配（let 参数）：_ 不绑定任何东西
  > fn radius(s: Shape) -> I64 {
  >     match s {
  >         .Circle(r) => r,
  >         _ => 0
  >     }
  > }
  > 
  > // 按移动匹配：.Tri 落进 _ 分支时，它的 Box 在分支结束时析构
  > fn is_dot(sink s: Shape) -> Bool {
  >     match s {
  >         .Dot => true,
  >         _ => false
  >     }
  > }
  > 
  > fn name(n: I64) -> Str {
  >     match n {
  >         0 => "zero",
  >         1 => "one",
  >         -1 => "minus one",
  >         _ => "many"
  >     }
  > }
  > 
  > fn op(s: Str) -> I64 {
  >     match s {
  >         "add" => 1,
  >         "sub" => 2,
  >         _ => 0
  >     }
  > }
  > 
  > fn main() {
  >     let c: Shape = .Circle(5)
  >     let t: Shape = .Tri(box(3))
  >     println("{radius(c)}")
  >     println("{radius(t)}")
  >     println("{is_dot(t)}")
  >     println("{is_dot(.Dot)}")
  >     for i in -1..3 {
  >         println(name(i))
  >     }
  >     println(op("sub").to_str())
  >     println(op("a".concat("dd")).to_str())
  >     println(op("mul").to_str())
  >     let flag = 3 > 2
  >     match flag {
  >         true => println("是"),
  >         _ => println("否")
  >     }
  >     match "s".concat("ub") {
  >         "sub" => println("减"),
  >         _ => println("别的")
  >     }
  >     let o: Option<Str> = .Some("x")
  >     match o {
  >         .None => println("空"),
  >         _ => println("有值")
  >     }
  > }
  > EOF
  $ sinterc run ok.sn
  5
  0
  false
  true
  minus one
  zero
  one
  many
  2
  1
  0
  是
  减
  有值

覆盖检查（`Matching.Wildcard` 的 covers）：_ 多余、_ 不在最后、两个 _，都拒绝：

  $ cat > extra.sn <<'EOF'
  > fn f(o: Option<I64>) -> I64 {
  >     match o {
  >         .Some(x) => x,
  >         .None => 0,
  >         _ => 1
  >     }
  > }
  > fn main() { }
  > EOF
  $ sinterc extra.sn
  extra.sn:5:14: 错误：所有变体都已经写到了，_ 分支是多余的
   5 |         _ => 1
     |              ^
  [1]

  $ cat > order.sn <<'EOF'
  > fn f(o: Option<I64>) -> I64 {
  >     match o {
  >         _ => 1,
  >         .Some(x) => x
  >     }
  > }
  > fn main() { }
  > EOF
  $ sinterc order.sn
  order.sn:3:14: 错误：_ 分支只能放在最后
   3 |         _ => 1,
     |              ^
  [1]

没有 _ 时仍然要写全：

  $ cat > miss.sn <<'EOF'
  > enum E { A, B, C }
  > fn f(e: E) -> I64 {
  >     match e {
  >         .A => 1,
  >         .B => 2
  >     }
  > }
  > fn main() { }
  > EOF
  $ sinterc miss.sn
  miss.sn:3:5: 错误：match 没有覆盖 .C（枚举的 match 必须穷尽）
   3 |     match e {
     |     ^
  [1]

字面量分支：最后要有 _，同一个值不能写两次，分支的模式不能是变量：

  $ cat > lit.sn <<'EOF'
  > fn f(n: I64) -> I64 {
  >     match n {
  >         0 => 1,
  >         1 => 2
  >     }
  > }
  > EOF
  $ sinterc lit.sn
  lit.sn:2:5: 错误：按字面量 match 时最后要有 _ 分支（字面量写不全）
   2 |     match n {
     |     ^
  [1]

  $ cat > twice.sn <<'EOF'
  > fn f(n: I64) -> I64 {
  >     match n {
  >         0 => 1,
  >         0 => 2,
  >         _ => 3
  >     }
  > }
  > EOF
  $ sinterc twice.sn
  twice.sn:4:9: 错误：分支 0 写了两次
   4 |         0 => 2,
     |         ^
  [1]

  $ cat > var.sn <<'EOF'
  > fn f(n: I64) -> I64 {
  >     match n {
  >         x => x
  >     }
  > }
  > EOF
  $ sinterc var.sn
  var.sn:3:9: 错误：分支的模式不能是变量 x（要匹配所有其余情况时写 _）
   3 |         x => x
     |         ^
  [1]
