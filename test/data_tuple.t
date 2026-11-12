元组 (A, B)。它是内建的泛型结构体（字段名 0、1、…），检查规则和证明都沿用泛型结构体（Lean `Datatypes.Enum`、`Ownership.Partial`），不需要新的推理。
写法：(a, b) 构造，t.0 取元素，t.0 = e 给元素赋值，let (x, y) = e 拆开；类型写成 (A, B)，可以嵌套、作返回值、作泛型实参。
元素都可以复制时元组也可以复制，否则只能移动。2 到 6 个元素。

  $ cat > ok.sn <<'EOF'
  > fn divmod(a: I64, b: I64) -> (I64, I64) {
  >     (a / b, a % b)
  > }
  > 
  > fn swap<A, B>(sink p: (A, B)) -> (B, A) {
  >     let (a, b) = p
  >     (b, a)
  > }
  > 
  > fn main() {
  >     let p = (1, true)
  >     println("{p.0}")
  >     println("{p.1}")
  >     let (q, r) = divmod(17, 5)
  >     println("{q * 10 + r}")
  >     // 元素都可以复制时，元组也可以复制
  >     let a = (3, 4)
  >     let b = a
  >     println("{a.0 + b.1}")
  >     var v = ((1, 2), 3)
  >     v.0.1 = 20
  >     v.1 = 30
  >     println("{v.0.0 + v.0.1 + v.1}")
  >     let s = swap(("左", 5))
  >     println("{s.0}")
  >     println(s.1)
  >     var names: Vec<(Str, I64)> = Vec.new()
  >     names.push(("ann", 31))
  >     names.push(("bob", 27))
  >     for x in names {
  >         println(x.0.concat(x.1.to_str()))
  >     }
  >     let (n, age) = swap((40, "cy"))
  >     println(n)
  >     println("{age}")
  > }
  > EOF
  $ sinterc run ok.sn
  1
  true
  32
  7
  51
  5
  左
  ann31
  bob27
  cy
  40

含 Str 的元组只能移动：

  $ cat > moved.sn <<'EOF'
  > fn main() {
  >     let s = ("a", 1)
  >     let t = s
  >     println(s.0)
  > }
  > EOF
  $ sinterc moved.sn
  moved.sn:4:13: 错误：s 已被移动，不能再使用
   4 |     println(s.0)
     |             ^
  [1]

拆开时元素个数要对上：

  $ cat > arity.sn <<'EOF'
  > fn main() {
  >     let (a, b) = (1, 2, 3)
  > }
  > EOF
  $ sinterc arity.sn
  arity.sn:2:18: 错误：这里要拆开的是 2 个元素的元组，实际是 (I64, I64, I64)
   2 |     let (a, b) = (1, 2, 3)
     |                  ^
  [1]

最多 6 个元素：

  $ cat > big.sn <<'EOF'
  > fn main() {
  >     let t = (1, 2, 3, 4, 5, 6, 7)
  > }
  > EOF
  $ sinterc big.sn
  big.sn:2:13: 错误：元组最多 6 个元素（这里有 7 个）
   2 |     let t = (1, 2, 3, 4, 5, 6, 7)
     |             ^
  [1]
