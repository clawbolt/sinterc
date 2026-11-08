C8：泛型（Lean M9（一））。泛型函数 fn f<T, U: linear>(…)、泛型结构体和枚举，类型实参从实参和期望的类型推出来。
定义处按抽象类型检查：T 的值只能移动，普通的 T 自动析构，T: linear 必须交出去，不能对 T 做运算；
调用处检查约束（普通的 T 不能是 linear 类型）；用到的每个实例生成一份具体代码，照常检查。

Option<T>（内建）、Pair<A, B>，实例有 I64、Bool、Box<I64>、linear 结构体；Box 计入泄漏检查：

  $ cat > ok.sn <<'EOF'
  > struct Pair<A, B> { a: A, b: B }
  > 
  > linear struct Token { id: I64 }
  > 
  > fn some<T>(sink x: T) -> Option<T> { .Some(x) }
  > 
  > fn unwrap_or<T>(sink o: Option<T>, sink d: T) -> T {
  >     match o {
  >         .Some(x) => x,
  >         .None => d
  >     }
  > }
  > 
  > fn pair<A, B>(sink a: A, sink b: B) -> Pair<A, B> {
  >     Pair { a: a, b: b }
  > }
  > 
  > fn swap<A, B>(sink a: A, sink b: B) -> Pair<B, A> {
  >     pair(b, a)
  > }
  > 
  > fn pass<T: linear>(sink x: T) -> T { x }
  > 
  > fn close(sink t: Token) -> I64 {
  >     let n = t.id
  >     consume(t)
  >     n
  > }
  > 
  > fn main() {
  >     print_i64(unwrap_or(some(1), 2))
  >     let none: Option<I64> = .None
  >     print_i64(unwrap_or(none, 3))
  >     let b = unwrap_or(some(box(4)), box(5))
  >     print_i64(*b)
  >     let p = swap(true, 6)
  >     print_i64(p.a)
  >     print_bool(p.b)
  >     let q = pair(box(7), 8)
  >     print_i64(*q.a)
  >     let t = pass(Token { id: 9 })
  >     print_i64(close(t))
  >     let u = pass(10)
  >     print_i64(u)
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  1
  3
  4
  6
  true
  7
  9
  10

普通的 T 不能传 linear 类型（Lean M9 的 Respects）：

  $ cat > bound.sn <<'EOF'
  > linear struct Token { id: I64 }
  > fn keep<T>(sink x: T) {
  > }
  > fn main() {
  >     let t = Token { id: 1 }
  >     keep(t)
  > }
  > EOF
  $ sinterc bound.sn
  bound.sn:6:5: 错误：keep 的类型参数 T 没有标 linear，不能是 linear 类型 Token（要写 T: linear）
  [1]

定义处按抽象类型检查：即使只用 I64 实例化（可以复制），T 的值也只能移动一次：

  $ cat > twice.sn <<'EOF'
  > fn twice<T>(sink x: T) -> T {
  >     let y = x
  >     x
  > }
  > fn main() {
  >     print_i64(twice(1))
  > }
  > EOF
  $ sinterc twice.sn
  twice.sn:3:5: 错误：x 已被移动，不能再使用
  [1]

T: linear 的值必须交出去，即使只用 I64 实例化：

  $ cat > hold.sn <<'EOF'
  > fn hold<T: linear>(sink x: T) {
  >     let y = x
  > }
  > fn main() {
  >     hold(1)
  > }
  > EOF
  $ sinterc hold.sn
  hold.sn:2:5: 错误：y 是 linear 值，作用域结束时还没有被消费
  [1]

不能对 T 做运算（要等以后有了 trait）：

  $ cat > ops.sn <<'EOF'
  > fn same<T>(a: T, b: T) -> Bool {
  >     a == b
  > }
  > fn main() {
  >     print_bool(same(1, 2))
  > }
  > EOF
  $ sinterc ops.sn
  ops.sn:2:5: 错误：只能比较 I64 和 Bool
  [1]
