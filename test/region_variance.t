含引用的泛型结构体、枚举的协变（Lean `Regions.DataVariance`）。类型参数只出现在只读的位置（字段、载荷直接是它，
或者在 Result、另一个能协变的实例里）时，外层区域的实例可以当作内层的用：Option<&I64 in a> 当作 Option<&I64 in b>。
Box、Vec 里的保守地算不变；inout 实参仍然要求类型完全相同。

  $ cat > ok.sn <<'EOF'
  > struct Pair<A, B> { a: A, b: B }
  > 
  > fn get(o: Option<&I64 in x>) -> I64 {
  >     match o {
  >         .Some(r) => *r,
  >         .None => 0
  >     }
  > }
  > 
  > fn first(p: Pair<&I64 in x, &I64 in x>) -> I64 { *p.a + *p.b }
  > 
  > fn main() {
  >     arena |a| {
  >         let o: Option<&I64 in a> = .Some(alloc(a, 1))
  >         arena |b| {
  >             var cur: Option<&I64 in b> = .Some(alloc(b, 2))
  >             print_i64(get(cur))
  >             cur = o
  >             print_i64(get(cur))
  >             let p = Pair { a: alloc(a, 3), b: alloc(b, 4) }
  >             let q: Pair<&I64 in b, &I64 in b> = p
  >             print_i64(first(q))
  >         }
  >     }
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  2
  1
  7

反方向：内层区域的实例当作外层的用：

  $ cat > contra.sn <<'EOF'
  > struct Pair<A, B> { a: A, b: B }
  > fn main() {
  >     arena |a| {
  >         arena |b| {
  >             let p = Pair { a: alloc(a, 1), b: alloc(b, 2) }
  >             let q: Pair<&I64 in a, &I64 in a> = p
  >         }
  >     }
  > }
  > EOF
  $ sinterc contra.sn
  contra.sn:6:49: 错误：类型不匹配：需要 Pair<&I64 in a, &I64 in a>，实际是 Pair<&I64 in a, &I64 in b>
   6 |             let q: Pair<&I64 in a, &I64 in a> = p
     |                                                 ^
  [1]

类型参数在 Box 里：保守地算不变：

  $ cat > boxed.sn <<'EOF'
  > struct B<T> { x: Box<T> }
  > fn main() {
  >     arena |a| {
  >         let o = B { x: box(alloc(a, 1)) }
  >         arena |b| {
  >             let q: B<&I64 in b> = o
  >         }
  >     }
  > }
  > EOF
  $ sinterc boxed.sn
  boxed.sn:6:35: 错误：类型不匹配：需要 B<&I64 in b>，实际是 B<&I64 in a>
   6 |             let q: B<&I64 in b> = o
     |                                   ^
  [1]
