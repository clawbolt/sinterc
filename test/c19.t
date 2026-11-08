C19：区域和泛型（Lean M14）。泛型函数的类型实参可以是引用：实例里的区域名按出现顺序换成 '0、'1…，
就是实例的区域参数，调用处再按实参的区域实例化；Vec 的元素也可以是引用。
用户定义的泛型结构体、枚举（例如 Option<&I64 in a>）还不能含引用，要等到 C20。
inout 实参里的区域不能放宽（不变），否则被调用的函数能往外层的变量里写内层区域的引用。

同一个实例 id<&I64 in '0> 用在两个区域上；pick 按第一个实参推出 T，外层区域的引用当作内层的用；Vec 里存引用：

  $ cat > ok.sn <<'EOF'
  > fn id<T>(sink x: T) -> T { x }
  > 
  > fn pick<T>(c: Bool, sink x: T, sink y: T) -> T {
  >     if c { x } else { y }
  > }
  > 
  > fn sum(v: Vec<&I64 in a>) -> I64 {
  >     var s = 0
  >     var i = 0
  >     while i < v.len() {
  >         s = s + *vec_get(v, i)
  >         i = i + 1
  >     }
  >     s
  > }
  > 
  > fn main() {
  >     arena |a| {
  >         let r = alloc(a, 1)
  >         let r2 = id(r)
  >         print_i64(*r2)
  >         arena |b| {
  >             let s = alloc(b, 10)
  >             print_i64(*pick(true, s, r))
  >             print_i64(*pick(false, s, r))
  >             print_i64(*id(s) + *id(r))
  >         }
  >         var v: Vec<&I64 in a> = vec_new()
  >         v.push(r)
  >         v.push(alloc(a, 2))
  >         v.push(id(alloc(a, 3)))
  >         print_i64(sum(v))
  >     }
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  1
  10
  1
  11
  6

泛型函数返回的内层区域引用赋给外层变量（和 C4 的 outer.sn 对应）：

  $ cat > outer.sn <<'EOF'
  > fn id<T>(sink x: T) -> T { x }
  > fn main() {
  >     arena |a| {
  >         var r = alloc(a, 1)
  >         arena |b| {
  >             r = id(alloc(b, 2))
  >         }
  >         print_i64(*r)
  >     }
  > }
  > EOF
  $ sinterc outer.sn
  outer.sn:6:17: 错误：类型不匹配：需要 &I64 in a，实际是 &I64 in b
  [1]

inout 实参的区域不能放宽：区域 x 按第一个实参取成 b，r 的类型是 &I64 in a，放宽成 &I64 in b 之后 set 就能把 b 里的引用写进 r：

  $ cat > inout.sn <<'EOF'
  > fn set(y: &I64 in x, inout r: &I64 in x) { r = y }
  > fn main() {
  >     arena |a| {
  >         var r = alloc(a, 1)
  >         arena |b| {
  >             set(alloc(b, 2), r)
  >         }
  >         print_i64(*r)
  >     }
  > }
  > EOF
  $ sinterc inout.sn
  inout.sn:6:30: 错误：inout 实参的类型要和参数类型完全相同：需要 &I64 in b，实际是 &I64 in a（区域不能放宽）
  [1]

泛型枚举的类型实参含引用：

  $ cat > opt.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         let o: Option<&I64 in a> = .Some(alloc(a, 1))
  >     }
  > }
  > EOF
  $ sinterc opt.sn
  opt.sn:3:9: 错误：泛型结构体、枚举的类型实参暂不能含引用（&I64 in a，要等到 C20）
  [1]

元素是引用的 Vec 的 pop 会返回 Option<&I64 in a>，也要等到 C20：

  $ cat > pop.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         var v: Vec<&I64 in a> = vec_new()
  >         vec_push(v, alloc(a, 1))
  >         let o = vec_pop(v)
  >     }
  > }
  > EOF
  $ sinterc pop.sn
  pop.sn:5:17: 错误：元素是引用的 Vec 暂不支持 pop（返回的 Option<&I64 in a> 要等到 C20）
  [1]
