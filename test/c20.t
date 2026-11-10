C20：含引用的泛型结构体、枚举（Lean M15）。Option<&I64 in a>、Pair<&I64 in x, &I64 in y> 这样的类型里的区域
都算进类型的区域：块的值不能带出区域（R3），函数的区域参数也从这里收集，调用处按实参换名。
只差区域的实例在 C 里是同一个类型；元素是引用的 Vec 也可以 pop 了。

  $ cat > ok.sn <<'EOF'
  > struct Pair<A, B> { a: A, b: B }
  > 
  > fn some<T>(sink x: T) -> Option<T> { .Some(x) }
  > 
  > fn get_or(o: Option<&I64 in x>, d: I64) -> I64 {
  >     match o {
  >         .Some(r) => *r,
  >         .None => d
  >     }
  > }
  > 
  > fn sum(p: Pair<&I64 in x, &I64 in y>) -> I64 { *p.a + *p.b }
  > 
  > fn main() {
  >     arena |a| {
  >         let r = alloc(a, 1)
  >         print_i64(get_or(some(r), 0))
  >         let n: Option<&I64 in a> = .None
  >         print_i64(get_or(n, -1))
  >         arena |b| {
  >             let s = alloc(b, 10)
  >             print_i64(sum(Pair { a: r, b: s }))
  >             print_i64(get_or(some(s), 0))
  >         }
  >         var v: Vec<&I64 in a> = vec_new()
  >         v.push(r)
  >         v.push(alloc(a, 5))
  >         match v.pop() {
  >             .Some(t) => print_i64(*t),
  >             .None => print_i64(0)
  >         }
  >         print_i64(vec_len(v))
  >     }
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  1
  -1
  11
  10
  5
  1

块的值是 Option<&I64 in b>，带出区域 b（R3；Lean M15 的 bad1：漏掉类型实参就会接受它）：

  $ cat > escape.sn <<'EOF'
  > fn some<T>(sink x: T) -> Option<T> { .Some(x) }
  > fn main() {
  >     let o = arena |b| {
  >         some(alloc(b, 1))
  >     }
  > }
  > EOF
  $ sinterc escape.sn
  escape.sn:4:9: 错误：块的值的类型 Option<&I64 in b> 含有区域 b，不能带出这个区域（R3）
   4 |         some(alloc(b, 1))
     |         ^
  [1]

内层区域的引用装进外层的 Option：

  $ cat > outer.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         var o: Option<&I64 in a> = .None
  >         arena |b| {
  >             o = .Some(alloc(b, 2))
  >         }
  >         match o {
  >             .Some(r) => print_i64(*r),
  >             .None => print_i64(0)
  >         }
  >     }
  > }
  > EOF
  $ sinterc outer.sn
  outer.sn:5:23: 错误：类型不匹配：需要 &I64 in a，实际是 &I64 in b
   5 |             o = .Some(alloc(b, 2))
     |                       ^
  [1]
