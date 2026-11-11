C61：静态的字符串字面量（Lean M27 的三）。字面量不再每次分配：它指向程序里的字符串常量，
复制还是字面量，析构什么都不做；要修改（push、push_char、Bytes 的 set、push）时先复制到堆上。
程序结束时的泄漏检查照常：堆上的字符串都释放了，也没有去释放字面量（否则 free 会出错）。

  $ cat > lit.sn <<'EOF'
  > fn tag(n: I64) -> Str {
  >     match n {
  >         0 => "zero",
  >         _ => "many"
  >     }
  > }
  > fn main() {
  >     let a = "abc"
  >     var b = a.clone()
  >     b.push("d")
  >     b.push_char('e')
  >     print(a)
  >     print(b)
  >     var v: Vec<Str> = vec_new()
  >     for i in 0..3 {
  >         v.push("x")
  >     }
  >     v[1].push("y")
  >     print("{v[0]} {v[1]} {v[2]}")
  >     var s = ""
  >     for i in 0..3 {
  >         s.push(tag(i))
  >     }
  >     print(s)
  >     var bs = "hi".to_bytes()
  >     bs.set(0, 72)
  >     bs.push(33)
  >     print_bytes(bs)
  >     print("")
  >     var e = bytes_new()
  >     e.push(65)
  >     print_bytes(e)
  >     print("")
  >     let c = a.clone()
  >     print(c)
  > }
  > EOF
  $ sinterc run lit.sn
  abc
  abcde
  x xy x
  zeromanymany
  Hi!
  A
  abc

生成的 C 里字面量直接指向字符串常量：

  $ sinterc lit.sn -o lit.c && grep -c "r.lit = true" lit.c
  1

字符串切片 &str in a（Lean M27 的一、二）：text(a, s) 把字符串放进区域 a，子切片、split、lines 都不分配。
examples/ok/c61_view.sn 是完整一些的例子（validate.sh 把它导出给 Lean 复核）。

  $ cat > csv.sn <<'EOF'
  > use std.view
  > 
  > fn sum_col(data: &str in r, col: I64) -> I64 {
  >     var total = 0
  >     for line in view.lines(data) {
  >         let fields = view.split(line, ",")
  >         match view.to_i64(view.trim(fields[col])) {
  >             .Some(x) => { total = total + x },
  >             .None => {}
  >         }
  >     }
  >     total
  > }
  > 
  > fn main() {
  >     arena |a| {
  >         var s = "id,n\n"
  >         for i in 0..1000 {
  >             s.push("{i}, {i * 2}\n")
  >         }
  >         let data = text(a, s)
  >         print_i64(sum_col(data, 0))
  >         print_i64(sum_col(data, 1))
  >         let ls = view.lines(data)
  >         let first = ls[1]
  >         first.print_str()
  >         print("")
  >         print(first.sub(3, first.len()).to_str())
  >     }
  > }
  > EOF
  $ sinterc run csv.sn
  499500
  999000
  0, 0
  0

子切片越界时 panic：

  $ cat > oob.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         let v = text(a, "abc")
  >         print(v.sub(1, 4).to_str())
  >     }
  > }
  > EOF
  $ sinterc run oob.sn
  panic: 下标越界
  [101]

切片活不过它的区域：内层区域的切片不能放进外层区域的 Vec：

  $ cat > escape.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         var keep: Vec<&str in a> = vec_new()
  >         arena |b| {
  >             let v = text(b, "abc")
  >             keep.push(v)
  >         }
  >         print_i64(keep.len())
  >     }
  > }
  > EOF
  $ sinterc escape.sn
  escape.sn:6:23: 错误：类型不匹配：需要 &str in a，实际是 &str in b
   6 |             keep.push(v)
     |                       ^
  [1]

切片不能解引用；text 的第一个参数必须是区域；类型只有 &str in 区域：

  $ cat > deref.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         let v = text(a, "abc")
  >         let s = *v
  >     }
  > }
  > EOF
  $ sinterc deref.sn
  deref.sn:4:17: 错误：字符串切片不能解引用（要 Str 时用 to_str() 复制一份）
   4 |         let s = *v
     |                 ^
  [1]
  $ cat > notarena.sn <<'EOF'
  > fn main() {
  >     let v = text(1, "abc")
  > }
  > EOF
  $ sinterc notarena.sn
  notarena.sn:2:18: 错误：text 的第一个参数必须是区域，I64 不是
   2 |     let v = text(1, "abc")
     |                  ^
  [1]
  $ cat > badty.sn <<'EOF'
  > fn f(v: &string in a) -> I64 { 0 }
  > fn main() {}
  > EOF
  $ sinterc badty.sn
  badty.sn:1:10: 错误：未知类型 string（字符串切片写 &str in 区域）
   1 | fn f(v: &string in a) -> I64 { 0 }
     |          ^
  [1]
