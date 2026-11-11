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
