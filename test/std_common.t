常用函数。Str 的 to_upper、to_lower、replace、lines；Vec 的 min、sum、binary_search、dedup 和 std.vec 的 range；
I64 的 gcd、lcm；std.fs 的 read_lines、write_lines 和 std.io 的 input_lines。
都是已有内建函数的组合，不需要新的推理。

  $ cat > ok.sn <<'EOF'
  > use std.vec
  > use std.fs
  > 
  > fn show(v: Vec<I64>) {
  >     var s = ""
  >     var first = true
  >     for x in v {
  >         if !first { s.push(" ") }
  >         s.push("{x}")
  >         first = false
  >     }
  >     println(s)
  > }
  > 
  > fn main() {
  >     println("Hello, 世界!".to_upper())
  >     println("ABC def".to_lower())
  >     println("a-b-c".replace("-", " + "))
  >     println("aaa".replace("aa", "b"))
  >     for l in "one\ntwo\r\n\nthree\n".lines() {
  >         println("[{l}]")
  >     }
  >     show(vec.range(5))
  >     var d: Vec<I64> = vec_new()
  >     d.push(1)
  >     d.push(1)
  >     d.push(2)
  >     d.push(2)
  >     d.push(2)
  >     d.push(3)
  >     d.push(1)
  >     d.push(1)
  >     d.dedup()
  >     show(d)
  >     match d.min() {
  >         .Some(m) => println("最小 {m}"),
  >         .None => println("空")
  >     }
  >     var t = vec.range(10).map(|x| x * 3)
  >     match t.binary_search(9) {
  >         .Some(i) => println("9 在 {i}"),
  >         .None => println("没有 9")
  >     }
  >     match t.binary_search(10) {
  >         .Some(i) => println("10 在 {i}"),
  >         .None => println("没有 10")
  >     }
  >     var f: Vec<F64> = vec_new()
  >     f.push(0.5)
  >     f.push(1.25)
  >     f.push(2.0)
  >     println("{f.sum()}")
  >     println("{(12).gcd(-18)}")
  >     println("{(4).lcm(6)}")
  >     println("{(0).gcd(0)}")
  >     var ls: Vec<Str> = vec_new()
  >     ls.push("第一行")
  >     ls.push("second")
  >     match fs.write_lines("out.txt", ls) {
  >         Ok(_) => println("写好了"),
  >         Err(e) => println(e)
  >     }
  >     match fs.read_lines("out.txt") {
  >         Ok(v) => {
  >             println("{vec_len(v)}")
  >             for l in v {
  >                 println(l)
  >             }
  >         },
  >         Err(e) => println(e)
  >     }
  > }
  > EOF
  $ sinterc run ok.sn
  HELLO, 世界!
  abc def
  a + b + c
  ba
  [one]
  [two]
  []
  [three]
  0 1 2 3 4
  1 2 3 1
  最小 1
  9 在 3
  没有 10
  3.75
  6
  12
  0
  写好了
  2
  第一行
  second
  $ cat out.txt
  第一行
  second

按行读标准输入：

  $ cat > lines.sn <<'EOF'
  > use std.io
  > 
  > fn main() {
  >     let ls = io.input_lines()
  >     println("{vec_len(ls)} 行")
  >     for l in ls {
  >         println("| {l}")
  >     }
  > }
  > EOF
  $ printf 'a\nb b\n\nc' | sinterc run lines.sn
  4 行
  | a
  | b b
  | 
  | c
