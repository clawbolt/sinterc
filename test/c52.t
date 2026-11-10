C52：常用函数。std.str 的 to_upper、to_lower、replace、lines；std.vec 的 range、min、sum_f64、
binary_search、dedup；std.math 的 gcd、lcm；新模块 std.io 的 read_lines、write_lines、input_lines。
都是已有内建函数的组合，不需要新的推理。

  $ cat > ok.sn <<'EOF'
  > use std.str
  > use std.vec
  > use std.math
  > use std.io
  > 
  > fn show(v: Vec<I64>) {
  >     var s = ""
  >     var first = true
  >     for x in v {
  >         if !first { s.push(" ") }
  >         s.push("{x}")
  >         first = false
  >     }
  >     print(s)
  > }
  > 
  > fn main() {
  >     print(str.to_upper("Hello, 世界!"))
  >     print(str.to_lower("ABC def"))
  >     print(str.replace("a-b-c", "-", " + "))
  >     print(str.replace("aaa", "aa", "b"))
  >     for l in str.lines("one\ntwo\r\n\nthree\n") {
  >         print("[{l}]")
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
  >     vec.dedup(d)
  >     show(d)
  >     match vec.min(d) {
  >         .Some(m) => print("最小 {m}"),
  >         .None => print("空")
  >     }
  >     var t = vec.map(vec.range(10), |x| x * 3)
  >     match vec.binary_search(t, 9) {
  >         .Some(i) => print("9 在 {i}"),
  >         .None => print("没有 9")
  >     }
  >     match vec.binary_search(t, 10) {
  >         .Some(i) => print("10 在 {i}"),
  >         .None => print("没有 10")
  >     }
  >     var f: Vec<F64> = vec_new()
  >     f.push(0.5)
  >     f.push(1.25)
  >     f.push(2.0)
  >     print_f64(vec.sum_f64(f))
  >     print_i64(math.gcd(12, -18))
  >     print_i64(math.lcm(4, 6))
  >     print_i64(math.gcd(0, 0))
  >     var ls: Vec<Str> = vec_new()
  >     ls.push("第一行")
  >     ls.push("second")
  >     match io.write_lines("out.txt", ls) {
  >         Ok(_) => print("写好了"),
  >         Err(e) => print(e)
  >     }
  >     match io.read_lines("out.txt") {
  >         Ok(v) => {
  >             print_i64(vec_len(v))
  >             for l in v {
  >                 print(l)
  >             }
  >         },
  >         Err(e) => print(e)
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
  >     print("{vec_len(ls)} 行")
  >     for l in ls {
  >         print("| {l}")
  >     }
  > }
  > EOF
  $ printf 'a\nb b\n\nc' | sinterc run lines.sn
  4 行
  | a
  | b b
  | 
  | c
