字符 Char 和 UTF-8（Lean `Library.Utf8`）。Char 是一个 Unicode 标量值，可以复制；Str 仍然存 UTF-8 字节。
s.chars() 逐个解码（不合法的字节解成 U+FFFD），s.char_count() 是字符数，s.push_char(c) 按 UTF-8 写进去。
std.char 提供 ASCII 的分类和大小写转换。for 也能直接遍历函数返回的 Vec。

  $ cat > ok.sn <<'EOF'
  > 
  > fn count_digits(s: Str) -> I64 {
  >     var n = 0
  >     for c in s.chars() {
  >         if c.is_digit() { n = n + 1 }
  >     }
  >     n
  > }
  > 
  > fn upper(s: Str) -> Str {
  >     var r = ""
  >     for c in s.chars() {
  >         r.push_char(c.to_upper())
  >     }
  >     r
  > }
  > 
  > fn reverse(s: Str) -> Str {
  >     let cs = s.chars()
  >     var r = ""
  >     var i = vec_len(cs)
  >     while i > 0 {
  >         i = i - 1
  >         r.push_char(vec_get(cs, i))
  >     }
  >     r
  > }
  > 
  > fn squares(n: I64) -> Vec<I64> {
  >     var v: Vec<I64> = vec_new()
  >     for i in 0..n {
  >         v.push(i * i)
  >     }
  >     v
  > }
  > 
  > fn main() {
  >     let s = "héllo, 世界 😀"
  >     println("{str_len(s)}")
  >     println("{s.char_count()}")
  >     for c in "a中😀".chars() {
  >         println("{c.code()}")
  >     }
  >     println("{'中'}")
  >     println(upper("abc-xyz 中文"))
  >     println(reverse("你好，abc"))
  >     println(count_digits("a1b22c333").to_str())
  >     println("{'a' < 'b'}")
  >     println("{'中' == '中'}")
  >     println("{char_code('\t')}")
  >     println("{'Q'.to_lower()}")
  >     match char_from(65) {
  >         .Some(c) => println("{c}"),
  >         .None => println("不是字符")
  >     }
  >     match char_from(55296) {
  >         .Some(c) => println("{c}"),
  >         .None => println("不是字符")
  >     }
  >     match '7'.to_digit() {
  >         .Some(d) => println("{d}"),
  >         .None => println("不是数字")
  >     }
  >     var t = str_from_char('😀')
  >     t.push_char('!')
  >     println(t)
  >     for x in squares(4) {
  >         println("{x}")
  >     }
  > }
  > EOF
  $ sinterc run ok.sn
  19
  11
  97
  20013
  128512
  中
  ABC-XYZ 中文
  cba，好你
  6
  true
  true
  9
  q
  A
  不是字符
  7
  😀!
  0
  1
  4
  9

引号里要正好一个字符：

  $ cat > two.sn <<'EOF'
  > fn main() {
  >     let c = 'ab'
  > }
  > EOF
  $ sinterc two.sn
  two.sn:2:13: 错误：字符字面量的引号里要正好是一个字符（转义只有 \n、\t、\\、\'、\0）
   2 |     let c = 'ab'
     |             ^
  [1]

Char 不是整数，要用 char_code 转换：

  $ cat > mix.sn <<'EOF'
  > fn main() {
  >     let n: I64 = 'a'
  > }
  > EOF
  $ sinterc mix.sn
  mix.sn:2:18: 错误：类型不匹配：需要 I64，实际是 Char
   2 |     let n: I64 = 'a'
     |                  ^
  [1]
