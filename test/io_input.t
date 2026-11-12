读取输入和字符串的字节、子串。read_line() 读一行，返回 Option<Str>（读到末尾时 .None，行尾的换行去掉）；
s.parse_i64() 解析整数，格式不对或超出 I64 范围时 .None；s.byte(i) 是第 i 个字节，s.slice(a, b) 是 [a, b) 的子串（新的 Str），
下标越界时 panic。新字符串都是新分配的（Lean `Datatypes.Str`），越界检查和 Vec 一样，不需要新的 Lean 里程碑。

把输入的每一行当作整数加起来，不是整数的行计数：

  $ cat > in.sn <<'EOF'
  > use std.io
  > 
  > fn main() {
  >     var total = 0
  >     var bad = 0
  >     while true {
  >         match io.read_line() {
  >             .Some(l) => match l.parse_i64() {
  >                 .Some(n) => { total = total + n },
  >                 .None => { bad = bad + 1 }
  >             },
  >             .None => { break }
  >         }
  >     }
  >     println("{total}")
  >     println("{bad}")
  > }
  > EOF
  $ printf '10\n20\nabc\n-5\n\n7' | sinterc run in.sn
  32
  2

字节、子串和整数解析的边界：

  $ cat > str.sn <<'EOF'
  > fn show(o: Option<I64>) {
  >     match o {
  >         .Some(n) => println("{n}"),
  >         .None => println("无")
  >     }
  > }
  > 
  > fn main() {
  >     let s = "hello"
  >     println("{s.byte(0)}")
  >     println(s.slice(1, 4))
  >     println("{s.slice(5, 5).len()}")
  >     show("+12".parse_i64())
  >     show("9223372036854775807".parse_i64())
  >     show("9223372036854775808".parse_i64())
  >     show("-9223372036854775808".parse_i64())
  >     show("1x".parse_i64())
  >     println("{s.byte(5)}")
  > }
  > EOF
  $ sinterc run str.sn
  104
  ell
  0
  12
  9223372036854775807
  无
  -9223372036854775808
  无
  panic: 下标越界
  [101]
