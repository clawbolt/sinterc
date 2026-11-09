C38：panic（Lean M19）。panic(msg) 打印说明后立刻结束进程（退出码 101），不析构任何值、不执行 errdefer、
不要求线性值已被消费；类型是 Never，后面的代码走不到。std.option、std.result 有了 unwrap 和 expect。

  $ cat > ok.sn <<'EOF'
  > use std.option
  > use std.result
  > 
  > fn parse(s: Str) -> Result<I64, Str> {
  >     match str_to_i64(s) {
  >         .Some(n) => Ok(n),
  >         .None => Err(str_concat("不是整数：", s))
  >     }
  > }
  > 
  > fn half(n: I64) -> I64 {
  >     if n % 2 != 0 { panic("不是偶数") }
  >     n / 2
  > }
  > 
  > fn main() {
  >     print_i64(option.unwrap(str_to_i64("42")))
  >     print_i64(result.unwrap(parse("7")))
  >     print_i64(option.expect(str_to_i64("8"), "要一个数"))
  >     print_i64(half(10))
  >     let n: I64 = result.expect(parse("x"), "读配置失败")
  >     print_i64(n)
  > }
  > EOF
  $ sinterc run ok.sn
  42
  7
  8
  5
  panic: 读配置失败
  [101]

panic 不要求消费线性值（这是明确的取舍：线性值的保证只对正常返回和 Err 返回成立）：

  $ cat > lin.sn <<'EOF'
  > linear struct Tx { id: I64 }
  > 
  > fn main() {
  >     let t = Tx { id: 1 }
  >     panic("放弃")
  > }
  > EOF
  $ sinterc run lin.sn
  panic: 放弃
  [101]

没有 panic 时照样报“没有被消费”：

  $ cat > forget.sn <<'EOF'
  > linear struct Tx { id: I64 }
  > 
  > fn main() {
  >     let t = Tx { id: 1 }
  > }
  > EOF
  $ sinterc forget.sn
  forget.sn:4:5: 错误：t 是 linear 值，作用域结束时还没有被消费
  [1]

只有一个分支 panic 时，另一个分支照常检查：

  $ cat > branch.sn <<'EOF'
  > linear struct Tx { id: I64 }
  > 
  > fn main() {
  >     let t = Tx { id: 1 }
  >     if true { panic("放弃") }
  > }
  > EOF
  $ sinterc branch.sn
  branch.sn:4:5: 错误：t 是 linear 值，作用域结束时还没有被消费
  [1]

说明必须是 Str：

  $ cat > arg.sn <<'EOF'
  > fn main() {
  >     panic(3)
  > }
  > EOF
  $ sinterc arg.sn
  arg.sn:2:11: 错误：类型不匹配：需要 Str，实际是 I64
  [1]
