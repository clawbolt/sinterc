标准库第一版。用 Sinter 写，放在 std/ 下，构建时编进编译器（gen/embed.ml 生成 src/stdlib_src.ml）；
use std.X 引入后用 X.f(…) 访问。核心部分（std/core/）预导入，不用 use；其余的模块是 std.io、std.fs、std.env、std.process、
std.vec、std.math、std.map、std.hashmap、std.bigint。标准库的代码和用户的代码一样检查。

  $ cat > ok.sn <<'EOF'
  > 
  > fn parse(s: Str) -> Result<I64, Str> {
  >     match s.parse_i64() {
  >         .Some(n) => Ok(n),
  >         .None => Err("不是整数：".concat(s))
  >     }
  > }
  > 
  > fn main() {
  >     println("{(3).min(8)}")
  >     println("{(3).max(8)}")
  >     println("{(-5).abs()}")
  >     println("{(15).clamp(0, 10)}")
  >     println("{(2).pow(10)}")
  >     println(max("apple", "banana"))
  >     println("42".parse_i64().unwrap_or(0).to_str())
  >     println("x".parse_i64().unwrap_or(-1).to_str())
  >     println("x".parse_i64().is_none().to_str())
  >     println(parse("7").is_ok().to_str())
  >     println(parse("oops").unwrap_or(0).to_str())
  >     var v: Vec<I64> = vec_new()
  >     v.push(3)
  >     v.push(1)
  >     v.push(4)
  >     println("{v.sum()}")
  >     println("{v.max().unwrap_or(0)}")
  >     println("{v.contains(4)}")
  >     println("{v.index_of(1).unwrap_or(-1)}")
  >     v.reverse()
  >     println("{v[0]}")
  >     let parts = "a, b ,c".split(",")
  >     var trimmed: Vec<Str> = vec_new()
  >     for p in parts {
  >         trimmed.push(p.trim())
  >     }
  >     println(trimmed.join("+"))
  >     println("hello".starts_with("he").to_str())
  >     println("hello".ends_with("lo").to_str())
  >     println("hello".find("ll").unwrap_or(-1).to_str())
  >     println("ab".repeat(3))
  > }
  > EOF
  $ sinterc run ok.sn
  3
  8
  5
  10
  1024
  banana
  42
  -1
  true
  true
  0
  8
  4
  true
  1
  4
  a+b+c
  true
  true
  2
  ababab

标准库里没有的模块：

  $ cat > nostd.sn <<'EOF'
  > use std.nothing
  > fn main() {
  > }
  > EOF
  $ sinterc nostd.sn
  nostd.sn:1:1: 错误：标准库里没有模块 nothing（有：bigint、env、fs、hashmap、io、map、math、process、vec）
   1 | use std.nothing
     | ^
  [1]

只有标准库写成 std.m：

  $ cat > dotted.sn <<'EOF'
  > use foo.bar
  > fn main() {
  > }
  > EOF
  $ sinterc dotted.sn
  dotted.sn:1:1: 错误：只有标准库的模块写成 std.m；自己的模块直接写 use bar
   1 | use foo.bar
     | ^
  [1]

自己的 math.sn 和 std.math 不能同时用：

  $ cat > math.sn <<'EOF'
  > pub fn one() -> I64 { 1 }
  > EOF
  $ cat > both.sn <<'EOF'
  > use math
  > use std.math
  > fn main() {
  > }
  > EOF
  $ sinterc both.sn
  both.sn:2:1: 错误：模块 math 既是 math.sn 又是 std/math.sn，同一个程序里只能用其中一个
   2 | use std.math
     | ^
  [1]
