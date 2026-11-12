标准库第一版。用 Sinter 写，放在 std/ 下，构建时编进编译器（gen/embed.ml 生成 src/stdlib_src.ml）；
use std.math 引入后照样用 math.min(…) 访问。有 std.math（min、max、abs、sign、clamp、pow，trait Eq、Ord 和
I64、Bool、Str 的实现，min_of、max_of）、std.option、std.result、std.vec（sum、max、contains、index_of、reverse）、
std.str（starts_with、ends_with、find、contains、trim、split、join、repeat）。标准库的代码和用户的代码一样检查。

  $ cat > ok.sn <<'EOF'
  > use std.math
  > use std.option
  > use std.result
  > use std.vec
  > use std.str
  > 
  > fn parse(s: Str) -> Result<I64, Str> {
  >     match str_to_i64(s) {
  >         .Some(n) => Ok(n),
  >         .None => Err(str_concat("不是整数：", s))
  >     }
  > }
  > 
  > fn main() {
  >     print_i64(math.min(3, 8))
  >     print_i64(math.max(3, 8))
  >     print_i64(math.abs(-5))
  >     print_i64(math.clamp(15, 0, 10))
  >     print_i64(math.pow(2, 10))
  >     print(math.max_of("apple", "banana"))
  >     print_i64(option.unwrap_or(str_to_i64("42"), 0))
  >     print_i64(option.unwrap_or(str_to_i64("x"), -1))
  >     print_bool(option.is_none(str_to_i64("x")))
  >     print_bool(result.is_ok(parse("7")))
  >     print_i64(result.unwrap_or(parse("oops"), 0))
  >     var v: Vec<I64> = vec_new()
  >     v.push(3)
  >     v.push(1)
  >     v.push(4)
  >     print_i64(vec.sum(v))
  >     print_i64(option.unwrap_or(vec.max(v), 0))
  >     print_bool(vec.contains(v, 4))
  >     print_i64(option.unwrap_or(vec.index_of(v, 1), -1))
  >     vec.reverse(v)
  >     print_i64(vec_get(v, 0))
  >     let parts = str.split("a, b ,c", ",")
  >     var trimmed: Vec<Str> = vec_new()
  >     for p in parts {
  >         trimmed.push(str.trim(p))
  >     }
  >     print(str.join(trimmed, "+"))
  >     print_bool(str.starts_with("hello", "he"))
  >     print_bool(str.ends_with("hello", "lo"))
  >     print_i64(option.unwrap_or(str.find("hello", "ll"), -1))
  >     print(str.repeat("ab", 3))
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
  nostd.sn:1:1: 错误：标准库里没有模块 nothing（有：bigint、char、env、fs、hashmap、io、map、math、option、os、process、result、str、treemap、vec、view）
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
