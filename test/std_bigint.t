std.bigint（Lean `Library.BigInt`）。符号加上从低位到高位的各位（每位小于 10^9）；加、减、乘、比较、除以小数
和 `Library.BigInt` 的算法一一对应，`Library.BigInt` 证明了它们算出的整数是对的、结果的每一位仍小于 10^9。

  $ cat > ok.sn <<'EOF'
  > use std.bigint
  > use std.math
  > use std.option
  > use std.vec
  > 
  > fn big(s: Str) -> bigint.BigInt { option.unwrap(bigint.parse(s)) }
  > 
  > fn show(x: bigint.BigInt) { print(bigint.to_str(x)) }
  > 
  > fn fact(n: I64) -> bigint.BigInt {
  >     var r = bigint.from_i64(1)
  >     for i in 1..n + 1 {
  >         r = bigint.mul(r, bigint.from_i64(i))
  >     }
  >     r
  > }
  > 
  > fn fib(n: I64) -> bigint.BigInt {
  >     var a = bigint.zero()
  >     var b = bigint.from_i64(1)
  >     for i in 0..n {
  >         let c = bigint.add(a, b)
  >         a = b
  >         b = c
  >     }
  >     a
  > }
  > 
  > fn next(inout s: I64) -> I64 {
  >     s = (s * 1103515245 + 12345) % 2147483648
  >     s / 65536
  > }
  > 
  > fn main() {
  >     show(fact(30))
  >     show(bigint.pow(bigint.from_i64(2), 100))
  >     show(fib(200))
  >     let mn = bigint.from_i64(-9223372036854775807 - 1)
  >     show(mn)
  >     show(bigint.sub(mn, bigint.from_i64(1)))
  >     show(bigint.sub(big("1000000000000000000000"), bigint.from_i64(1)))
  >     show(bigint.add(bigint.from_i64(-5), bigint.from_i64(3)))
  >     let z = bigint.add(bigint.from_i64(5), bigint.from_i64(-5))
  >     print("{bigint.to_str(z)} {bigint.sign(z)}")
  >     show(bigint.mul(bigint.from_i64(-12), bigint.from_i64(12)))
  >     show(big("+000123"))
  >     print_bool(option.is_none(bigint.parse("12a")))
  >     print_bool(option.is_none(bigint.parse("-")))
  >     let f = fact(30)
  >     let (q, r) = bigint.divmod_small(f, 7)
  >     show(q)
  >     print_i64(r)
  >     let back = bigint.add(bigint.mul(q, bigint.from_i64(7)), bigint.from_i64(r))
  >     print_bool(eq(back, f))
  >     let (q2, r2) = bigint.divmod_small(bigint.from_i64(-7), 2)
  >     print("{bigint.to_str(q2)} {r2}")
  >     var v: Vec<bigint.BigInt> = vec_new()
  >     v.push(bigint.from_i64(5))
  >     v.push(bigint.from_i64(-3))
  >     v.push(bigint.pow(bigint.from_i64(10), 20))
  >     v.push(bigint.zero())
  >     v.push(bigint.neg(bigint.pow(bigint.from_i64(10), 20)))
  >     vec.sort(v)
  >     for x in v { show(x) }
  >     // 和 I64 对照：2000 组随机数，乘积不超过 10^18
  >     var seed = 11
  >     var bad = 0
  >     for k in 0..2000 {
  >         let a = next(seed) * 30000 - next(seed) * 30000
  >         let b = next(seed) * 30000 - next(seed)
  >         let x = bigint.from_i64(a)
  >         let y = bigint.from_i64(b)
  >         if !str_eq(bigint.to_str(bigint.add(x, y)), str_from_i64(a + b)) { bad = bad + 1 }
  >         if !str_eq(bigint.to_str(bigint.sub(x, y)), str_from_i64(a - b)) { bad = bad + 1 }
  >         if !str_eq(bigint.to_str(bigint.mul(x, y)), str_from_i64(a * b)) { bad = bad + 1 }
  >         if cmp(x, y) != cmp(a, b) { bad = bad + 1 }
  >         let (dq, dr) = bigint.divmod_small(x, 7)
  >         if !str_eq(bigint.to_str(dq), str_from_i64(a / 7)) || dr != a % 7 { bad = bad + 1 }
  >         let p = bigint.parse(str_from_i64(b))
  >         if !eq(option.unwrap(p), y) { bad = bad + 1 }
  >     }
  >     print_i64(bad)
  > }
  > EOF
  $ sinterc run ok.sn
  265252859812191058636308480000000
  1267650600228229401496703205376
  280571172992510140037611932413038677189525
  -9223372036854775808
  -9223372036854775809
  999999999999999999999
  -2
  0 0
  -144
  123
  true
  true
  37893265687455865519472640000000
  0
  true
  -3 -1
  -100000000000000000000
  -3
  0
  5
  100000000000000000000
  0

除数不在 1 到 10^9 之间时 panic：

  $ cat > div0.sn <<'EOF'
  > use std.bigint
  > 
  > fn main() {
  >     let (q, r) = bigint.divmod_small(bigint.from_i64(1), 0)
  > }
  > EOF
  $ sinterc run div0.sn
  panic: bigint.divmod_small 的除数要在 1 到 1000000000 之间
  [101]
