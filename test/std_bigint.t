std.bigint（Lean `Library.BigInt`）。符号加上从低位到高位的各位（每位小于 10^9）；加、减、乘、比较、除以小数
和 `Library.BigInt` 的算法一一对应，`Library.BigInt` 证明了它们算出的整数是对的、结果的每一位仍小于 10^9。

  $ cat > ok.sn <<'EOF'
  > use std.bigint
  > 
  > fn big(s: Str) -> bigint.BigInt { bigint.parse(s).unwrap() }
  > 
  > fn show(x: bigint.BigInt) { println(x.to_str()) }
  > 
  > fn fact(n: I64) -> bigint.BigInt {
  >     var r = bigint.from_i64(1)
  >     for i in 1..n + 1 {
  >         r = r.mul(bigint.from_i64(i))
  >     }
  >     r
  > }
  > 
  > fn fib(n: I64) -> bigint.BigInt {
  >     var a = bigint.zero()
  >     var b = bigint.from_i64(1)
  >     for i in 0..n {
  >         let c = a.add(b)
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
  >     show(bigint.from_i64(2).pow(100))
  >     show(fib(200))
  >     let mn = bigint.from_i64(-9223372036854775807 - 1)
  >     show(mn)
  >     show(mn.sub(bigint.from_i64(1)))
  >     show(big("1000000000000000000000").sub(bigint.from_i64(1)))
  >     show(bigint.from_i64(-5).add(bigint.from_i64(3)))
  >     let z = bigint.from_i64(5).add(bigint.from_i64(-5))
  >     println("{z.to_str()} {z.sign()}")
  >     show(bigint.from_i64(-12).mul(bigint.from_i64(12)))
  >     show(big("+000123"))
  >     println(bigint.parse("12a").is_none().to_str())
  >     println(bigint.parse("-").is_none().to_str())
  >     let f = fact(30)
  >     let (q, r) = f.divmod_small(7)
  >     show(q)
  >     println("{r}")
  >     let back = q.mul(bigint.from_i64(7)).add(bigint.from_i64(r))
  >     println("{back.eq(f)}")
  >     let (q2, r2) = bigint.from_i64(-7).divmod_small(2)
  >     println("{q2.to_str()} {r2}")
  >     var v: Vec<bigint.BigInt> = vec_new()
  >     v.push(bigint.from_i64(5))
  >     v.push(bigint.from_i64(-3))
  >     v.push(bigint.from_i64(10).pow(20))
  >     v.push(bigint.zero())
  >     v.push(bigint.from_i64(10).pow(20).neg())
  >     v.sort()
  >     for x in v { show(x) }
  >     // 和 I64 对照：2000 组随机数，乘积不超过 10^18
  >     var seed = 11
  >     var bad = 0
  >     for k in 0..2000 {
  >         let a = next(seed) * 30000 - next(seed) * 30000
  >         let b = next(seed) * 30000 - next(seed)
  >         let x = bigint.from_i64(a)
  >         let y = bigint.from_i64(b)
  >         if !x.add(y).to_str().eq((a + b).to_str()) { bad = bad + 1 }
  >         if !x.sub(y).to_str().eq((a - b).to_str()) { bad = bad + 1 }
  >         if !x.mul(y).to_str().eq((a * b).to_str()) { bad = bad + 1 }
  >         if x.cmp(y).to_i64() != a.cmp(b).to_i64() { bad = bad + 1 }
  >         let (dq, dr) = x.divmod_small(7)
  >         if !dq.to_str().eq((a / 7).to_str()) || dr != a % 7 { bad = bad + 1 }
  >         let p = bigint.parse(b.to_str())
  >         if !p.unwrap().eq(y) { bad = bad + 1 }
  >     }
  >     println("{bad}")
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
  >     let (q, r) = bigint.from_i64(1).divmod_small(0)
  > }
  > EOF
  $ sinterc run div0.sn
  panic: BigInt 的 divmod_small 的除数要在 1 到 1000000000 之间
  [101]
