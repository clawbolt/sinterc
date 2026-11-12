浮点数 F64。可以复制，不涉及所有权，不需要新的推理。
字面量 1.5、2.0e-3、1e9；+ - * / 和比较要求两边都是 F64（不做隐式转换，用 to_f64、to_i64）；% 只用于 I64。
按 IEEE 754：不检查溢出和除以零（得到 inf、NaN）。"{x}" / x.to_str() 打印能还原成同一个值的最短写法，整数值带 .0。

  $ cat > ok.sn <<'EOF'
  > use std.math
  > 
  > fn area(r: F64) -> F64 {
  >     math.pi() * r * r
  > }
  > 
  > fn mean(v: Vec<F64>) -> F64 {
  >     var s = 0.0
  >     for x in v {
  >         s = s + x
  >     }
  >     s / to_f64(vec_len(v))
  > }
  > 
  > fn main() {
  >     println("{0.1 + 0.2}")
  >     println("{2.0}")
  >     println("{1.5e3}")
  >     println("{-0.25}")
  >     println("{area(2.0)}")
  >     var v: Vec<F64> = vec_new()
  >     v.push(1.0)
  >     v.push(2.5)
  >     v.push(4.0)
  >     println("{mean(v)}")
  >     println("{(2.0).sqrt()}")
  >     println("{to_i64(3.99)}")
  >     println("{to_i64(-3.99)}")
  >     println("{(2.5).round()}")
  >     println("{(-1.5).floor()}")
  >     println("{(2.0).powi(-3)}")
  >     println("{0.1 + 0.2 == 0.3}")
  >     println("{1.0 < 2.0}")
  >     println(str_concat("x = ", str_from_f64(1.0 / 3.0)))
  >     match "2.75".to_f64() {
  >         .Some(x) => println("{x * 2.0}"),
  >         .None => println("不是数")
  >     }
  >     match str_to_f64("abc") {
  >         .Some(x) => println("{x}"),
  >         .None => println("不是数")
  >     }
  >     // t.0.1 仍然是取嵌套元组的元素，不是浮点数 0.1
  >     let t = ((1, 2), 3)
  >     println("{t.0.1}")
  >     let z = 0.0
  >     println("{(z / z).is_nan()}")
  >     println("{1.0 / z}")
  >     println("{1e20}")
  > }
  > EOF
  $ sinterc run ok.sn
  0.30000000000000004
  2.0
  1500.0
  -0.25
  12.566370614359172
  2.5
  1.4142135623730951
  3
  -3
  3.0
  -2.0
  0.125
  false
  true
  x = 0.3333333333333333
  5.5
  不是数
  2
  true
  inf
  1e+20

不做隐式转换：

  $ cat > mix.sn <<'EOF'
  > fn main() {
  >     let x = 1.0 + 2
  > }
  > EOF
  $ sinterc mix.sn
  mix.sn:2:19: 错误：类型不匹配：需要 F64，实际是 I64
   2 |     let x = 1.0 + 2
     |                   ^
  [1]

% 只用于整数：

  $ cat > rem.sn <<'EOF'
  > fn main() {
  >     let y = 5.0 % 2.0
  > }
  > EOF
  $ sinterc rem.sn
  rem.sn:2:13: 错误：类型不匹配：需要 I64，实际是 F64
   2 |     let y = 5.0 % 2.0
     |             ^
  [1]

F64 转换成 I64 时超出范围会 panic：

  $ cat > big.sn <<'EOF'
  > fn main() {
  >     println("{to_i64(1e30)}")
  > }
  > EOF
  $ sinterc run big.sn
  panic: F64 转换成 I64 时超出范围（或是 NaN）
  [101]
