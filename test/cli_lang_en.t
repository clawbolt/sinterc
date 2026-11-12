英文的错误信息。--lang=en 或环境变量 SINTER_LANG=en 时，编译错误和运行时的 panic 用英文；默认中文。
编译器内部照常用中文报错，输出前按 src/i18n.ml 的表换成英文，表里没有的信息原样输出中文。

  $ cat > move.sn <<'EOF'
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > fn main() {
  >     let b = box(1)
  >     println("{take(b)}")
  >     println("{*b}")
  > }
  > EOF
  $ sinterc --lang=en run move.sn
  move.sn:5:15: error: b has been moved and cannot be used again
   5 |     println("{*b}")
     |               ^
  [1]

默认还是中文：

  $ sinterc run move.sn
  move.sn:5:15: 错误：b 已被移动，不能再使用
   5 |     println("{*b}")
     |               ^
  [1]

类型错误：

  $ cat > ty.sn <<'EOF'
  > fn main() {
  >     let x: I64 = true
  > }
  > EOF
  $ SINTER_LANG=en sinterc ty.sn
  ty.sn:2:18: error: type mismatch: expected I64, found Bool
   2 |     let x: I64 = true
     |                  ^
  [1]

linear 值没有被消费（信息由几段拼起来，填进去的短语也翻译）：

  $ cat > lin.sn <<'EOF'
  > linear struct T { n: I64 }
  > fn main() {
  >     let t = T { n: 1 }
  > }
  > EOF
  $ SINTER_LANG=en sinterc lin.sn
  lin.sn:3:5: error: t is linear and has not been consumed at the end of its scope
   3 |     let t = T { n: 1 }
     |     ^
  [1]

投影还活着时移走源对象：

  $ cat > lock.sn <<'EOF'
  > struct P { a: Box<I64>, n: I64 }
  > 
  > fn eat(sink p: P) -> I64 { p.n }
  > 
  > fn main() {
  >     let p = P { a: box(1), n: 2 }
  >     let a = p.a
  >     println("{eat(p)}")
  >     println("{*a}")
  > }
  > EOF
  $ SINTER_LANG=en sinterc lock.sn
  lock.sn:8:19: error: p is borrowed by projection a (which is used later), so it cannot be moved
   8 |     println("{eat(p)}")
     |                   ^
  [1]

运行时的 panic：

  $ cat > oob.sn <<'EOF'
  > fn main() {
  >     var w: Vec<I64> = vec_new()
  >     println("{vec_get(w, 3)}")
  > }
  > EOF
  $ SINTER_LANG=en sinterc run oob.sn
  panic: index out of bounds
  [101]
