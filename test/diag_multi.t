一次报多个错误。函数之间互不影响，每个函数报它的第一个错误，按位置排序，最多显示 20 个。
类型检查有错误时只报类型错误；都通过之后，再报所有函数的所有权错误。

  $ cat > types.sn <<'EOF'
  > fn a() -> I64 { true }
  > 
  > fn b() -> Bool {
  >     let x: I64 = false
  >     x == 1
  > }
  > 
  > fn c() -> I64 { 1 }
  > 
  > fn main() {
  >     print_i64(c())
  >     print_bool(3)
  > }
  > EOF
  $ sinterc types.sn
  types.sn:1:17: 错误：类型不匹配：需要 I64，实际是 Bool
   1 | fn a() -> I64 { true }
     |                 ^
  types.sn:4:18: 错误：类型不匹配：需要 I64，实际是 Bool
   4 |     let x: I64 = false
     |                  ^
  types.sn:12:16: 错误：类型不匹配：需要 Bool，实际是 I64
   12 |     print_bool(3)
      |                ^
  [1]

所有权错误：两个函数各报一个。

  $ cat > own.sn <<'EOF'
  > fn take(sink b: Box<I64>) -> I64 { *b }
  > 
  > fn f() {
  >     let b = box(1)
  >     print_i64(take(b))
  >     print_i64(*b)
  > }
  > 
  > fn main() {
  >     let c = box(2)
  >     print_i64(take(c))
  >     print_i64(take(c))
  >     f()
  > }
  > EOF
  $ sinterc own.sn
  own.sn:6:15: 错误：b 已被移动，不能再使用
   6 |     print_i64(*b)
     |               ^
  own.sn:12:20: 错误：c 已被移动，不能再使用
   12 |     print_i64(take(c))
      |                    ^
  [1]

超过 20 个时只显示前 20 个：

  $ for i in $(seq 1 25); do echo "fn f$i() -> I64 { true }"; done > many.sn
  $ echo "fn main() {}" >> many.sn
  $ sinterc many.sn 2>&1 | grep -c "错误："
  20
  $ sinterc many.sn 2>&1 | tail -1
  （还有 5 个错误没有显示）
  $ sinterc --lang=en many.sn 2>&1 | tail -1
  (5 more errors not shown)
