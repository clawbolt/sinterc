迭代器（Lean `Regions.DataGeneric`、`Projection.Return`）。for x in e：e 是 Vec 时按下标走；否则 e 是迭代器，
展开成 var #it = e，再反复 #it.next()，直到 .None。s.chars() 逐个解码，不分配；m.iter() 给出键和值的借用，
迭代器活着时 m 被锁住。迭代器是存着借用的结构体（Chars<&str in s>、MapIter<&Map<K, V> in m, &K in m>），
它的 next 是签名里带区域参数的泛型函数。

  $ cat > ok.sn <<'EOF'
  > use std.map
  > use std.hashmap
  > 
  > fn main() {
  >     var n = 0
  >     for c in "a中😀b".chars() {
  >         n = n + 1
  >         if c == 'b' { println("b 是第 {n} 个") }
  >     }
  >     let cs = "xyz".chars().collect()
  >     println("{cs.len()} {cs[2]}")
  >     var m: map.Map<I64, Str> = map.Map.new()
  >     let a = m.insert(3, "c")
  >     let b = m.insert(1, "a")
  >     let d = m.insert(2, "b")
  >     for e in m.iter() {
  >         let (k, v) = e
  >         println("{*k} {*v}")
  >     }
  >     var h: hashmap.HashMap<Str, I64> = hashmap.HashMap.new()
  >     let x = h.insert("one", 1)
  >     let y = h.insert("two", 2)
  >     var total = 0
  >     for e in h.iter() {
  >         total = total + *e.1
  >         if *e.1 == 2 { break }
  >     }
  >     println("{total}")
  >     let z = m.insert(4, "d")
  >     println("{m.len()}")
  > }
  > EOF
  $ sinterc run ok.sn
  b 是第 4 个
  3 z
  1 a
  2 b
  3 c
  3
  4

迭代器还活着时修改 Map：拒绝。

  $ cat > lock.sn <<'EOF'
  > use std.map
  > 
  > fn main() {
  >     var m: map.Map<I64, I64> = map.Map.new()
  >     let a = m.insert(1, 1)
  >     for e in m.iter() {
  >         let b = m.insert(2, 2)
  >     }
  > }
  > EOF
  $ sinterc lock.sn
  lock.sn:7:17: 错误：m 正被引用 for 循环的迭代器 借用（之后还会用到 for 循环的迭代器），不能以 inout 传入
   7 |         let b = m.insert(2, 2)
     |                 ^
  [1]

不是 Vec、区间或迭代器：

  $ cat > notiter.sn <<'EOF'
  > fn main() {
  >     for x in 3 {
  >     }
  > }
  > EOF
  $ sinterc notiter.sn
  notiter.sn:2:14: 错误：for 只能遍历 Vec、a..b 或迭代器（有 next 方法的值，例如 s.chars()、m.iter()），I64 不是
   2 |     for x in 3 {
     |              ^
  [1]
