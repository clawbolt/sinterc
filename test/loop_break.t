break 和 continue。break 跳出最内层的循环，continue 进入它的下一轮（for 循环照样走到下一个元素）。
跳走之前析构循环里还活着的值、释放循环里进入的区域，和 return 穿过作用域一样；
之后的代码走不到。所有权检查和 Lean 的复核本来就按控制流图做（`FlowGraph.Region` 对任意 CFG 成立），只是多了几条边。
运行时的泄漏检查保证跳走时值都被析构了。

  $ cat > ok.sn <<'EOF'
  > fn find(v: Vec<I64>, x: I64) -> I64 {
  >     var i = 0
  >     for y in v {
  >         if y == x { return i }
  >         i = i + 1
  >     }
  >     -1
  > }
  > 
  > fn main() {
  >     for i in 0..10 {
  >         if i == 3 { continue }
  >         if i == 6 { break }
  >         print_i64(i)
  >     }
  >     var n = 0
  >     while true {
  >         n = n + 1
  >         if n > 2 { break }
  >     }
  >     print_i64(n)
  >     var v: Vec<I64> = vec_new()
  >     for i in 0..5 { v.push(i * 3) }
  >     print_i64(find(v, 9))
  >     print_i64(find(v, 4))
  >     var names: Vec<Str> = vec_new()
  >     names.push("甲")
  >     names.push("乙")
  >     names.push("丙")
  >     for s in names {
  >         let t = s.concat("!")
  >         if s.eq("乙") { continue }
  >         print(t)
  >     }
  >     for i in 0..3 {
  >         arena |a| {
  >             let r = alloc(a, i)
  >             if *r == 1 { break }
  >             print_i64(*r)
  >         }
  >     }
  >     for i in 0..2 {
  >         for j in 0..5 {
  >             if j == 2 { break }
  >             print_i64(i * 10 + j)
  >         }
  >     }
  > }
  > EOF
  $ sinterc run ok.sn
  0
  1
  2
  4
  5
  3
  3
  -1
  甲!
  丙!
  0
  0
  1
  10
  11

循环外面的 break：

  $ cat > out.sn <<'EOF'
  > fn main() {
  >     break
  > }
  > EOF
  $ sinterc out.sn
  out.sn:2:5: 错误：break 只能用在循环里
   2 |     break
     |     ^
  [1]
  $ sinterc --lang=en out.sn
  out.sn:2:5: error: break can only be used inside a loop
   2 |     break
     |     ^
  [1]

跳走之前没有消费 linear 值：

  $ cat > lin.sn <<'EOF'
  > linear struct T { x: I64 }
  > fn main() {
  >     while true {
  >         let t = T { x: 1 }
  >         break
  >     }
  > }
  > EOF
  $ sinterc lin.sn
  lin.sn:5:9: 错误：t 是 linear 值，作用域结束时还没有被消费
   5 |         break
     |         ^
  [1]
