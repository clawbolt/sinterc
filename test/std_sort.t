std.vec 的 sort 和 sort_by（Lean `Library.HeapSort`）。堆排序，只用 vec_swap 交换元素，O(n log n)，不稳定。
`Library.HeapSort` 证明了结果排好序（heapSort_sorted），并且是原来元素的一个排列（heapSort_swaps）。

  $ cat > ok.sn <<'EOF'
  > 
  > struct Person { name: Str, age: I64 }
  > 
  > fn show(v: Vec<I64>) {
  >     var s = ""
  >     var first = true
  >     for x in v {
  >         if !first { s.push(" ") }
  >         s.push("{x}")
  >         first = false
  >     }
  >     println(s)
  > }
  > 
  > fn next(inout s: I64) -> I64 {
  >     s = (s * 1103515245 + 12345) % 2147483648
  >     s % 1000
  > }
  > 
  > fn insertion(inout v: Vec<I64>) {
  >     var i = 1
  >     while i < v.len() {
  >         var j = i
  >         while j > 0 && v[j - 1] > v[j] {
  >             v.swap(j - 1, j)
  >             j = j - 1
  >         }
  >         i = i + 1
  >     }
  > }
  > 
  > fn main() {
  >     var a: Vec<I64> = vec_new()
  >     a.push(5)
  >     a.push(3)
  >     a.push(8)
  >     a.push(1)
  >     a.push(9)
  >     a.push(2)
  >     a.push(7)
  >     a.push(2)
  >     a.sort()
  >     show(a)
  >     a.sort_by(|x, y| x > y)
  >     show(a)
  >     var e: Vec<I64> = vec_new()
  >     e.sort()
  >     println("{e.len()}")
  >     e.push(4)
  >     e.sort()
  >     show(e)
  >     var w: Vec<Str> = vec_new()
  >     w.push("pear")
  >     w.push("apple")
  >     w.push("fig")
  >     w.push("banana")
  >     w.sort()
  >     for s in w {
  >         println(s)
  >     }
  >     var ps: Vec<Person> = vec_new()
  >     ps.push(Person { name: "Ann", age: 30 })
  >     ps.push(Person { name: "Bo", age: 25 })
  >     ps.push(Person { name: "Cy", age: 35 })
  >     ps.push(Person { name: "Di", age: 28 })
  >     ps.sort_by(|p, q| p.age < q.age)
  >     for p in ps {
  >         println("{p.name} {p.age}")
  >     }
  >     var f: Vec<F64> = vec_new()
  >     f.push(3.0)
  >     f.push(-1.5)
  >     f.push(2.25)
  >     f.push(0.0)
  >     f.sort_by(|x, y| x < y)
  >     for x in f {
  >         println("{x}")
  >     }
  >     // 和插入排序对照：2000 个伪随机数
  >     var seed = 42
  >     var b: Vec<I64> = vec_new()
  >     var c: Vec<I64> = vec_new()
  >     for i in 0..2000 {
  >         let x = next(seed)
  >         b.push(x)
  >         c.push(x)
  >     }
  >     b.sort()
  >     insertion(c)
  >     var same = true
  >     for i in 0..2000 {
  >         if b[i] != c[i] { same = false }
  >     }
  >     println("{same}")
  > }
  > EOF
  $ sinterc run ok.sn
  1 2 2 3 5 7 8 9
  9 8 7 5 3 2 2 1
  0
  4
  apple
  banana
  fig
  pear
  Bo 25
  Di 28
  Ann 30
  Cy 35
  -1.5
  0.0
  2.25
  3.0
  true
