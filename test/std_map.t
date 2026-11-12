std.map，存成 AVL 树的有序映射（Lean `Library.TreeMap`：中序遍历就是 `Library.Map` 的有序表，插入、删除和 `Library.Map` 一致）。
查找、插入、删除都是对数时间；值通过闭包拿到：m.get(k, |v| …)、m.for_each(|k, v| …)。
按升序插入 1023 个键（不平衡的树会退化成链表），AVL 树的高度是 10；删掉偶数键之后键仍然有序：

  $ cat > ok.sn <<'EOF'
  > use std.map
  > 
  > fn main() {
  >     var m: map.Map<I64, I64> = map.Map.new()
  >     for i in 0..1023 {
  >         let o = m.insert(i, i * i)
  >     }
  >     println("{m.len()}")
  >     println("{m.height()}")
  >     match m.get(64, |v| v + 1) {
  >         .Some(x) => println("{x}"),
  >         .None => println("没有")
  >     }
  >     println("{m.contains(1022)}")
  >     println("{m.contains(1023)}")
  >     match m.insert(5, 0) {
  >         .Some(x) => println("{x}"),
  >         .None => println("新键")
  >     }
  >     println("{m.len()}")
  >     var k = 0
  >     while k < 1023 {
  >         let o = m.remove(k)
  >         k = k + 2
  >     }
  >     match m.remove(0) {
  >         .Some(x) => println("{x}"),
  >         .None => println("已经删掉")
  >     }
  >     println("{m.len()}")
  >     println("{m.height() <= 10}")
  >     var ok = true
  >     var prev = -1
  >     var total = 0
  >     m.for_each(|key, v| {
  >         if key <= prev || key % 2 == 0 { ok = false }
  >         prev = key
  >         total = total + 1
  >     })
  >     println("{ok}")
  >     println("{total}")
  >     println("{m.contains(50)}")
  >     println("{m.contains(51)}")
  > }
  > EOF
  $ sinterc run ok.sn
  1023
  10
  4097
  true
  false
  25
  1023
  已经删掉
  511
  true
  true
  511
  false
  true

键和值都可以是 Str（Box 里的值和整棵树在作用域结束时析构，没有泄漏）：

  $ cat > str.sn <<'EOF'
  > use std.map
  > 
  > fn main() {
  >     var m: map.Map<Str, Str> = map.Map.new()
  >     let o1 = m.insert("pear", "梨")
  >     let o2 = m.insert("apple", "苹果")
  >     let o3 = m.insert("fig", "无花果")
  >     m.for_each(|k, v| println(k.concat(" ").concat(v)))
  >     match m.get("fig", |v| v.len()) {
  >         .Some(n) => println("{n}"),
  >         .None => println("没有")
  >     }
  >     match m.remove("apple") {
  >         .Some(v) => println(v),
  >         .None => println("没有")
  >     }
  >     println("{m.len()}")
  > }
  > EOF
  $ sinterc run str.sn
  apple 苹果
  fig 无花果
  pear 梨
  9
  苹果
  2
