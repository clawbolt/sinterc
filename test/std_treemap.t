std.treemap，存成 AVL 树的有序映射（Lean `Library.TreeMap`：中序遍历就是 `Library.Map` 的有序表，插入、删除和 `Library.Map` 一致）。
查找、插入、删除都是对数时间；值通过闭包拿到：get(m, k, |v| …)、for_each(m, |k, v| …)。
按升序插入 1023 个键（不平衡的树会退化成链表），AVL 树的高度是 10；删掉偶数键之后键仍然有序：

  $ cat > ok.sn <<'EOF'
  > use std.treemap
  > 
  > fn main() {
  >     var m: treemap.TreeMap<I64, I64> = treemap.new()
  >     for i in 0..1023 {
  >         let o = treemap.insert(m, i, i * i)
  >     }
  >     print_i64(treemap.len(m))
  >     print_i64(treemap.height(m))
  >     match treemap.get(m, 64, |v| v + 1) {
  >         .Some(x) => print_i64(x),
  >         .None => print("没有")
  >     }
  >     print_bool(treemap.contains(m, 1022))
  >     print_bool(treemap.contains(m, 1023))
  >     match treemap.insert(m, 5, 0) {
  >         .Some(x) => print_i64(x),
  >         .None => print("新键")
  >     }
  >     print_i64(treemap.len(m))
  >     var k = 0
  >     while k < 1023 {
  >         let o = treemap.remove(m, k)
  >         k = k + 2
  >     }
  >     match treemap.remove(m, 0) {
  >         .Some(x) => print_i64(x),
  >         .None => print("已经删掉")
  >     }
  >     print_i64(treemap.len(m))
  >     print_bool(treemap.height(m) <= 10)
  >     var ok = true
  >     var prev = -1
  >     var total = 0
  >     treemap.for_each(m, |key, v| {
  >         if key <= prev || key % 2 == 0 { ok = false }
  >         prev = key
  >         total = total + 1
  >     })
  >     print_bool(ok)
  >     print_i64(total)
  >     print_bool(treemap.contains(m, 50))
  >     print_bool(treemap.contains(m, 51))
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
  > use std.treemap
  > 
  > fn main() {
  >     var m: treemap.TreeMap<Str, Str> = treemap.new()
  >     let o1 = treemap.insert(m, "pear", "梨")
  >     let o2 = treemap.insert(m, "apple", "苹果")
  >     let o3 = treemap.insert(m, "fig", "无花果")
  >     treemap.for_each(m, |k, v| print(str_concat(str_concat(k, " "), v)))
  >     match treemap.get(m, "fig", |v| str_len(v)) {
  >         .Some(n) => print_i64(n),
  >         .None => print("没有")
  >     }
  >     match treemap.remove(m, "apple") {
  >         .Some(v) => print(v),
  >         .None => print("没有")
  >     }
  >     print_i64(treemap.len(m))
  > }
  > EOF
  $ sinterc run str.sn
  apple 苹果
  fig 无花果
  pear 梨
  9
  苹果
  2
