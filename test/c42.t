C42：std.map 的查找改成二分查找（Lean S5：和原来从头往后找的结果相同，所以 S4 的定律照样成立）。
查找是对数时间；插入、删除仍要挪动元素，是线性时间。

  $ cat > ok.sn <<'EOF'
  > use std.map
  > 
  > fn main() {
  >     var m: map.Map<I64, I64> = map.new()
  >     // 0..101 按 i * 37 % 101 的顺序插入：键是 0 到 100 的一个排列
  >     for i in 0..101 {
  >         let k = i * 37 % 101
  >         let o = map.insert(m, k, k * k)
  >     }
  >     print_i64(map.len(m))
  >     var ok = true
  >     var j = 0
  >     for key in m.keys {
  >         if key != j { ok = false }
  >         j = j + 1
  >     }
  >     print_bool(ok)
  >     match map.index_of(m, 64) {
  >         .Some(i) => print_i64(vec_get(m.vals, i)),
  >         .None => print("没有")
  >     }
  >     print_bool(map.contains(m, 0))
  >     print_bool(map.contains(m, 100))
  >     print_bool(map.contains(m, 101))
  >     print_bool(map.contains(m, -1))
  >     var k = 0
  >     while k < 101 {
  >         let o = map.remove(m, k)
  >         k = k + 2
  >     }
  >     print_i64(map.len(m))
  >     print_bool(map.contains(m, 50))
  >     print_bool(map.contains(m, 51))
  > }
  > EOF
  $ sinterc run ok.sn
  101
  true
  4096
  true
  true
  false
  false
  50
  false
  true
