C39：标准库 std.map（规格 Lean S4）和内建的 vec_swap（Lean M20）。Map<K, V> 按键从小到大存成两个 Vec，
键要实现 math.Ord；insert 已有键时换掉值并返回旧值，remove 返回去掉的值。vec_swap(inout v, i, j) 交换两个元素，越界时 panic。

  $ cat > ok.sn <<'EOF'
  > use std.map
  > use std.option
  > 
  > fn show(m: map.Map<Str, I64>) {
  >     var i = 0
  >     while i < map.len(m) {
  >         print(vec_get(m.keys, i))
  >         print_i64(vec_get(m.vals, i))
  >         i = i + 1
  >     }
  > }
  > 
  > fn main() {
  >     var m: map.Map<Str, I64> = map.new()
  >     print_bool(option.is_none(map.insert(m, "pear", 3)))
  >     let a = map.insert(m, "apple", 1)
  >     let b = map.insert(m, "fig", 2)
  >     print_i64(option.unwrap_or(map.insert(m, "fig", 20), 0))
  >     show(m)
  >     print_bool(map.contains(m, "fig"))
  >     print_bool(map.contains(m, "kiwi"))
  >     match map.index_of(m, "pear") {
  >         .Some(i) => print_i64(vec_get(m.vals, i)),
  >         .None => print("没有")
  >     }
  >     print_i64(option.unwrap_or(map.remove(m, "apple"), 0))
  >     print_i64(option.unwrap_or(map.remove(m, "kiwi"), -1))
  >     print_i64(map.len(m))
  >     show(m)
  >     var n: map.Map<I64, Str> = map.new()
  >     var k = 5
  >     while k > 0 {
  >         let c = map.insert(n, k, str_concat("v", str_slice("abcde", k - 1, k)))
  >         k = k - 1
  >     }
  >     print(option.unwrap_or(map.insert(n, 3, "C"), "?"))
  >     for key in n.keys {
  >         print_i64(key)
  >     }
  >     for val in n.vals {
  >         print(val)
  >     }
  >     var v: Vec<I64> = vec_new()
  >     v.push(1)
  >     v.push(2)
  >     v.swap(0, 1)
  >     print_i64(vec_get(v, 0))
  > }
  > EOF
  $ sinterc run ok.sn
  true
  2
  apple
  1
  fig
  20
  pear
  3
  true
  false
  3
  1
  -1
  2
  fig
  20
  pear
  3
  vc
  1
  2
  3
  4
  5
  va
  vb
  C
  vd
  ve
  2

vec_swap 越界时 panic：

  $ cat > oob.sn <<'EOF'
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     v.push(1)
  >     vec_swap(v, 0, 1)
  > }
  > EOF
  $ sinterc run oob.sn
  panic: 下标越界
  [101]

vec_swap 的 v 要能修改：

  $ cat > let.sn <<'EOF'
  > fn f(v: Vec<I64>) {
  >     vec_swap(v, 0, 0)
  > }
  > fn main() {
  > }
  > EOF
  $ sinterc let.sn
  let.sn:2:14: 错误：v 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）
  [1]

键的类型要实现 math.Ord：

  $ cat > noord.sn <<'EOF'
  > use std.map
  > struct P { x: I64 }
  > fn main() {
  >     var m: map.Map<P, I64> = map.new()
  >     let o = map.insert(m, P { x: 1 }, 1)
  > }
  > EOF
  $ sinterc noord.sn
  noord.sn:5:17: 错误：P 没有实现 math.Ord（map.insert 要求 K: math.Ord）
  [1]
