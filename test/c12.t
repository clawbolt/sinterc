C12：内建的动态数组 Vec<T>（Lean M12）。vec_new()、vec_push(inout v, sink x)、vec_pop(inout v) -> Option<T>、vec_len(v)、vec_get(v, i)。
vec_get 是一个位置：读出元素（可复制时复制），或者作为元素的投影借用；push 和 pop 要 inout 传入 v，所以投影还活着时被排他性检查拒绝。
Vec 和其中的元素都计入泄漏检查，程序正常退出就说明没有泄漏。

push、pop、用 get 求和；作用域结束时剩下的元素一起析构：

  $ cat > ok.sn <<'EOF'
  > fn sum(v: Vec<Box<I64>>) -> I64 {
  >     var s = 0
  >     var i = 0
  >     while i < vec_len(v) {
  >         s = s + *vec_get(v, i)
  >         i = i + 1
  >     }
  >     s
  > }
  > 
  > fn main() {
  >     var v: Vec<Box<I64>> = vec_new()
  >     var i = 1
  >     while i <= 10 {
  >         vec_push(v, box(i))
  >         i = i + 1
  >     }
  >     print_i64(vec_len(v))
  >     print_i64(sum(v))
  >     match vec_pop(v) {
  >         .Some(b) => print_i64(*b),
  >         .None => print_i64(0)
  >     }
  >     print_i64(sum(v))
  >     var w: Vec<I64> = vec_new()
  >     match vec_pop(w) {
  >         .Some(x) => print_i64(x),
  >         .None => print_i64(-1)
  >     }
  >     vec_push(w, 7)
  >     vec_push(w, 8)
  >     let a = vec_get(w, 1)
  >     print_i64(a)
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  10
  55
  10
  45
  -1
  8

下标越界：

  $ cat > oob.sn <<'EOF'
  > fn main() {
  >     var w: Vec<I64> = vec_new()
  >     vec_push(w, 1)
  >     print_i64(vec_get(w, 5))
  > }
  > EOF
  $ sinterc oob.sn -o oob.c && cc -std=gnu11 -w oob.c -o oob && ./oob
  panic: 下标越界
  [101]

get 的投影还活着时 push：

  $ cat > lock.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Box<I64>> = vec_new()
  >     vec_push(v, box(1))
  >     let e = vec_get(v, 0)
  >     vec_push(v, box(2))
  >     print_i64(*e)
  > }
  > EOF
  $ sinterc lock.sn
  lock.sn:5:14: 错误：v 正被投影 e 借用（之后还会用到 e），不能以 inout 传入
   5 |     vec_push(v, box(2))
     |              ^
  [1]

元素不能直接移出（要用 vec_pop）：

  $ cat > mv.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Box<I64>> = vec_new()
  >     vec_push(v, box(1))
  >     let b = unbox(vec_get(v, 0))
  > }
  > EOF
  $ sinterc mv.sn
  mv.sn:4:19: 错误：不能从 Box 或 Vec 里移出值（Box 用 unbox，Vec 的元素用 vec_pop）
   4 |     let b = unbox(vec_get(v, 0))
     |                   ^
  [1]
