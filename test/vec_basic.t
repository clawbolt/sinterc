内建的动态数组 Vec<T>（Lean `Datatypes.Vec`）。vec_new()、vec_push(inout v, sink x)、vec_pop(inout v) -> Option<T>、vec_len(v)、vec_get(v, i)。
vec_get 是一个位置：读出元素（可复制时复制），或者作为元素的投影借用；push 和 pop 要 inout 传入 v，所以投影还活着时被排他性检查拒绝。
Vec 和其中的元素都计入泄漏检查，程序正常退出就说明没有泄漏。

push、pop、用 get 求和；作用域结束时剩下的元素一起析构：

  $ cat > ok.sn <<'EOF'
  > fn sum(v: Vec<Box<I64>>) -> I64 {
  >     var s = 0
  >     var i = 0
  >     while i < v.len() {
  >         s = s + *v[i]
  >         i = i + 1
  >     }
  >     s
  > }
  > 
  > fn main() {
  >     var v: Vec<Box<I64>> = vec_new()
  >     var i = 1
  >     while i <= 10 {
  >         v.push(box(i))
  >         i = i + 1
  >     }
  >     println("{v.len()}")
  >     println("{sum(v)}")
  >     match v.pop() {
  >         .Some(b) => println("{*b}"),
  >         .None => println("{0}")
  >     }
  >     println("{sum(v)}")
  >     var w: Vec<I64> = vec_new()
  >     match w.pop() {
  >         .Some(x) => println("{x}"),
  >         .None => println("{-1}")
  >     }
  >     w.push(7)
  >     w.push(8)
  >     let a = w[1]
  >     println("{a}")
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
  >     w.push(1)
  >     println("{w[5]}")
  > }
  > EOF
  $ sinterc oob.sn -o oob.c && cc -std=gnu11 -w oob.c -o oob && ./oob
  panic: 下标越界
  [101]

get 的投影还活着时 push：

  $ cat > lock.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Box<I64>> = vec_new()
  >     v.push(box(1))
  >     let e = v[0]
  >     v.push(box(2))
  >     println("{*e}")
  > }
  > EOF
  $ sinterc lock.sn
  lock.sn:5:5: 错误：v 正被投影 e 借用（之后还会用到 e），不能以 inout 传入
   5 |     v.push(box(2))
     |     ^
  [1]

元素不能直接移出（要用 v.pop()）：

  $ cat > mv.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Box<I64>> = vec_new()
  >     v.push(box(1))
  >     let b = unbox(v[0])
  > }
  > EOF
  $ sinterc mv.sn
  mv.sn:4:20: 错误：不能从 Box 或 Vec 里移出值（Box 用 unbox，Vec 的元素用 v.pop()）
   4 |     let b = unbox(v[0])
     |                    ^
  [1]
