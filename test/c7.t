C7：按借用的 match（Lean M8（二））。match let x { … } 显式借用；被匹配的位置不能整个移走时
（let / inout 参数、字段、解引用）自动借用，和 let 绑定投影的规则相同。
需要析构的载荷绑定成投影，不拥有值；投影还会用到时，被匹配的值被锁住，不能移动、修改、以 inout 传入或析构。

显式借用后继续用 x、let 参数和字段上的自动借用、借用之后再按移动拆开：

  $ cat > ok.sn <<'EOF'
  > enum Opt { Some(Box<I64>), None }
  > struct Holder { o: Opt, n: I64 }
  > 
  > fn peek(o: Opt) -> I64 {
  >     match o {
  >         .Some(b) => *b,
  >         .None => 0
  >     }
  > }
  > 
  > fn show(b: Box<I64>) { print_i64(*b) }
  > 
  > fn main() {
  >     let o: Opt = .Some(box(5))
  >     let n = match let o {
  >         .Some(b) => { show(b); *b + 1 }
  >         .None => 0
  >     }
  >     print_i64(n)
  >     print_i64(peek(o))
  >     let h = Holder { o: .Some(box(7)), n: 1 }
  >     print_i64(match h.o { .Some(b) => *b, .None => 0 })
  >     print_i64(match o { .Some(b) => *b, .None => -1 })
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  5
  6
  5
  7
  5

锁定：借用期间移走被匹配的值（Lean M8（二）的反例：之后再通过 b 读就是读已释放的内存）：

  $ cat > lock.sn <<'EOF'
  > enum Opt { Some(Box<I64>), None }
  > fn eat(sink o: Opt) { }
  > fn main() {
  >     let o: Opt = .Some(box(1))
  >     match let o {
  >         .Some(b) => {
  >             eat(o)
  >             print_i64(*b)
  >         }
  >         .None => { }
  >     }
  > }
  > EOF
  $ sinterc lock.sn
  lock.sn:7:17: 错误：o 正被投影 b 借用（之后还会用到 b），不能移动
   7 |             eat(o)
     |                 ^
  [1]

分支变量不拥有载荷，不能移走：

  $ cat > own.sn <<'EOF'
  > enum Opt { Some(Box<I64>), None }
  > fn eat_box(sink b: Box<I64>) { }
  > fn main() {
  >     let o: Opt = .Some(box(1))
  >     match let o {
  >         .Some(b) => eat_box(b),
  >         .None => { }
  >     }
  > }
  > EOF
  $ sinterc own.sn
  own.sn:6:29: 错误：b 不拥有值（是 let / inout 参数或投影），不能移动
   6 |         .Some(b) => eat_box(b),
     |                             ^
  [1]

match let 只能借用一个位置：

  $ cat > place.sn <<'EOF'
  > enum Opt { Some(Box<I64>), None }
  > fn wrap(n: I64) -> Opt { .Some(box(n)) }
  > fn main() {
  >     let n = match let wrap(1) {
  >         .Some(b) => *b,
  >         .None => 0
  >     }
  > }
  > EOF
  $ sinterc place.sn
  place.sn:4:23: 错误：match let 后面必须是变量、字段或解引用（要借用的位置）
   4 |     let n = match let wrap(1) {
     |                       ^
  [1]
