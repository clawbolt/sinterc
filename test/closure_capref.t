闭包捕获引用（Lean `Closures.CapRef`）。闭包可以用类型里有区域的变量：&T in a、Vec<&T in a>、区域 a 本身。
提升出的函数里，这些区域换成区域参数，按闭包所在位置的嵌套顺序编号，只保留那里真实成立的先后关系。

  $ cat > ok.sn <<'EOF'
  > 
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     v.push(1)
  >     v.push(2)
  >     v.push(3)
  >     arena |a| {
  >         let base = alloc(a, 100)
  >         // 读外层区域里的值
  >         let w = v.map(|x| x + *base)
  >         for x in w {
  >             println("{x}")
  >         }
  >         arena |b| {
  >             // 捕获区域 b 本身，在里面分配；改外层的 Vec<&I64 in b>
  >             var refs: Vec<&I64 in b> = vec_new()
  >             v.for_each(|x| { refs.push(alloc(b, x * 2)) })
  >             // 外层区域 a 的引用可以放进 Vec<&I64 in b>（a 比 b 活得久）
  >             v.for_each(|x| {
  >                 if x == 1 { refs.push(base) }
  >             })
  >             var s = 0
  >             var i = 0
  >             while i < refs.len() {
  >                 s = s + *refs[i]
  >                 i = i + 1
  >             }
  >             println("{s}")
  >             println("{v.count(|x| x * 50 > *refs[3])}")
  >         }
  >     }
  > }
  > EOF
  $ sinterc run ok.sn
  101
  102
  103
  112
  1

闭包里也不能把内层区域的引用放进外层区域的 Vec：

  $ cat > short.sn <<'EOF'
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     arena |a| {
  >         var outer: Vec<&I64 in a> = vec_new()
  >         arena |b| {
  >             v.for_each(|x| { outer.push(alloc(b, x)) })
  >         }
  >     }
  > }
  > EOF
  $ sinterc short.sn
  short.sn:6:41: 错误：类型不匹配：需要 &I64 in a，实际是 &I64 in b
   6 |             v.for_each(|x| { outer.push(alloc(b, x)) })
     |                                         ^
  [1]
