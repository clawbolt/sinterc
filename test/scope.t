结构化并发（Lean `Concurrency.Scope`）。scope |s| { … } 里 s.spawn(|| …) 启动任务，块结束时等所有任务结束。
任务的捕获只有两种：scope 外面的变量只读共享（整个 scope 里谁都不能修改、移动它），scope 里面的变量移进任务。
所以任务之间、任务和父任务之间没有数据竞争，结果和按顺序执行相同；任务的输出顺序不定，这里排好序再比较。

  $ cat > ok.sn <<'EOF'
  > fn count(label: Str, n: I64, words: Vec<Str>) {
  >     var total = 0
  >     for i in 0..n {
  >         total = total + i
  >     }
  >     println("{label} {n}: {total} {words.len()}")
  > }
  > 
  > fn main() {
  >     var words: Vec<Str> = Vec.new()
  >     words.push("x")
  >     words.push("y")
  >     let prefix = "task"
  >     scope |s| {
  >         for k in 1..5 {
  >             let label = "{prefix}{k}"
  >             s.spawn(|| count(label, k * 1000, words))
  >         }
  >     }
  >     words.push("z")
  >     println("{words.len()}")
  > }
  > EOF
  $ sinterc run ok.sn | sort
  3
  task1 1000: 499500 2
  task2 2000: 1999000 2
  task3 3000: 4498500 2
  task4 4000: 7998000 2

父任务在 scope 里修改只读共享的变量：拒绝。

  $ cat > parent_write.sn <<'EOF'
  > fn main() {
  >     var n = 1
  >     scope |s| {
  >         s.spawn(|| println("{n}"))
  >         n = 2
  >     }
  > }
  > EOF
  $ sinterc parent_write.sn
  parent_write.sn:5:9: 错误：n 被 scope 里的任务只读共享，scope 结束之前不能修改
   5 |         n = 2
     |         ^
  [1]

任务修改 scope 外面的变量：拒绝。

  $ cat > task_write.sn <<'EOF'
  > fn main() {
  >     var n = 1
  >     scope |s| {
  >         s.spawn(|| { n = 2 })
  >     }
  > }
  > EOF
  $ sinterc task_write.sn
  task_write.sn:4:17: 错误：任务不能修改 scope 外面的变量 n：它被所有任务只读共享（要修改的值先在 scope 里放进新的变量，再移进任务）
   4 |         s.spawn(|| { n = 2 })
     |                 ^
  [1]

移进任务之后父任务还用它：拒绝。

  $ cat > moved.sn <<'EOF'
  > fn main() {
  >     scope |s| {
  >         let msg = "hi"
  >         s.spawn(|| println(msg))
  >         println(msg)
  >     }
  > }
  > EOF
  $ sinterc moved.sn
  moved.sn:5:17: 错误：msg 已被移动，不能再使用
   5 |         println(msg)
     |                 ^
  [1]

scope 里 return：拒绝（离开之前要等任务结束）。

  $ cat > ret.sn <<'EOF'
  > fn main() {
  >     scope |s| {
  >         return
  >     }
  > }
  > EOF
  $ sinterc ret.sn
  ret.sn:3:9: 错误：scope 里不能写 return：离开 scope 之前要等所有任务结束（可以先把结果存进 scope 外面的 var 变量）
   3 |         return
     |         ^
  [1]

s 只能用来 spawn：

  $ cat > handle.sn <<'EOF'
  > fn main() {
  >     scope |s| {
  >         let t = s
  >     }
  > }
  > EOF
  $ sinterc handle.sn
  handle.sn:3:17: 错误：s 是 scope 的句柄，只能用来启动任务：s.spawn(|| …)
   3 |         let t = s
     |                 ^
  [1]
