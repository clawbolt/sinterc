std.os——环境变量、目录、子进程。内建函数叫 os_…（不占用户的名字），std.os 给它们短的名字。
和文件读写一样只读外部的状态、返回新的值（Lean `Library.FileIO` 的形状）；os.exit 和 panic 一样结束进程、不析构（Lean `FlowGraph.Panic`）。
子进程不经过 shell，参数原样传过去。

  $ cat > ok.sn <<'EOF'
  > use std.os
  > 
  > fn strs(sink a: Str, sink b: Str) -> Vec<Str> {
  >     var v: Vec<Str> = vec_new()
  >     v.push(a)
  >     v.push(b)
  >     v
  > }
  > 
  > fn main() {
  >     match os.env("SINTER_TEST") {
  >         .Some(v) => print("env: {v}"),
  >         .None => print("env: none")
  >     }
  >     match os.env("SINTER_NOT_SET") {
  >         .Some(v) => print("env: {v}"),
  >         .None => print("env: none")
  >     }
  >     match os.make_dir("d") {
  >         Ok(_) => print("made d"),
  >         Err(e) => print(e)
  >     }
  >     print("{os.exists("d")} {os.is_dir("d")} {os.exists("nope")}")
  >     match write_file("d/b.txt", "b") { Ok(_) => {}, Err(e) => print(e) }
  >     match write_file("d/a.txt", "a") { Ok(_) => {}, Err(e) => print(e) }
  >     match write_file("d/c.txt", "c") { Ok(_) => {}, Err(e) => print(e) }
  >     match os.list_dir("d") {
  >         Ok(ns) => {
  >             var s = ""
  >             for n in ns {
  >                 s.push(n)
  >                 s.push(" ")
  >             }
  >             print(s)
  >         },
  >         Err(e) => print(e)
  >     }
  >     match os.remove("d/b.txt") { Ok(_) => print("removed"), Err(e) => print(e) }
  >     print("{os.exists("d/b.txt")} {os.is_dir("d/a.txt")}")
  >     match os.run("sh", strs("-c", "echo from child; exit 3")) {
  >         Ok(c) => print("code {c}"),
  >         Err(e) => print(e)
  >     }
  >     match os.output("echo", strs("hello", "world")) {
  >         Ok(out) => print("got [{out}]"),
  >         Err(e) => print(e)
  >     }
  >     match os.output("sh", strs("-c", "exit 2")) {
  >         Ok(out) => print(out),
  >         Err(e) => print(e)
  >     }
  >     let none: Vec<Str> = vec_new()
  >     match os.run("sinter-no-such-program", none) {
  >         Ok(c) => print("code {c}"),
  >         Err(e) => print("cannot start")
  >     }
  >     print("bye")
  >     os.exit(7)
  >     print("not printed")
  > }
  > EOF
  $ SINTER_TEST=hi sinterc run ok.sn
  env: hi
  env: none
  made d
  true true false
  a.txt b.txt c.txt 
  removed
  false false
  from child
  code 3
  got [hello world
  ]
  sh：退出码 2
  cannot start
  bye
  [7]

目录不存在时 Err：

  $ cat > nodir.sn <<'EOF'
  > use std.os
  > fn main() {
  >     match os.list_dir("no-such-dir") {
  >         Ok(ns) => print("{ns.len()}"),
  >         Err(e) => print("error")
  >     }
  >     match os.make_dir("no-such-dir/x") {
  >         Ok(_) => print("made"),
  >         Err(e) => print("error")
  >     }
  > }
  > EOF
  $ sinterc run nodir.sn
  error
  error

os.exit 之后的代码走不到，也不析构（和 panic 一样，泄漏检查不报错）：

  $ cat > early.sn <<'EOF'
  > use std.os
  > fn main() {
  >     let s = "owned"
  >     let b = box(1)
  >     if s.len() > 0 { os.exit(0) }
  >     print("unreachable")
  > }
  > EOF
  $ sinterc run early.sn
