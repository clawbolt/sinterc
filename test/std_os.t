std.fs、std.env、std.process——目录、环境变量、子进程（原来的 std.os）。内建函数叫 os_…（不占用户的名字），这些模块给它们短的名字。
和文件读写一样只读外部的状态、返回新的值（Lean `Library.FileIO` 的形状）；env.exit 和 panic 一样结束进程、不析构（Lean `FlowGraph.Panic`）。
子进程不经过 shell，参数原样传过去。

  $ cat > ok.sn <<'EOF'
  > use std.fs
  > use std.env
  > use std.process
  > 
  > fn strs(sink a: Str, sink b: Str) -> Vec<Str> {
  >     var v: Vec<Str> = Vec.new()
  >     v.push(a)
  >     v.push(b)
  >     v
  > }
  > 
  > fn main() {
  >     match env.get("SINTER_TEST") {
  >         .Some(v) => println("env: {v}"),
  >         .None => println("env: none")
  >     }
  >     match env.get("SINTER_NOT_SET") {
  >         .Some(v) => println("env: {v}"),
  >         .None => println("env: none")
  >     }
  >     match fs.make_dir("d") {
  >         Ok(_) => println("made d"),
  >         Err(e) => println(e)
  >     }
  >     println("{fs.exists("d")} {fs.is_dir("d")} {fs.exists("nope")}")
  >     match fs.write("d/b.txt", "b") { Ok(_) => {}, Err(e) => println(e) }
  >     match fs.write("d/a.txt", "a") { Ok(_) => {}, Err(e) => println(e) }
  >     match fs.write("d/c.txt", "c") { Ok(_) => {}, Err(e) => println(e) }
  >     match fs.list_dir("d") {
  >         Ok(ns) => {
  >             var s = ""
  >             for n in ns {
  >                 s.push(n)
  >                 s.push(" ")
  >             }
  >             println(s)
  >         },
  >         Err(e) => println(e)
  >     }
  >     match fs.remove("d/b.txt") { Ok(_) => println("removed"), Err(e) => println(e) }
  >     println("{fs.exists("d/b.txt")} {fs.is_dir("d/a.txt")}")
  >     match process.run("sh", strs("-c", "echo from child; exit 3")) {
  >         Ok(c) => println("code {c}"),
  >         Err(e) => println(e)
  >     }
  >     match process.output("echo", strs("hello", "world")) {
  >         Ok(out) => println("got [{out}]"),
  >         Err(e) => println(e)
  >     }
  >     match process.output("sh", strs("-c", "exit 2")) {
  >         Ok(out) => println(out),
  >         Err(e) => println(e)
  >     }
  >     let none: Vec<Str> = Vec.new()
  >     match process.run("sinter-no-such-program", none) {
  >         Ok(c) => println("code {c}"),
  >         Err(e) => println("cannot start")
  >     }
  >     println("bye")
  >     env.exit(7)
  >     println("not printed")
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
  > use std.fs
  > fn main() {
  >     match fs.list_dir("no-such-dir") {
  >         Ok(ns) => println("{ns.len()}"),
  >         Err(e) => println("error")
  >     }
  >     match fs.make_dir("no-such-dir/x") {
  >         Ok(_) => println("made"),
  >         Err(e) => println("error")
  >     }
  > }
  > EOF
  $ sinterc run nodir.sn
  error
  error

env.exit 之后的代码走不到，也不析构（和 panic 一样，泄漏检查不报错）：

  $ cat > early.sn <<'EOF'
  > use std.env
  > fn main() {
  >     let s = "owned"
  >     let b = box(1)
  >     if s.len() > 0 { env.exit(0) }
  >     println("unreachable")
  > }
  > EOF
  $ sinterc run early.sn
