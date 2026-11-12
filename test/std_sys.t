系统层的模块：std.io（标准输入输出）、std.fs（文件和目录）、std.env（参数、环境变量、退出）、std.process（子进程）。
都包着内建函数（Lean `Library.FileIO` 的形状：只读外部的状态、返回新的值）；打印一行的 println、eprintln 是预导入的。
标准库里可以用 intrinsic.f(…) 调用内建函数 f（模块自己有同名函数时，例如 io.read_line），别的代码不能。

标准输入、输出；read_line 和 read_all 读的是同一个缓冲：

  $ cat > io.sn <<'EOF'
  > use std.io
  > fn main() {
  >     println("hello")
  >     eprintln("to stderr")
  >     match io.read_line() {
  >         .Some(l) => println("first: {l}"),
  >         .None => println("none")
  >     }
  >     let rest = io.read_all()
  >     println("rest: {rest.len()} bytes")
  >     println(rest.trim())
  > }
  > EOF
  $ printf 'one\ntwo\nthree\n' | sinterc run io.sn
  hello
  to stderr
  first: one
  rest: 10 bytes
  two
  three

逐块读行（块很小时跨块的行也拼起来）：

  $ cat > chunks.sn <<'EOF'
  > use std.io
  > fn main() {
  >     var r = io.lines()
  >     var n = 0
  >     var going = true
  >     while going {
  >         arena |b| {
  >             match io.next_block_in(r, b, 3) {
  >                 .Some(block) => { n = n + block.lines().len() },
  >                 .None => { going = false }
  >             }
  >         }
  >     }
  >     println("{n} lines")
  > }
  > EOF
  $ printf 'aa\nbbbb\ncc' | sinterc run chunks.sn
  3 lines

文件和目录：

  $ cat > fs.sn <<'EOF'
  > use std.fs
  > fn main() {
  >     match fs.make_dir("d") { Ok(_) => println("made d"), Err(e) => println(e) }
  >     println("{fs.exists("d")} {fs.is_dir("d")} {fs.exists("nope")}")
  >     match fs.write("d/a.txt", "x\r\ny\n") { Ok(_) => {}, Err(e) => println(e) }
  >     match fs.read("d/a.txt") {
  >         Ok(s) => println("read {s.len()} bytes"),
  >         Err(e) => println(e)
  >     }
  >     var ls: Vec<Str> = Vec.new()
  >     ls.push("p")
  >     ls.push("q")
  >     match fs.write_lines("d/b.txt", ls) { Ok(_) => {}, Err(e) => println(e) }
  >     match fs.read_lines("d/b.txt") {
  >         Ok(v) => println(v.join(",")),
  >         Err(e) => println(e)
  >     }
  >     match fs.read_lines("d/a.txt") {
  >         Ok(v) => println(v.join(",")),
  >         Err(e) => println(e)
  >     }
  >     match fs.read_bytes("d/a.txt") {
  >         Ok(b) => println("{b.len()} bytes, utf8 {b.decode().is_some()}"),
  >         Err(e) => println(e)
  >     }
  >     match fs.list_dir("d") {
  >         Ok(ns) => println(ns.join(" ")),
  >         Err(e) => println(e)
  >     }
  >     arena |a| {
  >         match fs.read_in(a, "d/b.txt") {
  >             Ok(v) => println("{v.lines().len()} lines"),
  >             Err(e) => println(e)
  >         }
  >     }
  >     match fs.read("nope.txt") { Ok(_) => println("?"), Err(e) => println(e) }
  >     match fs.remove("d/a.txt") { Ok(_) => println("removed"), Err(e) => println(e) }
  >     println("{fs.exists("d/a.txt")}")
  > }
  > EOF
  $ sinterc run fs.sn
  made d
  true true false
  read 5 bytes
  p,q
  x,y
  5 bytes, utf8 true
  a.txt b.txt
  2 lines
  nope.txt：No such file or directory
  removed
  false

命令行参数、环境变量、子进程、退出：

  $ cat > sys.sn <<'EOF'
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
  >     let a = env.args()
  >     println("{a.len()} args: {a.join(",")}")
  >     match env.get("SINTER_TEST") {
  >         .Some(v) => println("env: {v}"),
  >         .None => println("env: none")
  >     }
  >     match env.get("SINTER_NOT_SET") {
  >         .Some(v) => println("env: {v}"),
  >         .None => println("env: none")
  >     }
  >     match process.run("sh", strs("-c", "echo from child; exit 3")) {
  >         Ok(c) => println("code {c}"),
  >         Err(e) => println(e)
  >     }
  >     match process.output("echo", strs("hello", "world")) {
  >         Ok(out) => println("got [{out.trim()}]"),
  >         Err(e) => println(e)
  >     }
  >     println("bye")
  >     env.exit(5)
  >     println("not printed")
  > }
  > EOF
  $ SINTER_TEST=hi sinterc run sys.sn x y
  2 args: x,y
  env: hi
  env: none
  from child
  code 3
  got [hello world]
  bye
  [5]

intrinsic.f(…) 只能在标准库里用：

  $ cat > intr.sn <<'EOF'
  > fn main() {
  >     let v = intrinsic.args()
  > }
  > EOF
  $ sinterc intr.sn
  intr.sn:2:23: 错误：intrinsic.args：内建函数只能在标准库里这样调用
   2 |     let v = intrinsic.args()
     |                       ^
  [1]
