文件与参数 IO（Lean `Library.FileIO`）。args() 是命令行参数（不含程序名），read_file 读整个文件，
write_file 把文件换成新内容（先写临时文件再 rename，失败时原文件不变）；出错时 Err(“路径：系统的说明”)。

  $ cat > io.sn <<'EOF'
  > 
  > use std.fs
  > use std.env
  > 
  > fn save(path: Str, lines: Vec<Str>) -> Result<(), Str> {
  >     let text = lines.join(",")
  >     fs.write(path, text)?
  >     Ok(())
  > }
  > 
  > fn main() {
  >     let a = env.args()
  >     println("{a.len()}")
  >     for x in a {
  >         println(x)
  >     }
  >     match save("out.txt", env.args()) {
  >         Ok(_) => println("写好了"),
  >         Err(e) => println(e)
  >     }
  >     match fs.read("out.txt") {
  >         Ok(s) => println(s),
  >         Err(e) => println(e)
  >     }
  >     match fs.write("out.txt", "覆盖") {
  >         Ok(_) => println("又写了一次"),
  >         Err(e) => println(e)
  >     }
  >     match fs.read("out.txt") {
  >         Ok(s) => println(s),
  >         Err(e) => println(e)
  >     }
  >     match fs.read("nope.txt") {
  >         Ok(s) => println(s),
  >         Err(e) => println(e)
  >     }
  >     match fs.write("no/such/dir/x.txt", "x") {
  >         Ok(_) => println("不该成功"),
  >         Err(e) => println(e)
  >     }
  > }
  > EOF
  $ sinterc run io.sn alpha "b c" 甲
  3
  alpha
  b c
  甲
  写好了
  alpha,b c,甲
  又写了一次
  覆盖
  nope.txt：No such file or directory
  no/such/dir/x.txt：No such file or directory

没有参数时 args() 是空的；写失败不留下临时文件：

  $ cat > empty.sn <<'EOF'
  > use std.env
  > 
  > fn main() {
  >     println("{env.args().len()}")
  > }
  > EOF
  $ sinterc run empty.sn
  0
  $ ls
  empty.sn
  io.sn
  out.txt

参数类型不对：

  $ cat > bad.sn <<'EOF'
  > use std.fs
  > 
  > fn main() {
  >     let r = fs.read(3)
  > }
  > EOF
  $ sinterc bad.sn 2>&1 | grep -c 错误
  1
