C36：文件与参数 IO（Lean S3）。args() 是命令行参数（不含程序名），read_file 读整个文件，
write_file 把文件换成新内容（先写临时文件再 rename，失败时原文件不变）；出错时 Err(“路径：系统的说明”)。

  $ cat > io.sn <<'EOF'
  > use std.str
  > 
  > fn save(path: Str, lines: Vec<Str>) -> Result<(), Str> {
  >     let text = str.join(lines, ",")
  >     write_file(path, text)?
  >     Ok(())
  > }
  > 
  > fn main() {
  >     let a = args()
  >     print_i64(vec_len(a))
  >     for x in a {
  >         print(x)
  >     }
  >     match save("out.txt", args()) {
  >         Ok(_) => print("写好了"),
  >         Err(e) => print(e)
  >     }
  >     match read_file("out.txt") {
  >         Ok(s) => print(s),
  >         Err(e) => print(e)
  >     }
  >     match write_file("out.txt", "覆盖") {
  >         Ok(_) => print("又写了一次"),
  >         Err(e) => print(e)
  >     }
  >     match read_file("out.txt") {
  >         Ok(s) => print(s),
  >         Err(e) => print(e)
  >     }
  >     match read_file("nope.txt") {
  >         Ok(s) => print(s),
  >         Err(e) => print(e)
  >     }
  >     match write_file("no/such/dir/x.txt", "x") {
  >         Ok(_) => print("不该成功"),
  >         Err(e) => print(e)
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
  > fn main() {
  >     print_i64(vec_len(args()))
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
  > fn main() {
  >     let r = read_file(3)
  > }
  > EOF
  $ sinterc bad.sn 2>&1 | grep -c 错误
  1
