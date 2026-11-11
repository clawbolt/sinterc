C62：流式读入。read_file_in(a, path)、read_stdin_in(a) 把文件、标准输入（任意字节）直接读进区域 a 里的一块新缓冲区，
得到它的切片，不经过 Str；read_chunk(a, n) 从标准输入最多读 n 个字节，到头时得到空的切片。
新的缓冲区只追加在区域后面（Lean M27 的 fresh_valid、grow_read），区域结束时一起释放。

  $ printf 'apple,3\nbanana,12\ncherry,7\n' > data.csv
  $ cat > file.sn <<'EOF'
  > use std.view
  > 
  > fn total(path: Str) -> Result<I64, Str> {
  >     arena |a| {
  >         let data = read_file_in(a, path)?
  >         var sum = 0
  >         for line in view.lines(data) {
  >             let f = view.split(line, ",")
  >             match view.to_i64(f[1]) {
  >                 .Some(x) => { sum = sum + x },
  >                 .None => {}
  >             }
  >         }
  >         Ok(sum)
  >     }
  > }
  > 
  > fn main() {
  >     match total("data.csv") {
  >         Ok(n) => print_i64(n),
  >         Err(e) => print(e)
  >     }
  >     match total("missing.csv") {
  >         Ok(n) => print_i64(n),
  >         Err(e) => print("出错")
  >     }
  > }
  > EOF
  $ sinterc run file.sn
  22
  出错

标准输入一次读完，任意字节（包括 0）：

  $ cat > stdin.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         let v = read_stdin_in(a)
  >         print_i64(v.len())
  >         print_i64(v.byte(1))
  >         v.print_str()
  >     }
  > }
  > EOF
  $ printf 'a\000bc' | sinterc run stdin.sn | cat -v
  4
  0
  a^@bc
  $ sinterc run stdin.sn < data.csv
  27
  112
  apple,3
  banana,12
  cherry,7

按块读：每次最多 n 个字节，到头时是空的切片：

  $ cat > chunk.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         var n = 0
  >         var lines = 0
  >         var c = read_chunk(a, 4)
  >         while c.len() > 0 {
  >             n = n + c.len()
  >             for i in 0..c.len() {
  >                 if c.byte(i) == 10 { lines = lines + 1 }
  >             }
  >             c = read_chunk(a, 4)
  >         }
  >         print("{n} 字节，{lines} 行")
  >     }
  > }
  > EOF
  $ sinterc run chunk.sn < data.csv
  27 字节，3 行
  $ printf '' | sinterc run chunk.sn
  0 字节，0 行

大小必须大于 0：

  $ cat > zero.sn <<'EOF'
  > fn main() {
  >     arena |a| {
  >         let c = read_chunk(a, 0)
  >     }
  > }
  > EOF
  $ sinterc run zero.sn < data.csv
  panic: read_chunk 的大小必须大于 0
  [101]

第一个参数必须是区域：

  $ cat > notarena.sn <<'EOF'
  > fn main() {
  >     let c = read_stdin_in(1)
  > }
  > EOF
  $ sinterc notarena.sn
  notarena.sn:2:27: 错误：read_stdin_in 的第一个参数必须是区域，I64 不是
   2 |     let c = read_stdin_in(1)
     |                           ^
  [1]
