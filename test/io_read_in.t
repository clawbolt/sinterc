流式读入。read_file_in(a, path)、read_stdin_in(a) 把文件、标准输入（任意字节）直接读进区域 a 里的一块新缓冲区，
得到它的切片，不经过 Str；read_chunk(a, n) 从标准输入最多读 n 个字节，到头时得到空的切片。
新的缓冲区只追加在区域后面（Lean `Strings.Slice` 的 fresh_valid、grow_read），区域结束时一起释放。

  $ printf 'apple,3\nbanana,12\ncherry,7\n' > data.csv
  $ cat > file.sn <<'EOF'
  > 
  > fn total(path: Str) -> Result<I64, Str> {
  >     arena |a| {
  >         let data = read_file_in(a, path)?
  >         var sum = 0
  >         for line in data.lines() {
  >             let f = line.split(",")
  >             match f[1].parse_i64() {
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
  >         Ok(n) => println("{n}"),
  >         Err(e) => println(e)
  >     }
  >     match total("missing.csv") {
  >         Ok(n) => println("{n}"),
  >         Err(e) => println("出错")
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
  >         println("{v.len()}")
  >         println("{v.byte(1)}")
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
  >         println("{n} 字节，{lines} 行")
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

不收集成 Vec 的逐行、逐字段访问：line_end、trim_cr 逐行走，结果和 view.lines 相同；field(v, sep, i) 和 split(v, sep)[i] 相同，
段数不够时是 .None：

  $ cat > walk.sn <<'EOF'
  > 
  > fn check(a: Arena, sink s: Str) {
  >     let data = text(a, s)
  >     let ls = data.lines()
  >     var k = 0
  >     var pos = 0
  >     var same = true
  >     while pos < data.len() {
  >         let e = data.line_end(pos)
  >         let line = data.sub(pos, e).trim_cr()
  >         if k >= ls.len() || !line.eq_view(ls[k]) { same = false }
  >         let parts = line.split(",")
  >         for i in 0..parts.len() + 1 {
  >             match line.field(",", i) {
  >                 .Some(f) => { if i >= parts.len() || !f.eq_view(parts[i]) { same = false } },
  >                 .None => { if i < parts.len() { same = false } }
  >             }
  >         }
  >         k = k + 1
  >         pos = e + 1
  >     }
  >     println("{k} {ls.len()} {same && k == ls.len()}")
  > }
  > 
  > fn main() {
  >     arena |a| {
  >         check(a, "a,b,c\nd,,e\r\n,\n\nlast")
  >         check(a, "x\n")
  >         check(a, "")
  >         check(a, "no newline, at all")
  >     }
  > }
  > EOF
  $ sinterc run walk.sn
  5 5 true
  1 1 true
  0 0 true
  1 1 true
