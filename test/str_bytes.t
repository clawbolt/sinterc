字节串 Bytes（任意字节，和 Str 一样拥有一块堆内存、需要析构、只能移动）。字节是 0 到 255 的 I64，
push、set 超出时 panic，下标越界时 panic。read_bytes、write_bytes 读写二进制文件；str_to_bytes 复制字符串的字节，
str_from_utf8 只在合法的 UTF-8 时得到 .Some（Lean `Library.Utf8Check`：每一轮解出的字符编码回去要正好是吃掉的字节）。

  $ cat > ok.sn <<'EOF'
  > use std.io
  > use std.fs
  > 
  > fn hex(b: Bytes) -> Str {
  >     let ds = "0123456789abcdef"
  >     var s = ""
  >     for i in 0..b.len() {
  >         let x = b.get(i)
  >         if i > 0 { s.push(" ") }
  >         s.push(ds.slice(x / 16, x / 16 + 1))
  >         s.push(ds.slice(x % 16, x % 16 + 1))
  >     }
  >     s
  > }
  > 
  > fn check(b: Bytes) {
  >     match b.decode() {
  >         .Some(s) => println("utf8: {s}"),
  >         .None => println("not utf8: {hex(b)}")
  >     }
  > }
  > 
  > fn main() {
  >     var b = bytes_new()
  >     b.push(0)
  >     b.push(255)
  >     b.push(10)
  >     b.set(2, 128)
  >     println("{b.len()} {b.get(0)} {b.get(1)} {b.get(2)}")
  >     match fs.write_bytes("out.bin", b) {
  >         Ok(_) => println("written"),
  >         Err(e) => println(e)
  >     }
  >     match fs.read_bytes("out.bin") {
  >         Ok(c) => {
  >             println(hex(c))
  >             println("same: {c.eq(b)}")
  >         },
  >         Err(e) => println(e)
  >     }
  >     let t = "a中".to_bytes()
  >     println(hex(t))
  >     check(t)
  >     check(b)
  >     var u = t.slice(0, 3)
  >     check(u)
  >     u.append(t.slice(3, 4))
  >     check(u)
  >     var w = bytes_new()
  >     w.push(239)
  >     w.push(191)
  >     w.push(189)
  >     check(w)
  >     let c = b.clone()
  >     var all: Vec<Bytes> = vec_new()
  >     all.push(c)
  >     all.push(t)
  >     println("{all.len()} {all[1].len()}")
  >     var o = bytes_new()
  >     o.push(104)
  >     o.push(105)
  >     o.push(10)
  >     io.write_bytes(o)
  > }
  > EOF
  $ sinterc run ok.sn
  3 0 255 128
  written
  00 ff 80
  same: true
  61 e4 b8 ad
  utf8: a中
  not utf8: 00 ff 80
  not utf8: 61 e4 b8
  utf8: a中
  utf8: �
  2 4
  hi

字节超出 0 到 255 时 panic：

  $ cat > big.sn <<'EOF'
  > fn main() {
  >     var b = bytes_new()
  >     b.push(256)
  > }
  > EOF
  $ sinterc run big.sn
  panic: 字节不在 0 到 255 之间
  [101]

下标越界时 panic：

  $ cat > oob.sn <<'EOF'
  > fn main() {
  >     let b = "ab".to_bytes()
  >     println("{b.get(2)}")
  > }
  > EOF
  $ sinterc run oob.sn
  panic: 下标越界
  [101]

Bytes 只能移动；== 不能比较字节串：

  $ cat > moved.sn <<'EOF'
  > fn main() {
  >     let a = bytes_new()
  >     let b = a
  >     println("{a.len()}")
  > }
  > EOF
  $ sinterc moved.sn -o /dev/null 2>&1 | head -1
  moved.sn:4:15: 错误：a 已被移动，不能再使用

  $ cat > eq.sn <<'EOF'
  > fn main() {
  >     let a = bytes_new()
  >     let b = bytes_new()
  >     println("{a == b}")
  > }
  > EOF
  $ sinterc eq.sn
  eq.sn:4:15: 错误：字节串用 a.eq(b) 比较
   4 |     println("{a == b}")
     |               ^
  [1]
