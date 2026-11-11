C63：逐块读行。std.view 的 line_reader()、next_block(r, a, n) 每次从标准输入最多读 n 个字节，
上一块最后一个换行之后不完整的部分留到下一块前面，得到以换行结束的一段完整的行；每块放在自己的区域里，处理完就释放。
不管怎么分块，逐块得到的行和整个读进来再 view.lines 的行相同（Lean M28 的 stream_eq）。

  $ cat > stream.sn <<'EOF'
  > use std.view
  > 
  > fn main() {
  >     let a = args()
  >     var n = 4
  >     match a[0].to_i64() {
  >         .Some(x) => { n = x },
  >         .None => {}
  >     }
  >     var r = view.line_reader()
  >     var going = true
  >     var k = 0
  >     while going {
  >         arena |b| {
  >             match view.next_block(r, b, n) {
  >                 .Some(block) => {
  >                     for line in view.lines(block) {
  >                         print("{k}:[{line.to_str()}]")
  >                         k = k + 1
  >                     }
  >                 },
  >                 .None => { going = false }
  >             }
  >         }
  >     }
  > }
  > EOF
  $ cat > whole.sn <<'EOF'
  > use std.view
  > 
  > fn main() {
  >     arena |b| {
  >         var k = 0
  >         for line in view.lines(read_stdin_in(b)) {
  >             print("{k}:[{line.to_str()}]")
  >             k = k + 1
  >         }
  >     }
  > }
  > EOF
  $ sinterc build stream.sn -o stream && sinterc build whole.sn -o whole

\r\n 换行、空行、比块长的行、最后没有换行：

  $ printf 'ab,1\r\ncd\n\na line longer than the chunk\nlast' > in.txt
  $ ./whole < in.txt
  0:[ab,1]
  1:[cd]
  2:[]
  3:[a line longer than the chunk]
  4:[last]
  $ ./whole < in.txt > whole.out
  $ for n in 1 2 3 5 6 7 64 4096; do ./stream $n < in.txt > s.out; cmp -s whole.out s.out && echo "$n 相同"; done
  1 相同
  2 相同
  3 相同
  5 相同
  6 相同
  7 相同
  64 相同
  4096 相同

最后有换行、只有换行、空的输入：

  $ printf 'x\ny\n' | ./stream 1
  0:[x]
  1:[y]
  $ printf '\n\n' | ./stream 3
  0:[]
  1:[]
  $ printf '' | ./stream 2
