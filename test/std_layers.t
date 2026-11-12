标准库的分层。内建函数（编译器实现的 str_len、vec_push、read_line…）只有标准库能直接调用，用户代码写成方法或者标准库的函数；
读写外部的内建函数在标准库里也只有 std.io、std.fs、std.env、std.process 和预导入的打印能调用（核心部分和 std.vec、std.map 等都不读写外部）。
预导入的几个是例外，哪里都能调用：vec_new、bytes_new、box、unbox、consume、alloc、text、panic、choose。
v[i]、for、字符串插值展开出的内建调用不算用户写的。

直接调用内建函数，报错里给出该怎么写：

  $ cat > direct.sn <<'EOF'
  > fn main() {
  >     let n = str_len("ab")
  > }
  > EOF
  $ sinterc direct.sn
  direct.sn:2:13: 错误：str_len 是内建函数，标准库以外不能直接调用：写成 s.len()
   2 |     let n = str_len("ab")
     |             ^
  [1]
  $ sinterc --lang=en direct.sn
  direct.sn:2:13: error: str_len is a built-in function and cannot be called directly outside the standard library: write s.len()
   2 |     let n = str_len("ab")
     |             ^
  [1]

  $ cat > io.sn <<'EOF'
  > fn main() {
  >     let l = read_line()
  > }
  > EOF
  $ sinterc io.sn
  io.sn:2:13: 错误：read_line 是内建函数，标准库以外不能直接调用：写成 io.read_line()
   2 |     let l = read_line()
     |             ^
  [1]

  $ cat > conv.sn <<'EOF'
  > fn main() {
  >     let s = str_from_i64(3)
  > }
  > EOF
  $ sinterc conv.sn
  conv.sn:2:13: 错误：str_from_i64 是内建函数，标准库以外不能直接调用：写成 x.to_str() 或 "{x}"
   2 |     let s = str_from_i64(3)
     |             ^
  [1]

预导入的内建函数和语法糖（内建函数的名字是保留的，主文件里不能再定义同名的函数）：

  $ cat > ok.sn <<'EOF'
  > fn main() {
  >     var v: Vec<Box<I64>> = vec_new()
  >     v.push(box(7))
  >     for b in v {
  >         println("{*b}")
  >     }
  >     println("{*v[0] + unbox(box(0))}")
  >     arena |a| {
  >         let t = text(a, "x,y")
  >         println("{t.split(",").len()}")
  >     }
  >     match "b" {
  >         "a" => println("a"),
  >         _ => println("not a")
  >     }
  >     println("{"abc".len()}")
  > }
  > EOF
  $ sinterc run ok.sn
  7
  7
  2
  not a
  3

去掉了的 print_i64、print_bool、print_f64、print_char 也给出新的写法：

  $ cat > old.sn <<'EOF2'
  > fn main() {
  >     print_i64(3)
  > }
  > EOF2
  $ sinterc old.sn
  old.sn:2:5: 错误：print_i64 已经去掉了：写成 println("{x}")
   2 |     print_i64(3)
     |     ^
  [1]
