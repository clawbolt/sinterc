sinterc fmt。按记号格式化：重新缩进（每层 4 个空格），统一 , : = == => -> 等两边的空格，
{ } 里面留一个空格，去掉行尾空白，连续空行合成一个。不改变记号和换行的位置，注释、字符串原样保留。

  $ printf 'use std.math\n\n\n\nfn  add(a:I64 ,b : I64)->I64{\n  a+b   \n}\n// 注释保留\nfn main(){\nlet s="x = {add(1,2)}"   // 插值不动\n        if s.len()==5{println(s)}\n  match add(1,2){\n  3=>println("三"),\n         _ => { }\n  }\n   let v: Vec<I64> = Vec.new()\n}\n\n\n' > messy.sn
  $ sinterc fmt messy.sn
  use std.math
  
  fn add(a: I64, b: I64) -> I64 {
      a+b
  }
  // 注释保留
  fn main() {
      let s = "x = {add(1,2)}"   // 插值不动
      if s.len() == 5 { println(s) }
      match add(1, 2) {
          3 => println("三"),
          _ => {}
      }
      let v: Vec<I64> = Vec.new()
  }

格式化的结果再格式化不变，程序照样能编译运行：

  $ sinterc fmt messy.sn > tidy.sn
  $ sinterc fmt tidy.sn | cmp - tidy.sn
  $ sinterc run tidy.sn
  x = 3
  三

--check 列出需要格式化的文件，退出码 1；-w 写回之后就没有了：

  $ sinterc fmt --check messy.sn tidy.sn
  messy.sn
  [1]
  $ sinterc fmt -w messy.sn
  $ sinterc fmt --check messy.sn tidy.sn
  $ cmp messy.sn tidy.sn
