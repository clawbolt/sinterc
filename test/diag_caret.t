报错时显示出错的那一行，下一行用 ^ 标出列。列号按字节，^ 前的空白按显示宽度：中文占 2 格，制表符照抄。

前面有中文时 ^ 仍然对准：

  $ cat > wide.sn <<'EOF'
  > fn main() {
  >     print(str_concat("你好", y))
  > }
  > EOF
  $ sinterc wide.sn
  wide.sn:2:32: 错误：未定义的变量 y
   2 |     print(str_concat("你好", y))
     |                              ^
  [1]

错误在引入的标准库模块之外时照常显示主文件的行；和整个程序有关的错误（例如缺少 main）不显示源码：

  $ cat > nomain.sn <<'EOF'
  > fn f() { }
  > EOF
  $ sinterc nomain.sn
  nomain.sn:1:1: 错误：缺少 fn main()
  [1]

英文提示同样显示源码：

  $ sinterc --lang=en wide.sn
  wide.sn:2:32: error: undefined variable y
   2 |     print(str_concat("你好", y))
     |                              ^
  [1]
