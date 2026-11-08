C26：输出文字。print("…") 打印一个字符串字面量并换行；转义有 \n、\t、\\、\"，可以写中文。
字符串还不是一种类型：字面量只能直接作为 print 的实参，不能存进变量或传给别的函数。

  $ cat > ok.sn <<'EOF'
  > fn main() {
  >     print("你好，Sinter")
  >     print("tab:\tquote:\" backslash:\\")
  >     print("两行\n第二行")
  >     print_i64(1)
  > }
  > EOF
  $ sinterc run ok.sn
  你好，Sinter
  tab:	quote:" backslash:\
  两行
  第二行
  1

字面量存进变量：

  $ cat > var.sn <<'EOF'
  > fn main() {
  >     let s = "x"
  > }
  > EOF
  $ sinterc var.sn
  var.sn:2:13: 错误：字符串字面量暂时只能直接作为 print 的实参，例如 print("hello")
  [1]

print 的实参不是字符串字面量：

  $ cat > num.sn <<'EOF'
  > fn main() {
  >     print(1)
  > }
  > EOF
  $ sinterc num.sn
  num.sn:2:5: 错误：print 的实参必须是一个字符串字面量，例如 print("hello")（整数和布尔值用 print_i64、print_bool）
  [1]

字符串没有结束：

  $ cat > open.sn <<'EOF'
  > fn main() {
  >     print("abc
  > }
  > EOF
  $ sinterc open.sn
  open.sn:2:11: 错误：字符串没有结束（缺少右边的 "，字符串不能跨行）
  [1]
