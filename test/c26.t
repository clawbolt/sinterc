C26：输出文字。print("…") 打印一个字符串字面量并换行；转义有 \n、\t、\\、\"，可以写中文。
C29 起字符串是一种类型（见 c29.t），print 的实参是 Str。

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

print 的实参不是字符串：

  $ cat > num.sn <<'EOF'
  > fn main() {
  >     print(1)
  > }
  > EOF
  $ sinterc num.sn
  num.sn:2:11: 错误：类型不匹配：需要 Str，实际是 I64
   2 |     print(1)
     |           ^
  [1]

字符串没有结束：

  $ cat > open.sn <<'EOF'
  > fn main() {
  >     print("abc
  > }
  > EOF
  $ sinterc open.sn
  open.sn:2:11: 错误：字符串没有结束（缺少右边的 "，字符串不能跨行）
   2 |     print("abc
     |           ^
  [1]
