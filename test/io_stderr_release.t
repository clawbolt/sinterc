写到标准错误的 eprint(s)（末尾加换行）、eprint_str(s)（不加）；sinterc build / run 的 --release
（-O2，定义 SN_RELEASE：不统计分配次数，退出时不做泄漏检查）。

  $ cat > e.sn <<'EOF'
  > fn main() {
  >     print("out 1")
  >     eprint("err 1")
  >     eprint_str("err ")
  >     eprint_str("2\n")
  >     print("out 2")
  > }
  > EOF

标准输出和标准错误分开：

  $ sinterc run e.sn 2>/dev/null
  out 1
  out 2
  $ sinterc run e.sn >/dev/null
  err 1
  err 2

写标准错误之前先刷新标准输出，所以合在一起时顺序和程序里一样（即使标准输出是管道、有缓冲）：

  $ sinterc run e.sn 2>&1 | cat
  out 1
  err 1
  err 2
  out 2

--release：

  $ sinterc build --release e.sn -o e && ./e 2>/dev/null
  out 1
  out 2
  $ sinterc run --release e.sn 2>/dev/null
  out 1
  out 2

只生成 C 时不接受 --release（自己编译时加 -DSN_RELEASE）：

  $ sinterc --release e.sn > /dev/null 2>&1
  [2]
  $ sinterc e.sn -o e.c && grep -c "ifdef SN_RELEASE" e.c
  1
