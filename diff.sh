#!/bin/sh
# 附录 F 的 C5（后半）：M7（六）的 22400 个程序，Lean、sinterc、运行结果三方对照。
# 1. Lean 把程序打印成 Sinter 源码，每个函数名带着 Lean 的结论（lean/Core/C5b.lean）；
# 2. sinterc --batch 逐个检查：Lean 接受的必须接受；
# 3. sinterc 接受的编译成 C，每个按 15 组选择序列各跑一遍，不能泄漏。
cd "$(dirname "$0")"
dune build || exit 1
rm -f _batch/suite.sn _batch/suite.c _batch/suite _batch/sinterc
(cd ../lean && lake build && lake env lean Core/C5b.lean) || exit 1
fail=0
# _build 里的 main.exe 有时没有执行权限（-rw-------），复制一份再加上权限
mkdir -p _batch
cp _build/default/bin/main.exe _batch/sinterc && chmod +x _batch/sinterc || exit 1
./_batch/sinterc _batch/suite.sn --batch -o _batch/suite.c
case $? in
  0) ;;
  1) fail=1 ;;              # 有“Lean 接受而 sinterc 拒绝”或内部错误，C 照样生成了
  *) echo "sinterc 没能运行"; exit 1 ;;
esac
[ -f _batch/suite.c ] || { echo "没有生成 _batch/suite.c"; exit 1; }
cc -std=gnu11 -w -O1 _batch/suite.c -o _batch/suite || exit 1
./_batch/suite || fail=1
if [ "$fail" = 0 ]; then echo "三方对照全部通过"; else echo "三方对照有不一致"; exit 1; fi
