#!/bin/sh
# R5 的对照：每个例子（和 diff.sh 生成的 _batch/suite.sn，有的话）用 SINTER_MONO=check 编译一遍，
# 列出单态化和重新检查的比较（见 src/elab/mono.ml）。只看标准错误里以 mono 开头的行，不影响别的输出。
# 没有实例的例子（examples/bad 的多数：类型检查有错误时不生成实例）不列出。
cd "$(dirname "$0")"
mkdir -p _batch
if ! dune build 2> _batch/mono_build.log; then head -20 _batch/mono_build.log; exit 1; fi
sc=_batch/sinterc
rm -f "$sc"
cp _build/default/bin/main.exe "$sc"
chmod +x "$sc"
for f in examples/ok/*.sn examples/tour/*.sn examples/bad/*.sn; do
  r=$(SINTER_MONO=check "$sc" "$f" -o /dev/null 2>&1 | grep "^mono")
  case "$r" in
    *"实例 0 个"*) ;;
    *"不同 0，重新检查报错 0") echo "$f: $(echo "$r" | sed 's/^mono 对照：//')" ;;
    *) echo "== $f"; echo "$r" ;;
  esac
done
if [ -f _batch/suite.sn ]; then
  echo "== _batch/suite.sn（--batch）"
  SINTER_MONO=check "$sc" _batch/suite.sn --batch -o /dev/null 2>&1 | grep "^mono"
fi
