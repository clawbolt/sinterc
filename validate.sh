#!/bin/sh
# 附录 F 的 `Checking.Validate`：翻译验证。
# examples/ok 里的程序正常编译，导出的 CFG 必须被 Lean 全部接受；
# examples/bad 里的程序（编译器本来会报错）用 --unchecked 跳过编译器的所有权检查和区域检查后导出，
# Lean 必须独立地拒绝其中至少一个函数。
# 判断只用 Lean `FlowGraph.Region` 已证明健全的 cfgOk，见 lean/Core/Checking/Validate.lean。
set -e
cd "$(dirname "$0")"
dune build
# _build 里的 main.exe 有时没有执行权限（-rw-------），和 diff.sh 一样复制一份再加上权限。
# 先删掉旧的再复制：macOS 会杀掉被原地覆盖的可执行文件（签名缓存对不上，Killed: 9）
mkdir -p _batch
rm -f _batch/sinterc
cp _build/default/bin/main.exe _batch/sinterc
chmod +x _batch/sinterc
out=../lean/Validate
mkdir -p "$out"
rm -f "$out"/*.lean
for f in examples/ok/*.sn; do
  n=$(basename "$f" .sn)
  ./_batch/sinterc "$f" --emit=core -o "$out/ok_$n.lean"
done
for f in examples/bad/*.sn; do
  n=$(basename "$f" .sn)
  ./_batch/sinterc "$f" --emit=core --unchecked -o "$out/bad_$n.lean"
done
cd ../lean
lake build
fail=0
for f in Validate/*.lean; do
  echo "== $f"
  if ! lake env lean "$f"; then fail=1; fi
done
if [ "$fail" = 0 ]; then echo "全部通过"; else echo "有文件没通过"; exit 1; fi
