#!/bin/sh
# T2：用 sinterc fmt --check 检查仓库里的 Sinter 源码（示例、标准库）都已经格式化。
# fmt 自己会确认格式化前后记号相同、再格式化一次不变，否则以退出码 3 拒绝。
cd "$(dirname "$0")"
dune build 2>&1 | head -20
mkdir -p _batch
rm -f _batch/sinterc
cp _build/default/bin/main.exe _batch/sinterc
chmod +x _batch/sinterc
if ./_batch/sinterc fmt --check examples/ok/*.sn examples/bad/*.sn std/*.sn; then
  echo "格式检查通过"
else
  echo "上面的文件需要格式化：./_batch/sinterc fmt -w 文件"
  exit 1
fi
