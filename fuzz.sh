#!/bin/sh
# fmt 和编译器的随机测试（见 fuzz/fuzz.ml）。用法：sh fuzz.sh [轮数] [种子]，默认 20 轮、种子 1。
# 不一致的输入写到 _batch/fuzz/。
cd "$(dirname "$0")"
dune build 2>&1 | head -20
mkdir -p _batch
rm -rf _batch/fuzz
rm -f _batch/fuzz.exe
cp _build/default/fuzz/fuzz.exe _batch/fuzz.exe
chmod +x _batch/fuzz.exe
./_batch/fuzz.exe . "${1:-20}" "${2:-1}"
