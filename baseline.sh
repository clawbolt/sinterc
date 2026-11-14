#!/bin/sh
# 重构的 R0：记下编译器现在可以观察到的行为，之后每一步都和它逐项比较（见 ARCHITECTURE.md 的“重构”一节）。
# 用法：sh baseline.sh record [--with-lean]          记到 _baseline/（已经有了就拒绝，要重记先删掉它）
#       sh baseline.sh check [--with-c] [--with-lean] 记到 _batch/baseline/，再和 _baseline/ 比较
# 每个程序一组文件，名字是“目录_文件名.后缀”：
#   .diag、.diag.en  只生成 C 时的标准错误和退出码，中文和英文
#   .core            --emit=core 的输出（examples/bad 的加 --unchecked），后面附标准错误和退出码
#   .c               生成的 C；默认不比较（规划允许 C 变化），--with-c 时也比
#   .run             编译成程序运行一次的标准输出、标准错误和退出码（examples/ok 和 examples/tour）
#   timing.txt       examples/ok 全部生成 C 十遍的秒数，只记录不比较，R9 用它检查编译时间
#   lean.txt         --with-lean 时：diff.sh 的对照计数、validate.sh 每个文件的结论
cd "$(dirname "$0")"
mode=$1
shift
withc=0
lean=0
for a in "$@"; do
  case "$a" in
    --with-c) withc=1 ;;
    --with-lean) lean=1 ;;
    *) mode= ;;
  esac
done
case "$mode" in
  record) dst=_baseline ;;
  check) dst=_batch/baseline ;;
  *) echo "用法：sh baseline.sh record [--with-lean] | check [--with-c] [--with-lean]"; exit 2 ;;
esac
if [ "$mode" = record ] && [ -e _baseline ]; then
  echo "_baseline/ 已经有了；要重记的话先删掉它"
  exit 1
fi
mkdir -p _batch
# 构建失败时 _build 里可能留着旧的 main.exe，不能拿它记基线
if ! dune build 2> _batch/baseline_build.log; then
  head -20 _batch/baseline_build.log
  echo "构建失败，没有记基线（找不到 menhir 时先运行 eval \$(opam env)）"
  exit 1
fi
# 和 validate.sh 一样：先删掉旧的再复制，加上执行权限
rm -f _batch/sinterc
cp _build/default/bin/main.exe _batch/sinterc && chmod +x _batch/sinterc || exit 1
sc=$PWD/_batch/sinterc
# 编译器跑不起来时每个程序都会记成“退出码 127”，看起来像记好了
first=$(ls examples/ok/*.sn | head -1)
"$sc" "$first" -o /dev/null > /dev/null 2>&1
if [ $? -ge 126 ]; then
  echo "编译器 _batch/sinterc 运行不了，没有记基线"
  exit 1
fi
run=$PWD/_batch/baseline_run
out=$PWD/$dst
rm -rf "$out" "$run"
mkdir -p "$out" "$run"

# 程序运行时的参数和标准输入，和 test/ 里对应的 cram 测试一致
args_of() {
  case "$1" in
    ok_io_files) echo "input.txt copy.txt" ;;
    tour_http_server) echo "selftest" ;;
    *) echo "" ;;
  esac
}
stdin_of() {
  case "$1" in
    tour_words) echo "$run/words.txt" ;;
    *) echo "$run/input.txt" ;;
  esac
}

one() {
  d=$1
  f=$2
  n=${d}_$(basename "$f" .sn)
  "$sc" "$f" -o "$run/$n.c" > /dev/null 2> "$out/$n.diag"
  echo "退出码 $?" >> "$out/$n.diag"
  SINTER_LANG=en "$sc" "$f" -o /dev/null > /dev/null 2> "$out/$n.diag.en"
  echo "exit $?" >> "$out/$n.diag.en"
  [ -f "$run/$n.c" ] && cp "$run/$n.c" "$out/$n.c"
  if [ "$d" = bad ]; then u=--unchecked; else u=; fi
  "$sc" "$f" --emit=core $u > "$out/$n.core" 2> "$run/$n.err"
  c=$?
  { echo "--- 标准错误"; cat "$run/$n.err"; echo "退出码 $c"; } >> "$out/$n.core"
  [ "$d" = bad ] && return
  if "$sc" build "$f" -o "$run/$n.exe" > /dev/null 2>&1; then
    printf '10\n20\nabc\n-5\n\n7\n' > "$run/input.txt"
    printf 'the cat and the dog\nThe 猫 and the 狗, the end\n' > "$run/words.txt"
    # shellcheck disable=SC2046
    (cd "$run" && "./$n.exe" $(args_of "$n") < "$(stdin_of "$n")" > "$n.stdout" 2> "$n.stderr")
    c=$?
    { cat "$run/$n.stdout"; echo "--- 标准错误"; cat "$run/$n.stderr"; echo "退出码 $c"; } > "$out/$n.run"
  else
    echo "没能编译成程序" > "$out/$n.run"
  fi
}

for f in examples/ok/*.sn; do one ok "$f"; done
for f in examples/bad/*.sn; do one bad "$f"; done
for f in examples/tour/*.sn; do one tour "$f"; done

t0=$(date +%s)
i=0
while [ $i -lt 10 ]; do
  for f in examples/ok/*.sn; do "$sc" "$f" -o /dev/null > /dev/null 2>&1; done
  i=$((i + 1))
done
t1=$(date +%s)
echo "examples/ok 全部生成 C 十遍：$((t1 - t0)) 秒" > "$out/timing.txt"

if [ "$lean" = 1 ]; then
  if [ ! -d ../lean ]; then echo "没有 ../lean，跳过 --with-lean"; else
    { echo "== diff.sh"; sh diff.sh 2>&1 | grep -E "^对照|三方对照"
      echo "== validate.sh"; sh validate.sh 2>&1 | grep -E "^== |error|全部通过|没通过"; } > "$out/lean.txt"
  fi
fi

if [ "$mode" = record ]; then
  echo "基线已记到 _baseline/：$(ls _baseline | awk 'END{print NR}') 个文件；$(cat _baseline/timing.txt)"
  exit 0
fi
echo "基线：$(cat _baseline/timing.txt)"
echo "现在：$(cat "$out/timing.txt")"
skip="-x timing.txt"
[ "$withc" = 1 ] || skip="$skip -x *.c"
[ "$lean" = 1 ] || skip="$skip -x lean.txt"
# shellcheck disable=SC2086
set -f
if diff -r $skip _baseline "$out"; then echo "和基线相同"; else echo "和基线不同（见上面）"; exit 1; fi
