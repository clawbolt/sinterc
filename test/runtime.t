C 运行时在 runtime/ 下，sinterc 原样放进生成的 C 文件：sinter.h 放在开头，单独就能通过 C 编译器的检查
（没有用到的 static 函数不算问题）；sinter_lib.h 用到 Option<Str> 等内建实例类型，放在类型定义之后，
前面用 #define 给出这些类型的 C 名字。

  $ cc -std=gnu11 -fsyntax-only -Wall -Wno-unused-function -x c ../runtime/sinter.h
  $ cat > hello.sn <<'SN'
  > fn main() {
  >     var v: Vec<I64> = vec_new()
  >     v.push(1)
  >     println("hi {v.len()}")
  > }
  > SN
  $ sinterc hello.sn -o hello.c
  $ n=$(wc -l < ../runtime/sinter.h | tr -d ' '); head -n "$n" hello.c | cmp - ../runtime/sinter.h && echo 开头相同
  开头相同
  $ grep -c "^#define SN_OPT_STR " hello.c
  1
  $ grep -o "^SN_VEC_[A-Z]*(" hello.c | sort -u
  SN_VEC_LEN(
  SN_VEC_NEW(
  SN_VEC_PUSH(
  $ cc -std=gnu11 -w hello.c -o hello && ./hello
  hi 1
