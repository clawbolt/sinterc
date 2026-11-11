(* C 后端：从 MIR 生成 C。每个基本块是一个标签，块之间用 goto 跳转；
   MIR 已经按从左到右的顺序把操作数存进了局部变量，这里不需要再处理求值顺序。 *)
open Ast

let prelude = {|#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>
#include <spawn.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>
extern char **environ;

/* () 的值（C37）：只有一个值，存成一个字节的 0。 */
typedef char sn_unit;
static void sn_panic(const char *m) { fflush(stdout); fprintf(stderr, "panic: %s\n", m); exit(101); }
/* 命令行参数（C36）：main 里记下来，args() 读。 */
static int sn_argc = 0;
static char **sn_argv = 0;
static int64_t sn_i_add(int64_t a, int64_t b) { int64_t r; if (__builtin_add_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_i_sub(int64_t a, int64_t b) { int64_t r; if (__builtin_sub_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_i_mul(int64_t a, int64_t b) { int64_t r; if (__builtin_mul_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_i_div(int64_t a, int64_t b) { if (b == 0) sn_panic("除以零"); if (a == INT64_MIN && b == -1) sn_panic("整数溢出"); return a / b; }
static int64_t sn_i_rem(int64_t a, int64_t b) { if (b == 0) sn_panic("除以零"); if (a == INT64_MIN && b == -1) sn_panic("整数溢出"); return a % b; }
static int64_t sn_i_neg(int64_t a) { if (a == INT64_MIN) sn_panic("整数溢出"); return -a; }
/* F64（C47）：按 IEEE 754，不检查溢出（得到 inf）和除以零（得到 inf 或 NaN）。
   运算符按操作数的 C 类型选整数或浮点数的版本（类型检查保证两边相同）。 */
static double sn_f_add(double a, double b) { return a + b; }
static double sn_f_sub(double a, double b) { return a - b; }
static double sn_f_mul(double a, double b) { return a * b; }
static double sn_f_div(double a, double b) { return a / b; }
static double sn_f_neg(double a) { return -a; }
#define sn_add(a, b) _Generic((a), double: sn_f_add, default: sn_i_add)(a, b)
#define sn_sub(a, b) _Generic((a), double: sn_f_sub, default: sn_i_sub)(a, b)
#define sn_mul(a, b) _Generic((a), double: sn_f_mul, default: sn_i_mul)(a, b)
#define sn_div(a, b) _Generic((a), double: sn_f_div, default: sn_i_div)(a, b)
#define sn_rem(a, b) sn_i_rem(a, b)
#define sn_neg(a) _Generic((a), double: sn_f_neg, default: sn_i_neg)(a)
static void u_print_i64(int64_t x) { printf("%lld\n", (long long)x); }
static void u_print_bool(bool b) { puts(b ? "true" : "false"); }
/* F64 的文字（C47）：能还原成同一个值的最短写法；1e-5 到 1e16 之间用定点写法（1500.0、0.1），
   其余用指数写法（1e+20）；整数值带 .0。 */
static int sn_fmt_f64(char *b, size_t n, double x) {
  if (x != x) return snprintf(b, n, "NaN");
  if (x > 1.7976931348623157e308) return snprintf(b, n, "inf");
  if (x < -1.7976931348623157e308) return snprintf(b, n, "-inf");
  double a = x < 0 ? -x : x;
  int k = 0;
  if (a == 0 || (a >= 1e-5 && a < 1e16)) {
    for (int d = 0; d <= 30; d++) { k = snprintf(b, n, "%.*f", d, x); if (strtod(b, 0) == x) break; }
  } else {
    for (int p = 1; p <= 17; p++) { k = snprintf(b, n, "%.*g", p, x); if (strtod(b, 0) == x) break; }
  }
  if (!strpbrk(b, ".e")) { b[k++] = '.'; b[k++] = '0'; b[k] = 0; }
  return k;
}
static void u_print_f64(double x) { char b[64]; sn_fmt_f64(b, sizeof b, x); puts(b); }
static double u_to_f64(int64_t x) { return (double)x; }
static int64_t u_to_i64(double x) { if (!(x >= -9223372036854775808.0 && x < 9223372036854775808.0)) sn_panic("F64 转换成 I64 时超出范围（或是 NaN）"); return (int64_t)x; }
/* 用到 libm（Linux 上要 -lm，sinterc build / run 会加）。写成 static inline：没用到时 C 编译器不生成它们，
   不用浮点函数的程序就不依赖 libm，直接用 cc 编译也能链接。 */
static inline double u_f64_sqrt(double x) { return __builtin_sqrt(x); }
static inline double u_f64_floor(double x) { return __builtin_floor(x); }
static inline double u_f64_ceil(double x) { return __builtin_ceil(x); }
static inline double u_f64_round(double x) { return __builtin_round(x); }
static bool u_f64_is_nan(double x) { return x != x; }
/* 对照测试用：按 sn_seq 里的 0 / 1 依次返回假 / 真，用完之后一直返回假（循环因此一定结束）。 */
static const char *sn_seq = "";
static bool u_choose(void) { if (*sn_seq) return *sn_seq++ == '1'; return false; }
/* 原型阶段统计还没释放的 Box，程序结束时检查有没有泄漏。
   --release（C60）定义 SN_RELEASE：不计数，sn_live 一直是 0，退出时的检查也就总是通过。 */
static long long sn_live = 0;
#ifdef SN_RELEASE
#define SN_COUNT(e)
#else
#define SN_COUNT(e) e
#endif
static void *sn_alloc(size_t n) { void *p = malloc(n); if (!p) sn_panic("内存不足"); SN_COUNT(sn_live++); return p; }
static void sn_free(void *p) { free(p); SN_COUNT(sn_live--); }
static void *sn_realloc(void *p, size_t n) { if (!p) return sn_alloc(n); void *q = realloc(p, n); if (!q) sn_panic("内存不足"); return q; }
/* 字符串（C29）：末尾多一个 0；let 参数传指针，inout 参数传指针并可以改。
   静态字面量（C61，Lean M27 的三）：lit 是 1 时 p 指向程序里的字符串常量，不占堆——不分配，复制还是字面量，
   析构什么都不做；要修改时先用 sn_str_own 复制到堆上。lit 是 0 时拥有一块堆内存。
   lit 和 len 共用 8 个字节（位域），结构体和加标记之前一样是 16 字节，Vec<Str> 不因此变大 */
typedef struct { char *p; int64_t len : 63; uint64_t lit : 1; } sn_str;
_Static_assert(sizeof(sn_str) == 16, "sn_str 应该是 16 字节");
static sn_str sn_str_make(const char *s, int64_t n) { sn_str r = { 0 }; r.p = sn_alloc((size_t)n + 1); memcpy(r.p, s, (size_t)n); r.p[n] = 0; r.len = n; return r; }
static sn_str u_str_lit(const char *s) { sn_str r; r.p = (char *)s; r.len = (int64_t)strlen(s); r.lit = true; return r; }
/* C 字符串（参数、环境变量、目录项）复制成拥有堆内存的 Str：它们的内存不归程序所有 */
static sn_str sn_str_cstr(const char *s) { return sn_str_make(s, (int64_t)strlen(s)); }
static void sn_str_own(sn_str *s) { if (s->lit) *s = sn_str_make(s->p, s->len); }
static void u_print(const sn_str *s) { fwrite(s->p, 1, (size_t)s->len, stdout); putchar('\n'); }
static void u_print_str(const sn_str *s) { fwrite(s->p, 1, (size_t)s->len, stdout); }
/* 标准错误（C60）：先刷新标准输出，这样两者在终端里的先后顺序和程序里一样；stderr 本身不缓冲。 */
static void u_eprint(const sn_str *s) { fflush(stdout); fwrite(s->p, 1, (size_t)s->len, stderr); fputc('\n', stderr); }
static void u_eprint_str(const sn_str *s) { fflush(stdout); fwrite(s->p, 1, (size_t)s->len, stderr); }
static void u_print_byte(int64_t b) { if (b < 0 || b > 255) sn_panic("print_byte 的参数不在 0 到 255 之间"); putchar((int)b); }
/* panic(msg)（C38，Lean M19）：打印说明后结束进程，不析构任何值。 */
static void u_panic(const sn_str *m) { sn_panic(m->p); }
static int64_t u_str_len(const sn_str *s) { return s->len; }
static bool u_str_eq(const sn_str *a, const sn_str *b) { return a->len == b->len && memcmp(a->p, b->p, (size_t)a->len) == 0; }
static sn_str u_str_clone(const sn_str *s) { return s->lit ? *s : sn_str_make(s->p, s->len); }
static sn_str u_str_concat(const sn_str *a, const sn_str *b) {
  sn_str r = { 0 }; r.len = a->len + b->len; r.p = sn_alloc((size_t)r.len + 1);
  memcpy(r.p, a->p, (size_t)a->len); memcpy(r.p + a->len, b->p, (size_t)b->len); r.p[r.len] = 0; return r;
}
static void u_str_push(sn_str *s, const sn_str *t) {
  sn_str_own(s);
  s->p = sn_realloc(s->p, (size_t)(s->len + t->len) + 1);
  memcpy(s->p + s->len, t->p, (size_t)t->len); s->len += t->len; s->p[s->len] = 0;
}
static sn_str u_str_from_i64(int64_t x) { char b[24]; int n = snprintf(b, sizeof b, "%lld", (long long)x); return sn_str_make(b, n); }
static sn_str u_str_from_bool(bool b) { return b ? sn_str_make("true", 4) : sn_str_make("false", 5); }
/* UTF-8（C48，Lean S7）：sn_utf8_put 是 S7 的 encode，sn_utf8_get 是 decode1——解出一个字符，*k 是吃掉的字节数；
   不合法的字节（多余的续字节、过长的编码、代理区、超出范围、截断）解成 U+FFFD，只吃一个字节。 */
static int sn_utf8_put(char *b, uint32_t c) {
  if (c < 0x80) { b[0] = (char)c; return 1; }
  if (c < 0x800) { b[0] = (char)(0xC0 + c / 64); b[1] = (char)(0x80 + c % 64); return 2; }
  if (c < 0x10000) { b[0] = (char)(0xE0 + c / 4096); b[1] = (char)(0x80 + c / 64 % 64); b[2] = (char)(0x80 + c % 64); return 3; }
  b[0] = (char)(0xF0 + c / 262144); b[1] = (char)(0x80 + c / 4096 % 64); b[2] = (char)(0x80 + c / 64 % 64); b[3] = (char)(0x80 + c % 64); return 4;
}
static uint32_t sn_utf8_get(const unsigned char *p, int64_t n, int64_t *k) {
  uint32_t b0 = p[0];
  *k = 1;
#define SN_CONT(i) (n > (i) && p[i] >= 0x80 && p[i] < 0xC0)
  if (b0 < 0x80) return b0;
  if (b0 >= 0xC2 && b0 < 0xE0) {
    if (SN_CONT(1)) { *k = 2; return (b0 - 0xC0) * 64 + (p[1] - 0x80); }
    return 0xFFFD;
  }
  if (b0 >= 0xE0 && b0 < 0xF0) {
    if (SN_CONT(1) && SN_CONT(2)) {
      uint32_t c = (b0 - 0xE0) * 4096 + (p[1] - 0x80) * 64 + (p[2] - 0x80);
      if (c >= 0x800 && !(c >= 0xD800 && c <= 0xDFFF)) { *k = 3; return c; }
    }
    return 0xFFFD;
  }
  if (b0 >= 0xF0 && b0 < 0xF5) {
    if (SN_CONT(1) && SN_CONT(2) && SN_CONT(3)) {
      uint32_t c = (b0 - 0xF0) * 262144 + (p[1] - 0x80) * 4096 + (p[2] - 0x80) * 64 + (p[3] - 0x80);
      if (c >= 0x10000 && c < 0x110000) { *k = 4; return c; }
    }
    return 0xFFFD;
  }
#undef SN_CONT
  return 0xFFFD;
}
static void u_print_char(uint32_t c) { char b[5]; int k = sn_utf8_put(b, c); b[k] = 0; puts(b); }
static int64_t u_char_code(uint32_t c) { return (int64_t)c; }
static sn_str u_str_from_char(uint32_t c) { char b[4]; int k = sn_utf8_put(b, c); return sn_str_make(b, k); }
static void u_str_push_char(sn_str *s, uint32_t c) {
  char b[4]; int k = sn_utf8_put(b, c);
  sn_str_own(s);
  s->p = sn_realloc(s->p, (size_t)(s->len + k) + 1);
  memcpy(s->p + s->len, b, (size_t)k); s->len += k; s->p[s->len] = 0;
}
static int64_t u_str_char_count(const sn_str *s) {
  int64_t i = 0, n = 0, k;
  while (i < s->len) { sn_utf8_get((const unsigned char *)s->p + i, s->len - i, &k); i += k; n++; }
  return n;
}
/* Vec 的下标检查：越界时 panic，不读内存。 */
static int64_t sn_idx(int64_t n, int64_t i) { if (i < 0 || i >= n) sn_panic("下标越界"); return i; }
/* 字符串的第 i 个字节、[a, b) 的子串（C32）：越界时 panic，不读内存。 */
static int64_t u_str_byte(const sn_str *s, int64_t i) { return (unsigned char)s->p[sn_idx(s->len, i)]; }
static sn_str u_str_slice(const sn_str *s, int64_t a, int64_t b) {
  if (a < 0 || b < a || b > s->len) sn_panic("下标越界");
  return sn_str_make(s->p + a, b - a);
}
/* 字节串（C57）：和字符串的表示相同（末尾也多一个 0），内容可以是任意字节。字节是 0 到 255 的 I64。 */
static int64_t sn_byte(int64_t x) { if (x < 0 || x > 255) sn_panic("字节不在 0 到 255 之间"); return x; }
static sn_str u_bytes_new(void) { return u_str_lit(""); }
static int64_t u_bytes_len(const sn_str *b) { return b->len; }
static int64_t u_bytes_get(const sn_str *b, int64_t i) { return (unsigned char)b->p[sn_idx(b->len, i)]; }
static void u_bytes_set(sn_str *b, int64_t i, int64_t x) { int64_t k = sn_idx(b->len, i); int64_t y = sn_byte(x); sn_str_own(b); b->p[k] = (char)y; }
static void u_bytes_push(sn_str *b, int64_t x) {
  char c = (char)sn_byte(x);
  sn_str_own(b);
  b->p = sn_realloc(b->p, (size_t)b->len + 2); b->p[b->len++] = c; b->p[b->len] = 0;
}
static void u_bytes_append(sn_str *b, const sn_str *c) { u_str_push(b, c); }
static sn_str u_bytes_slice(const sn_str *b, int64_t x, int64_t y) { return u_str_slice(b, x, y); }
static sn_str u_bytes_clone(const sn_str *b) { return u_str_clone(b); }
static bool u_bytes_eq(const sn_str *a, const sn_str *b) { return u_str_eq(a, b); }
static sn_str u_str_to_bytes(const sn_str *s) { return u_str_clone(s); }
static void u_print_bytes(const sn_str *b) { fwrite(b->p, 1, (size_t)b->len, stdout); }
/* 合法的 UTF-8（Lean S11 的 valid）：每一轮解出一个字符，编码回去要正好是吃掉的那几个字节。 */
static bool sn_utf8_valid(const unsigned char *p, int64_t n) {
  int64_t i = 0, k; char e[4];
  while (i < n) {
    uint32_t c = sn_utf8_get(p + i, n - i, &k);
    int m = sn_utf8_put(e, c);
    if (m != k || memcmp(e, p + i, (size_t)k) != 0) return false;
    i += k;
  }
  return true;
}
/* std.os（C58）：结束进程（和 panic 一样先刷新输出、不析构）；路径是否存在、是不是目录；
   字符串按字节比较（给 os_list_dir 排序用）；子进程的参数表（argv[0] 是程序名，不经过 shell）和退出码。 */
static void u_os_exit(int64_t c) { fflush(stdout); exit((int)(c & 255)); }
static bool u_os_exists(const sn_str *p) { struct stat s; return stat(p->p, &s) == 0; }
static bool u_os_is_dir(const sn_str *p) { struct stat s; return stat(p->p, &s) == 0 && S_ISDIR(s.st_mode); }
static int sn_str_cmp(const void *a, const void *b) {
  const sn_str *x = a, *y = b; int64_t n = x->len < y->len ? x->len : y->len;
  int c = memcmp(x->p, y->p, (size_t)n);
  return c ? c : (x->len > y->len) - (x->len < y->len);
}
static int64_t sn_wait_code(pid_t pid) {
  int st;
  while (waitpid(pid, &st, 0) < 0) if (errno != EINTR) return -1;
  if (WIFEXITED(st)) return WEXITSTATUS(st);
  if (WIFSIGNALED(st)) return 128 + WTERMSIG(st);
  return -1;
}
/* 区域：一串内存块，分配时往当前块后面追加，离开 arena 块时整块释放。内存块也计入泄漏检查。 */
typedef struct sn_chunk { struct sn_chunk *next; size_t used, cap; _Alignas(16) char data[]; } sn_chunk;
typedef struct { sn_chunk *head; } sn_arena;
static void *sn_arena_alloc(sn_arena *a, size_t n) {
  n = (n + 15) & ~(size_t)15;
  if (!a->head || a->head->used + n > a->head->cap) {
    size_t cap = n > 4096 ? n : 4096;
    sn_chunk *c = sn_alloc(sizeof(sn_chunk) + cap);
    c->next = a->head; c->used = 0; c->cap = cap; a->head = c;
  }
  void *p = a->head->data + a->head->used;
  a->head->used += n;
  return p;
}
static void sn_arena_free(sn_arena *a) { sn_chunk *c = a->head; while (c) { sn_chunk *n = c->next; sn_free(c); c = n; } a->head = 0; }
/* 可以存起来的闭包 Fn(…)（C59）：堆上的环境、转发到提升出的函数的 code、析构并释放环境的 drop */
typedef struct { void *env; void (*code)(void); void (*drop)(void *); } sn_fn;
/* 字符串切片 &str in a（C61，Lean M27 的一、二）：缓冲区里的一段，不拥有内存。缓冲区由 text(a, s) 放进区域 a
   （字面量本来就在静态存储里，不用复制），区域结束时整块释放；之后不再改动，所以切片一直有效。
   子切片、下标都检查边界（越界时 panic），结果仍在同一块缓冲区里。 */
typedef struct { const char *p; int64_t len; } sn_view;
static sn_view u_text(sn_arena *a, sn_str s) {
  if (s.lit) return (sn_view){ s.p, s.len };
  char *q = sn_arena_alloc(a, (size_t)s.len + 1);
  memcpy(q, s.p, (size_t)s.len); q[s.len] = 0;
  sn_free(s.p);
  return (sn_view){ q, s.len };
}
static int64_t u_view_len(const sn_view *v) { return v->len; }
static int64_t u_view_byte(const sn_view *v, int64_t i) { return (unsigned char)v->p[sn_idx(v->len, i)]; }
static sn_view u_view_sub(const sn_view *v, int64_t a, int64_t b) {
  if (a < 0 || b < a || b > v->len) sn_panic("下标越界");
  return (sn_view){ v->p + a, b - a };
}
static sn_str u_view_to_str(const sn_view *v) { return sn_str_make(v->p, v->len); }
static bool u_view_eq(const sn_view *v, const sn_str *s) { return v->len == s->len && memcmp(v->p, s->p, (size_t)v->len) == 0; }
static bool u_view_eq_view(const sn_view *v, const sn_view *w) { return v->len == w->len && memcmp(v->p, w->p, (size_t)v->len) == 0; }
/* pat 在 from 及以后第一次出现的位置，没有时是 -1；from 不在 0 到 len 之间时 panic */
static int64_t u_view_find(const sn_view *v, const sn_str *pat, int64_t from) {
  if (from < 0 || from > v->len) sn_panic("下标越界");
  if (pat->len == 0) return from;
  for (int64_t i = from; i + pat->len <= v->len; i++)
    if (v->p[i] == pat->p[0] && memcmp(v->p + i, pat->p, (size_t)pat->len) == 0) return i;
  return -1;
}
static void u_view_print_str(const sn_view *v) { fwrite(v->p, 1, (size_t)v->len, stdout); }
|}

(* 泛型实例（名字里有 <）的 C 名字：编号加上去掉符号的写法，和用户写的名字（S_、u_ 开头）不会重名。 *)
let gen_names : (string, string) Hashtbl.t = Hashtbl.create 16

(* 名字本身就是 C 的标识符（只有字母、数字、下划线）时直接用；元组（C45）的实例 (I64, Bool) 等另起名字 *)
let is_ident n = String.for_all (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false) n

let cname prefix n =
  (* 泛型实例（Option<I64>）和别的模块里的名字（geom.area，C34）不是 C 的标识符，另起一个名字 *)
  if is_ident n then prefix ^ n
  else
    match Hashtbl.find_opt gen_names n with
    | Some c -> c
    | None ->
        let clean = String.map (function ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '_') n in
        let c = Printf.sprintf "g%d_%s" (Hashtbl.length gen_names) clean in
        Hashtbl.add gen_names n c;
        c

(* 结构体、枚举。只差区域的实例（Option<&I64 in a> 和 Option<&I64 in b>）是同一个 C 类型。 *)
let sname n = cname "S_" (match Typeck.erase (TStruct n) with TStruct m -> m | _ -> n)
let fname n = cname "u_" n   (* 函数 *)

(* Result<T, E> 的 C 名字由两个类型拼出来（每个用到的 Result 是一个具体的枚举）。 *)
let rec mangle = function
  | TI64 -> "I64"
  | TF64 -> "F64"
  | TChar -> "Char"
  | TBool -> "Bool"
  | TStr -> "Str"
  | TBytes -> "Bytes"
  | TView -> "View"
  | TUnit -> "U"
  | TStruct n -> if is_ident n then n else sname n
  | TBox t -> "B" ^ mangle t
  | TVec t -> "V" ^ mangle t ^ "_"
  | TResult (a, b) -> "R" ^ mangle a ^ "_" ^ mangle b ^ "_"
  | TRef (t, _) -> "P" ^ mangle t
  | TArena _ -> "A"
  | TOFn _ -> "Fn"   (* 所有 Fn(…) 的 C 表示相同 *)
  | TApp _ | TParam _ | TFn _ -> failwith "泛型的类型参数和函数类型不会出现在生成的 C 里"

let rec cty = function
  | TI64 -> "int64_t"
  | TF64 -> "double"
  | TChar -> "uint32_t"
  | TBool -> "bool"
  | TStr -> "sn_str"
  | TBytes -> "sn_str"   (* 字节串（C57）和字符串的表示相同 *)
  | TView -> "sn_view"
  | TUnit -> "sn_unit"   (* 存值的地方（变量、字段、载荷、Vec 元素）占一个字节；函数返回 () 时仍是 void *)
  | TStruct n -> sname n
  | TBox t -> cty t ^ "*"
  | TResult _ as t -> "S_" ^ mangle t
  | TVec _ as t -> "S_" ^ mangle t
  | TRef (TView, _) -> "sn_view"   (* 字符串切片（C61）：指针和长度，按值传 *)
  | TRef (t, _) -> cty t ^ "*"
  | TArena _ -> "sn_arena"
  | TOFn _ -> "sn_fn"
  | TApp _ | TParam _ | TFn _ -> failwith "泛型的类型参数和函数类型不会出现在生成的 C 里"

(* 析构一个值：先析构 Box 里面的值，再释放 Box；字符串释放它的字节；结构体、枚举（含 Result）调用这个类型的析构函数。
   析构函数每个类型一个（type_defs 生成），所以经过 Box 的递归类型（链表、树）的析构是递归调用（Lean M11）。 *)
let drop_fn t = "sn_drop_" ^ cty t

let rec drop_code ty lv =
  match ty with
  | TBox t -> drop_code t ("(*" ^ lv ^ ")") ^ Printf.sprintf "sn_free(%s); " lv
  | TStr | TBytes -> Printf.sprintf "if (!%s.lit) sn_free(%s.p); " lv lv
  | TOFn _ -> Printf.sprintf "%s.drop(%s.env); " lv lv
  | (TStruct _ | TResult _ | TVec _) when Typeck.needs_drop ty -> Printf.sprintf "%s(&%s); " (drop_fn ty) lv
  | TI64 | TF64 | TChar | TBool | TView | TUnit | TRef _ | TArena _ | TStruct _ | TResult _ | TVec _ | TApp _ | TParam _ | TFn _ -> ""

(* 析构函数的函数体：结构体逐个析构字段；枚举按运行时标签析构对应变体的载荷（Lean M8 的 dropOne）；
   Vec 逐个析构元素，再释放缓冲区（Lean M12）。 *)
let drop_body ty lv =
  match ty, Typeck.variants ty with
  | _, Some vs ->
      let cases =
        List.concat
          (List.mapi
             (fun i (_, t) ->
               match t with
               | Some t ->
                   let d = drop_code t (Printf.sprintf "%s.u.v%d" lv i) in
                   if d = "" then [] else [ Printf.sprintf "case %d: %sbreak; " i d ]
               | None -> [])
             vs)
      in
      if cases = [] then "" else Printf.sprintf "switch (%s.tag) { %sdefault: break; } " lv (String.concat "" cases)
  | TVec t, _ ->
      Printf.sprintf "for (int64_t i_ = 0; i_ < %s.len; i_++) { %s} if (%s.data) sn_free(%s.data); " lv
        (drop_code t (lv ^ ".data[i_]")) lv lv
  | TStruct n, None ->
      String.concat ""
        (List.map (fun (f : field) -> drop_code f.fty (lv ^ ".f_" ^ f.fname)) (Hashtbl.find Typeck.struct_tbl n))
  | _ -> ""

(* Vec 内建函数的辅助函数名。 *)
let vec_fn b e = Printf.sprintf "sn_v%s_%s" b (cty (TVec e))

(* 类型定义：先给所有结构体、枚举写前向声明，再按依赖顺序写定义（按值包含的类型先定义；
   Box 指向的类型只需要前向声明，放到后面再处理，所以经过 Box 的递归没有问题）；
   最后是需要析构的类型的析构函数（先写原型，析构函数之间可以互相调用）。 *)
let type_defs (p : program) (fs : Mir.func list) =
  let fwd = Buffer.create 256 and defs = Buffer.create 1024 in
  let protos = Buffer.create 256 and bodies = Buffer.create 1024 in
  let seen = Hashtbl.create 16 in
  let later = Queue.create () in
  let declare t =
    Hashtbl.replace seen (cty t) ();
    Buffer.add_string fwd (Printf.sprintf "typedef struct %s %s;\n" (cty t) (cty t));
    if Typeck.needs_drop t then begin
      Buffer.add_string protos (Printf.sprintf "static void %s(%s *p);\n" (drop_fn t) (cty t));
      Buffer.add_string bodies
        (Printf.sprintf "static void %s(%s *p) { %s}\n" (drop_fn t) (cty t) (drop_body t "(*p)"))
    end
  in
  let rec visit t =
    match t, Typeck.variants t with
    | (TBox t | TRef (t, _)), _ -> Queue.add t later
    | (TStruct _ | TResult _ | TVec _), _ when Hashtbl.mem seen (cty t) -> ()
    | TVec e, _ ->
        (* 动态数组：缓冲区指针、长度、容量；元素类型只需要前向声明 *)
        declare t;
        Queue.add e later;
        Buffer.add_string defs
          (Printf.sprintf "struct %s { %s *data; int64_t len, cap; };\n" (cty t) (cty e))
    | _, Some vs ->
        (* 枚举（含 Result）：标签加上各个载荷的 union *)
        declare t;
        List.iter (fun (_, t) -> Option.iter visit t) vs;
        let pays =
          List.concat
            (List.mapi
               (fun i (_, t) ->
                 match t with Some t -> [ Printf.sprintf " %s v%d;" (cty t) i ] | None -> [])
               vs)
        in
        Buffer.add_string defs
          (Printf.sprintf "struct %s { int64_t tag;%s };\n" (cty t)
             (if pays = [] then "" else Printf.sprintf " union {%s } u;" (String.concat "" pays)))
    | TStruct n, _ ->
        declare t;
        let fields = Hashtbl.find Typeck.struct_tbl n in
        List.iter (fun (f : field) -> visit f.fty) fields;
        Buffer.add_string defs (Printf.sprintf "struct %s {" (sname n));
        List.iter
          (fun (f : field) -> Buffer.add_string defs (Printf.sprintf " %s f_%s;" (cty f.fty) f.fname))
          fields;
        Buffer.add_string defs " };\n"
    | (TI64 | TF64 | TChar | TBool | TStr | TBytes | TView | TUnit | TArena _ | TResult _ | TApp _ | TParam _ | TFn _ | TOFn _), _ -> ()
  in
  List.iter
    (function
      | Struct { name; tparams = []; _ } | Enum { name; tparams = []; _ } -> visit (TStruct name)
      | Struct _ | Enum _ | Fn _ | Trait _ | Impl _ -> ())   (* 泛型定义本身没有 C 类型，用到的实例从函数的局部变量找到 *)
    p;
  List.iter
    (fun (f : Mir.func) ->
      visit f.ret;
      Array.iter (fun (l : Mir.local_info) -> visit l.lty) f.locals)
    fs;
  (* Vec 内建函数的辅助函数用到的类型（vec_pop 返回 Option<T>） *)
  Hashtbl.iter
    (fun _ (b, e) ->
      visit (TVec e);
      if b = "pop" then visit (Typeck.instantiate "Option" [ e ]))
    Typeck.vec_calls;
  (* read_line、str_to_i64 返回的 Option<Str>、Option<I64>（C32）：辅助函数总是生成，类型也总要有 *)
  let opt_str = Typeck.instantiate "Option" [ TStr ] and opt_i64 = Typeck.instantiate "Option" [ TI64 ] in
  let opt_f64 = Typeck.instantiate "Option" [ TF64 ] in
  visit opt_f64;
  (* 字符（C48）：char_from 返回的 Option<Char>，str_chars 返回的 Vec<Char>：辅助函数总是生成，类型也总要有 *)
  let opt_char = Typeck.instantiate "Option" [ TChar ] in
  visit opt_char;
  visit (TVec TChar);
  visit opt_str;
  visit opt_i64;
  (* args、read_file、write_file 的返回类型（C36） *)
  let vstr = TVec TStr and rstr = TResult (TStr, TStr) and runit = TResult (TUnit, TStr) in
  visit vstr;
  visit rstr;
  visit runit;
  (* read_bytes 的返回类型（C57） *)
  let rbytes = TResult (TBytes, TStr) in
  visit rbytes;
  (* std.os 的返回类型（C58） *)
  let rvstr = TResult (TVec TStr, TStr) and ri64 = TResult (TI64, TStr) in
  visit rvstr;
  visit ri64;
  (* read_file_in 的返回类型（C62） *)
  let rview = TResult (TRef (TView, "_"), TStr) in
  visit rview;
  while not (Queue.is_empty later) do
    visit (Queue.pop later)
  done;
  (* Vec 内建函数：每个用到的（函数, 元素的 C 类型）一个辅助函数（区域名不同的元素类型共用一个）。push 满了就把容量翻倍（realloc），
     pop 空的时候返回 .None。 *)
  let helpers = Buffer.create 256 in
  let emitted = Hashtbl.create 16 in
  Hashtbl.iter
    (fun _ (b, e) ->
      if not (Hashtbl.mem emitted (vec_fn b e)) then begin
        Hashtbl.add emitted (vec_fn b e) ();
        let v = cty (TVec e) and te = cty e in
        Buffer.add_string helpers
          (match b with
          | "new" -> Printf.sprintf "static %s %s(void) { return (%s){ 0, 0, 0 }; }\n" v (vec_fn b e) v
          | "push" ->
              Printf.sprintf
                "static void %s(%s *v, %s x) { if (v->len == v->cap) { int64_t c = v->cap ? v->cap * 2 : 4; v->data = sn_realloc(v->data, (size_t)c * sizeof(%s)); v->cap = c; } v->data[v->len++] = x; }\n"
                (vec_fn b e) v te te
          | "pop" ->
              let o = cty (Typeck.instantiate "Option" [ e ]) in
              Printf.sprintf
                "static %s %s(%s *v) { if (v->len == 0) return (%s){ .tag = 1 }; v->len--; return (%s){ .tag = 0, .u.v0 = v->data[v->len] }; }\n"
                o (vec_fn b e) v o o
          | "swap" ->
              Printf.sprintf
                "static void %s(%s *v, const int64_t *i, const int64_t *j) { int64_t a = sn_idx(v->len, *i), b = sn_idx(v->len, *j); %s t = v->data[a]; v->data[a] = v->data[b]; v->data[b] = t; }\n"
                (vec_fn b e) v te
          | _ -> Printf.sprintf "static int64_t %s(%s *v) { return v->len; }\n" (vec_fn b e) v)
      end)
    Typeck.vec_calls;
  (* 读一行：读到换行或文件末尾为止，去掉行尾的 \r；什么都没读到就到了末尾时返回 .None。
     解析整数：可以有一个正负号，后面全是数字；超出 I64 范围时返回 .None。 *)
  let oc = cty opt_char and vc = cty (TVec TChar) in
  Buffer.add_string helpers
    (Printf.sprintf
       "static %s u_char_from(int64_t n) {\n\
       \  if (n < 0 || n >= 0x110000 || (n >= 0xD800 && n <= 0xDFFF)) return (%s){ .tag = 1 };\n\
       \  return (%s){ .tag = 0, .u.v0 = (uint32_t)n };\n\
        }\n\
        static %s u_str_chars(const sn_str *s) {\n\
       \  %s v; v.data = 0; v.len = 0; v.cap = 0;\n\
       \  int64_t i = 0, k;\n\
       \  while (i < s->len) {\n\
       \    uint32_t c = sn_utf8_get((const unsigned char *)s->p + i, s->len - i, &k); i += k;\n\
       \    if (v.len == v.cap) { int64_t m = v.cap ? v.cap * 2 : 4; v.data = sn_realloc(v.data, (size_t)m * sizeof(uint32_t)); v.cap = m; }\n\
       \    v.data[v.len++] = c;\n\
       \  }\n\
       \  return v;\n\
        }\n"
       oc oc oc vc vc);
  let os = cty opt_str and oi = cty opt_i64 and of64 = cty opt_f64 in
  (* 解析浮点数（C47）：整个字符串都要是 strtod 认得的写法，不能有前后的空白 *)
  Buffer.add_string helpers
    (Printf.sprintf
       "static %s u_str_to_f64(const sn_str *s) {\n\
       \  if (s->len == 0 || s->len > 400 || s->p[0] == ' ' || s->p[0] == '\\t' || s->p[0] == '\\n') return (%s){ .tag = 1 };\n\
       \  char b[401]; memcpy(b, s->p, (size_t)s->len); b[s->len] = 0; char *e;\n\
       \  double x = strtod(b, &e);\n\
       \  if (*e != 0) return (%s){ .tag = 1 };\n\
       \  return (%s){ .tag = 0, .u.v0 = x };\n\
        }\n\
        static sn_str u_str_from_f64(double x) { char b[64]; int n = sn_fmt_f64(b, sizeof b, x); return sn_str_make(b, n); }\n"
       of64 of64 of64 of64);
  Buffer.add_string helpers
    (Printf.sprintf
       "static %s u_read_line(void) {\n\
       \  fflush(stdout);\n\
       \  int64_t cap = 16; sn_str r = { 0 }; r.p = sn_alloc((size_t)cap); r.len = 0; int c;\n\
       \  while ((c = getchar()) != EOF && c != '\\n') {\n\
       \    if (r.len + 1 >= cap) { cap *= 2; r.p = sn_realloc(r.p, (size_t)cap); }\n\
       \    r.p[r.len++] = (char)c;\n\
       \  }\n\
       \  if (c == EOF && r.len == 0) { sn_free(r.p); return (%s){ .tag = 1 }; }\n\
       \  if (r.len > 0 && r.p[r.len - 1] == '\\r') r.len--;\n\
       \  r.p[r.len] = 0;\n\
       \  return (%s){ .tag = 0, .u.v0 = r };\n\
        }\n\
        static %s u_str_to_i64(const sn_str *s) {\n\
       \  int64_t i = 0, n = 0; bool neg = false;\n\
       \  if (s->len > 0 && (s->p[0] == '-' || s->p[0] == '+')) { neg = s->p[0] == '-'; i = 1; }\n\
       \  if (i == s->len) return (%s){ .tag = 1 };\n\
       \  for (; i < s->len; i++) {\n\
       \    int d = s->p[i] - '0';\n\
       \    if (d < 0 || d > 9) return (%s){ .tag = 1 };\n\
       \    if (__builtin_mul_overflow(n, 10, &n) || (neg ? __builtin_sub_overflow(n, d, &n) : __builtin_add_overflow(n, d, &n)))\n\
       \      return (%s){ .tag = 1 };\n\
       \  }\n\
       \  return (%s){ .tag = 0, .u.v0 = n };\n\
        }\n"
       os os os oi oi oi oi oi);
  (* 文件与参数（C36，Lean S3）。出错说明是“路径：系统的说明”。
     write_file 先写到 path.sinter-tmp，写完、关闭都成功后 rename 换上去（原子的）；任何一步失败都删掉临时文件、原文件不变。 *)
  let vs = cty vstr and rs = cty rstr and ru = cty runit in
  Buffer.add_string helpers
    (Printf.sprintf
       "static sn_str sn_io_err(const char *path) {\n\
       \  char b[512]; int n = snprintf(b, sizeof b, \"%%s：%%s\", path, strerror(errno));\n\
       \  if (n < 0) n = 0; if (n >= (int)sizeof b) n = (int)sizeof b - 1;\n\
       \  return sn_str_make(b, n);\n\
        }\n\
        static %s u_args(void) {\n\
       \  %s r = { 0, 0, 0 };\n\
       \  int n = sn_argc > 1 ? sn_argc - 1 : 0;\n\
       \  if (n > 0) { r.data = sn_alloc((size_t)n * sizeof(sn_str)); r.cap = n; }\n\
       \  for (int i = 0; i < n; i++) r.data[r.len++] = sn_str_cstr(sn_argv[i + 1]);\n\
       \  return r;\n\
        }\n\
        static %s u_read_file(const sn_str *path) {\n\
       \  FILE *f = fopen(path->p, \"rb\");\n\
       \  if (!f) return (%s){ .tag = 1, .u.v1 = sn_io_err(path->p) };\n\
       \  int64_t cap = 256; sn_str r = { 0 }; r.p = sn_alloc((size_t)cap); r.len = 0; size_t k;\n\
       \  while ((k = fread(r.p + r.len, 1, (size_t)(cap - 1 - r.len), f)) > 0) {\n\
       \    r.len += (int64_t)k;\n\
       \    if (r.len + 1 >= cap) { cap *= 2; r.p = sn_realloc(r.p, (size_t)cap); }\n\
       \  }\n\
       \  if (ferror(f)) { sn_str e = sn_io_err(path->p); fclose(f); sn_free(r.p); return (%s){ .tag = 1, .u.v1 = e }; }\n\
       \  fclose(f);\n\
       \  r.p[r.len] = 0;\n\
       \  return (%s){ .tag = 0, .u.v0 = r };\n\
        }\n\
        static %s u_write_file(const sn_str *path, const sn_str *s) {\n\
       \  size_t n = (size_t)path->len + 16; char *tmp = malloc(n);\n\
       \  if (!tmp) sn_panic(\"内存不足\");\n\
       \  snprintf(tmp, n, \"%%s.sinter-tmp\", path->p);\n\
       \  FILE *f = fopen(tmp, \"wb\");\n\
       \  if (!f) { sn_str e = sn_io_err(path->p); free(tmp); return (%s){ .tag = 1, .u.v1 = e }; }\n\
       \  bool ok = fwrite(s->p, 1, (size_t)s->len, f) == (size_t)s->len;\n\
       \  if (!ok) { sn_str e = sn_io_err(path->p); fclose(f); remove(tmp); free(tmp); return (%s){ .tag = 1, .u.v1 = e }; }\n\
       \  if (fclose(f) != 0 || rename(tmp, path->p) != 0) { sn_str e = sn_io_err(path->p); remove(tmp); free(tmp); return (%s){ .tag = 1, .u.v1 = e }; }\n\
       \  free(tmp);\n\
       \  return (%s){ .tag = 0 };\n\
        }\n"
       vs vs rs rs rs rs ru ru ru ru ru);
  (* 字节串（C57）：read_bytes 就是 read_file 换一个结果类型（表示相同），write_bytes 就是 write_file；
     str_from_utf8 检查过（Lean S11）才复制成字符串 *)
  let rb = cty rbytes in
  Buffer.add_string helpers
    (Printf.sprintf
       "static %s u_read_bytes(const sn_str *path) {\n\
       \  %s r = u_read_file(path); %s o; o.tag = r.tag;\n\
       \  if (r.tag == 0) o.u.v0 = r.u.v0; else o.u.v1 = r.u.v1;\n\
       \  return o;\n\
        }\n\
        static %s u_write_bytes(const sn_str *path, const sn_str *b) { return u_write_file(path, b); }\n\
        static %s u_str_from_utf8(const sn_str *b) {\n\
       \  if (!sn_utf8_valid((const unsigned char *)b->p, b->len)) return (%s){ .tag = 1 };\n\
       \  return (%s){ .tag = 0, .u.v0 = u_str_clone(b) };\n\
        }\n"
       rb rs rb ru os os os);
  (* 流式读入（C62，Lean M27 的二：只在区域后面追加新的缓冲区）。sn_read_all_in 把 f 剩下的内容读进区域里的一块缓冲区：
     普通文件按 fstat 的大小分配（多留一个字节，读完再读一次才知道到头），大小没变时只分配一次、不复制；
     满了就换一块两倍大的（旧的留在区域里，区域结束时一起释放）。read_chunk 最多读 n 个字节，到头时得到空的切片 *)
  let rvw = cty rview in
  Buffer.add_string helpers
    (Printf.sprintf
       "static sn_view sn_read_all_in(sn_arena *a, FILE *f) {\n\
       \  struct stat st; size_t cap = 4096, n = 0, k;\n\
       \  if (fstat(fileno(f), &st) == 0 && S_ISREG(st.st_mode) && st.st_size > 0) cap = (size_t)st.st_size + 2;\n\
       \  char *p = sn_arena_alloc(a, cap);\n\
       \  while ((k = fread(p + n, 1, cap - 1 - n, f)) > 0) {\n\
       \    n += k;\n\
       \    if (n + 1 >= cap) { char *q = sn_arena_alloc(a, cap * 2); memcpy(q, p, n); p = q; cap *= 2; }\n\
       \  }\n\
       \  p[n] = 0;\n\
       \  return (sn_view){ p, (int64_t)n };\n\
        }\n\
        static %s u_read_file_in(sn_arena *a, const sn_str *path) {\n\
       \  FILE *f = fopen(path->p, \"rb\");\n\
       \  if (!f) return (%s){ .tag = 1, .u.v1 = sn_io_err(path->p) };\n\
       \  sn_view v = sn_read_all_in(a, f);\n\
       \  if (ferror(f)) { sn_str e = sn_io_err(path->p); fclose(f); return (%s){ .tag = 1, .u.v1 = e }; }\n\
       \  fclose(f);\n\
       \  return (%s){ .tag = 0, .u.v0 = v };\n\
        }\n\
        static sn_view u_read_stdin_in(sn_arena *a) { fflush(stdout); return sn_read_all_in(a, stdin); }\n\
        static sn_view u_read_chunk(sn_arena *a, int64_t n) {\n\
       \  if (n <= 0) sn_panic(\"read_chunk 的大小必须大于 0\");\n\
       \  fflush(stdout);\n\
       \  char *p = sn_arena_alloc(a, (size_t)n + 1);\n\
       \  size_t k = fread(p, 1, (size_t)n, stdin); p[k] = 0;\n\
       \  return (sn_view){ p, (int64_t)k };\n\
        }\n"
       rvw rvw rvw rvw);
  (* std.os（C58）。出错说明和文件读写一样是“路径：系统的说明”；os_output 的退出码不是 0 时是“程序：退出码 n” *)
  let rv = cty rvstr and ri = cty ri64 in
  Buffer.add_string helpers
    (Printf.sprintf
       "static %s u_os_env(const sn_str *n) {\n\
       \  const char *v = getenv(n->p);\n\
       \  if (!v) return (%s){ .tag = 1 };\n\
       \  return (%s){ .tag = 0, .u.v0 = sn_str_cstr(v) };\n\
        }\n\
        static %s u_os_make_dir(const sn_str *p) {\n\
       \  if (mkdir(p->p, 0777) != 0) return (%s){ .tag = 1, .u.v1 = sn_io_err(p->p) };\n\
       \  return (%s){ .tag = 0 };\n\
        }\n\
        static %s u_os_remove(const sn_str *p) {\n\
       \  if (remove(p->p) != 0) return (%s){ .tag = 1, .u.v1 = sn_io_err(p->p) };\n\
       \  return (%s){ .tag = 0 };\n\
        }\n\
        static %s u_os_list_dir(const sn_str *p) {\n\
       \  DIR *d = opendir(p->p);\n\
       \  if (!d) return (%s){ .tag = 1, .u.v1 = sn_io_err(p->p) };\n\
       \  %s v = { 0, 0, 0 }; struct dirent *e;\n\
       \  while ((e = readdir(d)) != 0) {\n\
       \    if (strcmp(e->d_name, \".\") == 0 || strcmp(e->d_name, \"..\") == 0) continue;\n\
       \    if (v.len == v.cap) { int64_t c = v.cap ? v.cap * 2 : 8; v.data = sn_realloc(v.data, (size_t)c * sizeof(sn_str)); v.cap = c; }\n\
       \    v.data[v.len++] = sn_str_cstr(e->d_name);\n\
       \  }\n\
       \  closedir(d);\n\
       \  if (v.len > 1) qsort(v.data, (size_t)v.len, sizeof(sn_str), sn_str_cmp);\n\
       \  return (%s){ .tag = 0, .u.v0 = v };\n\
        }\n\
        static char **sn_argvec(const sn_str *prog, const %s *args) {\n\
       \  char **a = malloc((size_t)(args->len + 2) * sizeof(char *));\n\
       \  if (!a) sn_panic(\"内存不足\");\n\
       \  a[0] = prog->p;\n\
       \  for (int64_t i = 0; i < args->len; i++) a[i + 1] = args->data[i].p;\n\
       \  a[args->len + 1] = 0;\n\
       \  return a;\n\
        }\n\
        static %s u_os_run(const sn_str *prog, const %s *args) {\n\
       \  fflush(stdout);\n\
       \  char **a = sn_argvec(prog, args); pid_t pid;\n\
       \  int r = posix_spawnp(&pid, prog->p, 0, 0, a, environ);\n\
       \  free(a);\n\
       \  if (r != 0) { errno = r; return (%s){ .tag = 1, .u.v1 = sn_io_err(prog->p) }; }\n\
       \  return (%s){ .tag = 0, .u.v0 = sn_wait_code(pid) };\n\
        }\n\
        static %s u_os_output(const sn_str *prog, const %s *args) {\n\
       \  fflush(stdout);\n\
       \  int fd[2];\n\
       \  if (pipe(fd) != 0) return (%s){ .tag = 1, .u.v1 = sn_io_err(prog->p) };\n\
       \  posix_spawn_file_actions_t fa; posix_spawn_file_actions_init(&fa);\n\
       \  posix_spawn_file_actions_adddup2(&fa, fd[1], 1);\n\
       \  posix_spawn_file_actions_addclose(&fa, fd[0]);\n\
       \  posix_spawn_file_actions_addclose(&fa, fd[1]);\n\
       \  char **a = sn_argvec(prog, args); pid_t pid;\n\
       \  int r = posix_spawnp(&pid, prog->p, &fa, 0, a, environ);\n\
       \  free(a); posix_spawn_file_actions_destroy(&fa); close(fd[1]);\n\
       \  if (r != 0) { close(fd[0]); errno = r; return (%s){ .tag = 1, .u.v1 = sn_io_err(prog->p) }; }\n\
       \  int64_t cap = 256; sn_str s = { 0 }; s.p = sn_alloc((size_t)cap); s.len = 0; ssize_t k;\n\
       \  while ((k = read(fd[0], s.p + s.len, (size_t)(cap - 1 - s.len))) != 0) {\n\
       \    if (k < 0) { if (errno == EINTR) continue; break; }\n\
       \    s.len += (int64_t)k;\n\
       \    if (s.len + 1 >= cap) { cap *= 2; s.p = sn_realloc(s.p, (size_t)cap); }\n\
       \  }\n\
       \  close(fd[0]);\n\
       \  s.p[s.len] = 0;\n\
       \  int64_t c = sn_wait_code(pid);\n\
       \  if (c != 0) {\n\
       \    sn_free(s.p);\n\
       \    char b[512]; int n = snprintf(b, sizeof b, \"%%s：退出码 %%lld\", prog->p, (long long)c);\n\
       \    if (n < 0) n = 0; if (n >= (int)sizeof b) n = (int)sizeof b - 1;\n\
       \    return (%s){ .tag = 1, .u.v1 = sn_str_make(b, n) };\n\
       \  }\n\
       \  return (%s){ .tag = 0, .u.v0 = s };\n\
        }\n"
       os os os ru ru ru ru ru ru rv rv vs rv vs ri vs ri ri rs vs rs rs rs rs);
  Buffer.contents fwd ^ Buffer.contents defs ^ Buffer.contents protos ^ Buffer.contents bodies
  ^ Buffer.contents helpers

(* ---------- 从 MIR 生成 ---------- *)

(* let / inout 参数和投影在 C 里是指针，用到时先解引用。 *)
let local (f : Mir.func) l =
  if f.locals.(l).lref then Printf.sprintf "(*l%d)" l else Printf.sprintf "l%d" l

let place f (p : Mir.place) =
  List.fold_left
    (fun s -> function
      | Mir.PField f -> Printf.sprintf "%s.f_%s" s f
      | Mir.PDeref -> Printf.sprintf "(*%s)" s
      | Mir.PVar i -> Printf.sprintf "%s.u.v%d" s i
      | Mir.PIndex i -> Printf.sprintf "%s.data[sn_idx(%s.len, %s)]" s s (local f i))
    (local f p.base) p.proj

let operand f = function
  | Mir.Const (Mir.CInt n) -> Printf.sprintf "INT64_C(%Ld)" n
  | Mir.Const (Mir.CChar c) -> Printf.sprintf "UINT32_C(%d)" c
  | Mir.Const (Mir.CF64 x) -> Printf.sprintf "%h" x   (* 十六进制浮点数：和字面量的值完全相同 *)
  | Mir.Const (Mir.CBool b) -> if b then "true" else "false"
  | Mir.Const Mir.CUnit -> "0"
  | Mir.Const (Mir.CStr s) ->
      (* C 的字符串字面量：可打印的 ASCII 原样写，其余字节（包括 UTF-8 的中文）写成八进制转义 *)
      let b = Buffer.create (String.length s + 2) in
      Buffer.add_char b '"';
      String.iter
        (fun c ->
          match c with
          | '"' | '\\' -> Buffer.add_char b '\\'; Buffer.add_char b c
          | ' ' .. '~' -> Buffer.add_char b c
          | c -> Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c)))
        s;
      Buffer.add_char b '"';
      Buffer.contents b
  | Mir.Copy p | Mir.Move p -> place f p
  | Mir.Ref p | Mir.RefMut p -> Printf.sprintf "&%s" (place f p)

let rvalue fn rv =
  let operand = operand fn in
  match rv with
  | Mir.Use o -> operand o
  | Mir.Bin (op, a, b) -> (
      let a = operand a and b = operand b in
      let f name = Printf.sprintf "sn_%s(%s, %s)" name a b in
      let c sym = Printf.sprintf "(%s %s %s)" a sym b in
      match op with
      | Add -> f "add" | Sub -> f "sub" | Mul -> f "mul" | Div -> f "div" | Rem -> f "rem"
      | Eq -> c "==" | Ne -> c "!=" | Lt -> c "<" | Le -> c "<=" | Gt -> c ">" | Ge -> c ">="
      | And -> c "&&" | Or -> c "||")
  | Mir.Un (Neg, a) -> Printf.sprintf "sn_neg(%s)" (operand a)
  | Mir.Un (Not, a) -> Printf.sprintf "(!%s)" (operand a)
  | Mir.Call (f, args) ->
      let name = match Hashtbl.find_opt Typeck.vec_calls f with Some (b, e) -> vec_fn b e | None -> fname f in
      Printf.sprintf "%s(%s)" name (String.concat ", " (List.map operand args))
  | Mir.Agg (n, fs) ->
      let fs = List.map (fun (f, o) -> Printf.sprintf ".f_%s = %s" f (operand o)) fs in
      Printf.sprintf "((%s){ %s })" (sname n) (String.concat ", " fs)
  | Mir.Unbox (t, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = %s; %s v_ = *p_; sn_free(p_); v_; })" t (operand o) t
  | Mir.BoxNew (t, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = sn_alloc(sizeof(%s)); *p_ = %s; p_; })" t t (operand o)
  | Mir.MkEnum (t, i, o) ->
      let payload = match o with Some o -> Printf.sprintf ", .u.v%d = %s" i (operand o) | None -> "" in
      Printf.sprintf "((%s){ .tag = %d%s })" (cty t) i payload
  | Mir.Payload (o, i) -> Printf.sprintf "%s.u.v%d" (operand o) i
  | Mir.TagIs (o, i) -> Printf.sprintf "(%s.tag == %d)" (operand o) i
  | Mir.Consume _ -> failwith "Consume 只出现在 Eval 里"
  | Mir.Alloc (t, a, o) ->
      let t = cty t in
      Printf.sprintf "({ %s *p_ = sn_arena_alloc(&%s, sizeof(%s)); *p_ = %s; p_; })" t (place fn a) t
        (operand o)

(* 从入口能走到的块；走不到的块（return 之后的死代码）不输出。 *)
let reachable (f : Mir.func) =
  let seen = Array.make (Array.length f.blocks) false in
  let rec go i =
    if not seen.(i) then begin
      seen.(i) <- true;
      match f.blocks.(i).term with
      | Mir.Goto j -> go j
      | Mir.Branch (_, a, b) -> go a; go b
      | Mir.Return | Mir.Halt | Mir.Unterminated -> ()
    end
  in
  go 0;
  seen

(* 局部变量的 C 类型：指针局部多一层 *。 *)
let lty (l : Mir.local_info) = if l.lref then cty l.lty ^ "*" else cty l.lty

let signature (f : Mir.func) =
  let ps =
    if f.params = [] then "void"
    else
      String.concat ", "
        (List.map (fun l -> Printf.sprintf "%s l%d" (lty f.locals.(l)) l) f.params)
  in
  Printf.sprintf "static %s %s(%s)" (if f.ret = TUnit then "void" else cty f.ret) (fname f.name) ps

let func b (f : Mir.func) =
  let add s = Buffer.add_string b s in
  add (signature f ^ " {\n");
  Array.iteri
    (fun i (l : Mir.local_info) ->
      if not (List.mem i f.params) then
        add (Printf.sprintf "  %s l%d;%s\n" (lty l) i
               (if l.lname = "" then "" else Printf.sprintf "  /* %s */" l.lname)))
    f.locals;
  let seen = reachable f in
  let place = place f and operand = operand f and rvalue = rvalue f in
  Array.iteri
    (fun i (blk : Mir.block) ->
      if seen.(i) then begin
        add (Printf.sprintf "bb%d: ;\n" i);
        List.iter
          (function
            | Mir.Assign (p, rv) -> add (Printf.sprintf "  %s = %s;\n" (place p) (rvalue rv))
            | Mir.Eval (Mir.Consume (t, o)) -> add (Printf.sprintf "  %s\n" (drop_code t (operand o)))
            | Mir.Eval (Mir.Payload _) -> ()   (* 只把值标记为已移走，没有代码 *)
            | Mir.Eval rv -> add (Printf.sprintf "  %s;\n" (rvalue rv))
            | Mir.Proj (q, p) -> add (Printf.sprintf "  l%d = &%s;\n" q (place p))
            | Mir.DropNow l ->
                add (Printf.sprintf "  %s\n" (drop_code f.locals.(l).lty (local f l)))
            | Mir.DropIf (l, fl) ->
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code f.locals.(l).lty (local f l)))
            | Mir.DropAt p -> add (Printf.sprintf "  %s\n" (drop_code (Mir.place_ty f p) (place p)))
            | Mir.DropField (l, path) ->
                let p = { (Mir.whole l) with proj = List.map (fun g -> Mir.PField g) path } in
                add (Printf.sprintf "  %s\n" (drop_code (Mir.place_ty f p) (place p)))
            | Mir.DropFieldIf (l, path, fl) ->
                let p = { (Mir.whole l) with proj = List.map (fun g -> Mir.PField g) path } in
                add (Printf.sprintf "  if (l%d) { %s}\n" fl (drop_code (Mir.place_ty f p) (place p)))
            | Mir.Live _ | Mir.Raise -> ()
            | Mir.Enter l -> add (Printf.sprintf "  %s.head = 0;\n" (local f l))
            | Mir.Leave l -> add (Printf.sprintf "  sn_arena_free(&%s);\n" (local f l))
            | Mir.Drop _ -> failwith "Drop 应当已由 Own.elaborate 展开")
          blk.stmts;
        match blk.term with
        | Mir.Goto j -> add (Printf.sprintf "  goto bb%d;\n" j)
        | Mir.Branch (o, a, c) ->
            add (Printf.sprintf "  if (%s) goto bb%d; else goto bb%d;\n" (operand o) a c)
        | Mir.Halt -> add "  __builtin_unreachable();\n"   (* u_panic 不会返回 *)
        | Mir.Return | Mir.Unterminated -> (
            match f.ret_local with
            | Some r -> add (Printf.sprintf "  return l%d;\n" r)
            | None -> add "  return;\n")
      end)
    f.blocks;
  add "}\n"

(* Fn(…)（C59，Lean M26）：每个造闭包的 Fn#… 一个环境结构体（捕获按值存进去）、一个转发函数（把环境的字段按指针
   交给提升出的函数，后面接上调用的实参）、一个析构函数（析构每个捕获，释放环境）；每种 Fn 类型一个 call#…，
   按类型的签名经过 code 调用。let / inout 参数在 C 里是指针，sink 参数是值。 *)
let closures () =
  let b = Buffer.create 256 in
  let add s = Buffer.add_string b s in
  let pty (m, t) = if m = MSink then cty t else cty t ^ " *" in
  let rty r = if r = TUnit then "void" else cty r in
  let names = List.sort compare (Hashtbl.fold (fun n _ acc -> n :: acc) Typeck.packs []) in
  List.iteri
    (fun k pk ->
      let (c : Typeck.closure), t = Hashtbl.find Typeck.packs pk in
      match t with
      | TOFn (pts, r) ->
          let fields = List.mapi (fun i (_, _, t) -> Printf.sprintf " %s c%d;" (cty t) i) c.caps in
          add (Printf.sprintf "struct sn_env%d {%s };\n" k (if fields = [] then " char c_;" else String.concat "" fields));
          let ps = List.mapi (fun i p -> Printf.sprintf ", %s a%d" (pty p) i) pts in
          let fwd =
            List.mapi (fun i _ -> Printf.sprintf "&x->c%d" i) c.caps @ List.mapi (fun i _ -> Printf.sprintf "a%d" i) pts
          in
          add
            (Printf.sprintf "static %s sn_code%d(void *e%s) { struct sn_env%d *x = e; (void)x; %s%s(%s); }\n" (rty r) k
               (String.concat "" ps) k
               (if r = TUnit then "" else "return ")
               (fname c.cname) (String.concat ", " fwd));
          let drops = List.mapi (fun i (_, _, t) -> drop_code t (Printf.sprintf "x->c%d" i)) c.caps in
          add
            (Printf.sprintf "static void sn_envdrop%d(void *e) { struct sn_env%d *x = e; (void)x; %ssn_free(e); }\n" k k
               (String.concat "" drops));
          let args = List.mapi (fun i (_, _, t) -> Printf.sprintf "%s c%d" (cty t) i) c.caps in
          let sets = List.mapi (fun i _ -> Printf.sprintf "x->c%d = c%d; " i i) c.caps in
          add
            (Printf.sprintf
               "static sn_fn %s(%s) { struct sn_env%d *x = sn_alloc(sizeof(struct sn_env%d)); %sreturn (sn_fn){ x, (void (*)(void))sn_code%d, sn_envdrop%d }; }\n"
               (fname pk)
               (if args = [] then "void" else String.concat ", " args)
               k k (String.concat "" sets) k k)
      | _ -> ())
    names;
  let calls = List.sort compare (Hashtbl.fold (fun n t acc -> (n, t) :: acc) Typeck.fcalls []) in
  List.iter
    (fun (n, t) ->
      match t with
      | TOFn (pts, r) ->
          let ps = List.mapi (fun i p -> Printf.sprintf ", %s a%d" (pty p) i) pts in
          let cast = Printf.sprintf "(%s (*)(void *%s))" (rty r) (String.concat "" (List.map (fun p -> ", " ^ pty p) pts)) in
          add
            (Printf.sprintf "static %s %s(sn_fn *f%s) { %s(%sf->code)(f->env%s); }\n" (rty r) (fname n) (String.concat "" ps)
               (if r = TUnit then "" else "return ")
               cast
               (String.concat "" (List.mapi (fun i _ -> Printf.sprintf ", a%d" i) pts)))
      | _ -> ())
    calls;
  Buffer.contents b

(* batch：对照测试的函数名。给了的话不调用 u_main，而是每个函数按所有长度不超过 3 的选择序列各跑一遍，
   每次跑完检查有没有没释放的 Box 或区域内存块。 *)
let program ?batch (p : program) (fs : Mir.func list) : string =
  let b = Buffer.create 4096 in
  let add s = Buffer.add_string b s in
  add prelude;
  add "\n";
  add (type_defs p fs);
  List.iter (fun f -> add (signature f ^ ";\n")) fs;
  add (closures ());
  List.iter (func b) fs;
  (match batch with
  | None ->
      add
        "int main(int argc, char **argv) {\n\
        \  sn_argc = argc; sn_argv = argv;\n\
        \  u_main();\n\
        \  if (sn_live != 0) { fprintf(stderr, \"leak: %lld 个 Box 没有释放\\n\", sn_live); return 102; }\n\
        \  return 0;\n\
         }\n"
  | Some names ->
      let rec seqs n = if n = 0 then [ "" ] else "" :: List.concat_map (fun s -> [ "1" ^ s; "0" ^ s ]) (seqs (n - 1)) in
      let seqs = List.sort_uniq compare (seqs 3) in
      add
        (Printf.sprintf "static const char *sn_seqs[] = { %s };\n"
           (String.concat ", " (List.map (Printf.sprintf "\"%s\"") seqs)));
      add "typedef void (*sn_test)(void);\n";
      List.iter (fun n -> add (Printf.sprintf "static void t_%s(void) { (void)u_%s(); }\n" n n)) names;
      add
        (Printf.sprintf "static const sn_test sn_tests[] = { %s };\n"
           (String.concat ", " (List.map (( ^ ) "t_") names)));
      add
        (Printf.sprintf "static const char *sn_names[] = { %s };\n"
           (String.concat ", " (List.map (Printf.sprintf "\"%s\"") names)));
      add
        (Printf.sprintf
           "int main(void) {\n\
           \  int n = %d, m = %d, bad = 0;\n\
           \  for (int i = 0; i < n; i++)\n\
           \    for (int s = 0; s < m; s++) {\n\
           \      sn_seq = sn_seqs[s];\n\
           \      sn_tests[i]();\n\
           \      if (sn_live != 0) {\n\
           \        if (bad < 10) printf(\"%%s 在选择序列 \\\"%%s\\\" 下泄漏 %%lld 块\\n\", sn_names[i], sn_seqs[s], sn_live);\n\
           \        bad++; sn_live = 0;\n\
           \      }\n\
           \    }\n\
           \  printf(\"运行：%%d 个 sinterc 接受的程序 × %%d 组选择，泄漏 %%d 次\\n\", n, m, bad);\n\
           \  return bad ? 1 : 0;\n\
            }\n"
           (List.length names) (List.length seqs)));
  Buffer.contents b
