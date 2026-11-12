/* Sinter 程序的 C 运行时：sinterc 把它原样放在生成的 C 文件开头（构建时由 src/dune 编进编译器）。
   这里只有和程序无关的部分；用到 Option<Str> 等内建实例类型的函数在 sinter_lib.h，按类型生成的类型定义和析构函数在 src/emit_c.ml。 */
#include <stdint.h>
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

/* () 的值：只有一个值，存成一个字节的 0。 */
typedef char sn_unit;
static void sn_panic(const char *m) { fflush(stdout); fprintf(stderr, "panic: %s\n", m); exit(101); }
/* 命令行参数：main 里记下来，args() 读。 */
static int sn_argc = 0;
static char **sn_argv = 0;
static int64_t sn_i_add(int64_t a, int64_t b) { int64_t r; if (__builtin_add_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_i_sub(int64_t a, int64_t b) { int64_t r; if (__builtin_sub_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_i_mul(int64_t a, int64_t b) { int64_t r; if (__builtin_mul_overflow(a, b, &r)) sn_panic("整数溢出"); return r; }
static int64_t sn_i_div(int64_t a, int64_t b) { if (b == 0) sn_panic("除以零"); if (a == INT64_MIN && b == -1) sn_panic("整数溢出"); return a / b; }
static int64_t sn_i_rem(int64_t a, int64_t b) { if (b == 0) sn_panic("除以零"); if (a == INT64_MIN && b == -1) sn_panic("整数溢出"); return a % b; }
static int64_t sn_i_neg(int64_t a) { if (a == INT64_MIN) sn_panic("整数溢出"); return -a; }
/* F64：按 IEEE 754，不检查溢出（得到 inf）和除以零（得到 inf 或 NaN）。
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
/* F64 的文字：能还原成同一个值的最短写法；1e-5 到 1e16 之间用定点写法（1500.0、0.1），
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
   --release定义 SN_RELEASE：不计数，sn_live 一直是 0，退出时的检查也就总是通过。 */
static long long sn_live = 0;
#ifdef SN_RELEASE
#define SN_COUNT(e)
#else
#define SN_COUNT(e) e
#endif
static void *sn_alloc(size_t n) { void *p = malloc(n); if (!p) sn_panic("内存不足"); SN_COUNT(sn_live++); return p; }
static void sn_free(void *p) { free(p); SN_COUNT(sn_live--); }
static void *sn_realloc(void *p, size_t n) { if (!p) return sn_alloc(n); void *q = realloc(p, n); if (!q) sn_panic("内存不足"); return q; }
/* 字符串：末尾多一个 0；let 参数传指针，inout 参数传指针并可以改。
   静态字面量（Lean `Strings.Slice`）：lit 是 1 时 p 指向程序里的字符串常量，不占堆——不分配，复制还是字面量，
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
/* 标准错误：先刷新标准输出，这样两者在终端里的先后顺序和程序里一样；stderr 本身不缓冲。 */
static void u_eprint(const sn_str *s) { fflush(stdout); fwrite(s->p, 1, (size_t)s->len, stderr); fputc('\n', stderr); }
static void u_eprint_str(const sn_str *s) { fflush(stdout); fwrite(s->p, 1, (size_t)s->len, stderr); }
static void u_print_byte(int64_t b) { if (b < 0 || b > 255) sn_panic("print_byte 的参数不在 0 到 255 之间"); putchar((int)b); }
/* panic(msg)（Lean `FlowGraph.Panic`）：打印说明后结束进程，不析构任何值。 */
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
/* UTF-8（Lean `Library.Utf8`）：sn_utf8_put 是 `Library.Utf8` 的 encode，sn_utf8_get 是 decode1——解出一个字符，*k 是吃掉的字节数；
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
/* 字符串的第 i 个字节、[a, b) 的子串：越界时 panic，不读内存。 */
static int64_t u_str_byte(const sn_str *s, int64_t i) { return (unsigned char)s->p[sn_idx(s->len, i)]; }
static sn_str u_str_slice(const sn_str *s, int64_t a, int64_t b) {
  if (a < 0 || b < a || b > s->len) sn_panic("下标越界");
  return sn_str_make(s->p + a, b - a);
}
/* 字节串：和字符串的表示相同（末尾也多一个 0），内容可以是任意字节。字节是 0 到 255 的 I64。 */
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
/* 合法的 UTF-8（Lean `Library.Utf8Check` 的 valid）：每一轮解出一个字符，编码回去要正好是吃掉的那几个字节。 */
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
/* std.os：结束进程（和 panic 一样先刷新输出、不析构）；路径是否存在、是不是目录；
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
/* 可以存起来的闭包 Fn(…)：堆上的环境、转发到提升出的函数的 code、析构并释放环境的 drop */
typedef struct { void *env; void (*code)(void); void (*drop)(void *); } sn_fn;
/* 字符串切片 &str in a（Lean `Strings.Slice`）：缓冲区里的一段，不拥有内存。缓冲区由 text(a, s) 放进区域 a
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
/* s.view()：s 的整段，借自 s（Lean `Projection.Return`）：切片活着时 s 被锁住，缓冲区不会变 */
static sn_view u_str_view(const sn_str *s) { return (sn_view){ s->p, s->len }; }
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

/* Vec 的内建函数：src/emit_c.ml 为每个用到的（函数, Vec 的 C 类型）用下面的宏生成一个（区域名不同的元素类型共用一个）。
   V 是 Vec 的 C 类型，T 是元素的 C 类型，O 是 Option<元素> 的 C 类型。push 满了就把容量翻倍，pop 空的时候返回 .None。 */
#define SN_VEC_NEW(fn, V) static V fn(void) { return (V){ 0, 0, 0 }; }
#define SN_VEC_PUSH(fn, V, T) \
  static void fn(V *v, T x) { \
    if (v->len == v->cap) { int64_t c = v->cap ? v->cap * 2 : 4; v->data = sn_realloc(v->data, (size_t)c * sizeof(T)); v->cap = c; } \
    v->data[v->len++] = x; \
  }
#define SN_VEC_POP(fn, V, O) \
  static O fn(V *v) { if (v->len == 0) return (O){ .tag = 1 }; v->len--; return (O){ .tag = 0, .u.v0 = v->data[v->len] }; }
#define SN_VEC_SWAP(fn, V, T) \
  static void fn(V *v, const int64_t *i, const int64_t *j) { \
    int64_t a = sn_idx(v->len, *i), b = sn_idx(v->len, *j); T t = v->data[a]; v->data[a] = v->data[b]; v->data[b] = t; \
  }
#define SN_VEC_LEN(fn, V) static int64_t fn(V *v) { return v->len; }

/* 程序的入口：记下命令行参数，运行 main，结束时还有没释放的堆内存就报 leak（--release 时不计数，总是通过）。 */
static int sn_main(int argc, char **argv, void (*m)(void)) {
  sn_argc = argc; sn_argv = argv;
  m();
  if (sn_live != 0) { fprintf(stderr, "leak: %lld 个 Box 没有释放\n", sn_live); return 102; }
  return 0;
}

/* 对照测试（sinterc --batch）的入口：n 个函数，每个按 m 组选择序列各跑一遍，每次跑完检查有没有没释放的内存。 */
typedef void (*sn_test)(void);
static int sn_batch(const sn_test *tests, const char *const *names, int n, const char *const *seqs, int m) {
  int bad = 0;
  for (int i = 0; i < n; i++)
    for (int s = 0; s < m; s++) {
      sn_seq = seqs[s];
      tests[i]();
      if (sn_live != 0) {
        if (bad < 10) printf("%s 在选择序列 \"%s\" 下泄漏 %lld 块\n", names[i], seqs[s], sn_live);
        bad++; sn_live = 0;
      }
    }
  printf("运行：%d 个 sinterc 接受的程序 × %d 组选择，泄漏 %d 次\n", n, m, bad);
  return bad ? 1 : 0;
}
