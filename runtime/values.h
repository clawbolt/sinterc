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
/* 第 i 个字节起解出一个字符（Lean `Library.Utf8` 的 decode1，不合法的字节是 U+FFFD）和它占的字节数：Chars 迭代器用 */
static uint32_t u_view_char_at(const sn_view *v, int64_t i) { int64_t k; sn_idx(v->len, i); return sn_utf8_get((const unsigned char *)v->p + i, v->len - i, &k); }
static int64_t u_view_char_len(const sn_view *v, int64_t i) { int64_t k; sn_idx(v->len, i); sn_utf8_get((const unsigned char *)v->p + i, v->len - i, &k); return k; }
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
