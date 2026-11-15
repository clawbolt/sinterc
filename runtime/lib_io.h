/* 用到内建类型实例（Option<Str>、Result<Str, Str> 等）的运行时函数。生成的 C 文件先写出这些实例的类型定义，
   再用 #define 给出它们的 C 名字（SN_OPT_STR 等，见 src/emit_c.ml 的 type_defs），然后原样放这一段。 */

/* 字符：不是合法码点（超出范围、代理区）时是 .None */
static SN_OPT_CHAR u_char_from(int64_t n) {
  if (n < 0 || n >= 0x110000 || (n >= 0xD800 && n <= 0xDFFF)) return (SN_OPT_CHAR){ .tag = 1 };
  return (SN_OPT_CHAR){ .tag = 0, .u.v0 = (uint32_t)n };
}
/* 按 UTF-8 逐个解码（Lean `Library.Utf8` 的 decode1） */
static SN_VEC_CHAR u_str_chars(const sn_str *s) {
  SN_VEC_CHAR v; v.data = 0; v.len = 0; v.cap = 0;
  int64_t i = 0, k;
  while (i < s->len) {
    uint32_t c = sn_utf8_get((const unsigned char *)s->p + i, s->len - i, &k); i += k;
    if (v.len == v.cap) { int64_t m = v.cap ? v.cap * 2 : 4; v.data = sn_realloc(v.data, (size_t)m * sizeof(uint32_t)); v.cap = m; }
    v.data[v.len++] = c;
  }
  return v;
}

/* 解析浮点数：整个字符串都要是 strtod 认得的写法，不能有前后的空白 */
static SN_OPT_F64 u_str_to_f64(const sn_str *s) {
  if (s->len == 0 || s->len > 400 || s->p[0] == ' ' || s->p[0] == '\t' || s->p[0] == '\n') return (SN_OPT_F64){ .tag = 1 };
  char b[401]; memcpy(b, s->p, (size_t)s->len); b[s->len] = 0; char *e;
  double x = strtod(b, &e);
  if (*e != 0) return (SN_OPT_F64){ .tag = 1 };
  return (SN_OPT_F64){ .tag = 0, .u.v0 = x };
}
static sn_str u_str_from_f64(double x) { char b[64]; int n = sn_fmt_f64(b, sizeof b, x); return sn_str_make(b, n); }

/* 读一行：读到换行或文件末尾为止，去掉行尾的 \r；什么都没读到就到了末尾时返回 .None */
static SN_OPT_STR u_read_line(void) {
  fflush(stdout);
  int64_t cap = 16; sn_str r = { 0 }; r.p = sn_alloc((size_t)cap); r.len = 0; int c;
  while ((c = getchar()) != EOF && c != '\n') {
    if (r.len + 1 >= cap) { cap *= 2; r.p = sn_realloc(r.p, (size_t)cap); }
    r.p[r.len++] = (char)c;
  }
  if (c == EOF && r.len == 0) { sn_free(r.p); return (SN_OPT_STR){ .tag = 1 }; }
  if (r.len > 0 && r.p[r.len - 1] == '\r') r.len--;
  r.p[r.len] = 0;
  return (SN_OPT_STR){ .tag = 0, .u.v0 = r };
}
/* 解析整数：可以有一个正负号，后面全是数字；超出 I64 范围时返回 .None */
static SN_OPT_I64 u_str_to_i64(const sn_str *s) {
  int64_t i = 0, n = 0; bool neg = false;
  if (s->len > 0 && (s->p[0] == '-' || s->p[0] == '+')) { neg = s->p[0] == '-'; i = 1; }
  if (i == s->len) return (SN_OPT_I64){ .tag = 1 };
  for (; i < s->len; i++) {
    int d = s->p[i] - '0';
    if (d < 0 || d > 9) return (SN_OPT_I64){ .tag = 1 };
    if (__builtin_mul_overflow(n, 10, &n) || (neg ? __builtin_sub_overflow(n, d, &n) : __builtin_add_overflow(n, d, &n)))
      return (SN_OPT_I64){ .tag = 1 };
  }
  return (SN_OPT_I64){ .tag = 0, .u.v0 = n };
}

/* 文件与参数（Lean `Library.FileIO`）。出错说明是“路径：系统的说明”。
   write_file 先写到 path.sinter-tmp，写完、关闭都成功后 rename 换上去（原子的）；任何一步失败都删掉临时文件、原文件不变。 */
static sn_str sn_io_err(const char *path) {
  char b[512]; int n = snprintf(b, sizeof b, "%s：%s", path, strerror(errno));
  if (n < 0) n = 0; if (n >= (int)sizeof b) n = (int)sizeof b - 1;
  return sn_str_make(b, n);
}
static SN_VEC_STR u_args(void) {
  SN_VEC_STR r = { 0, 0, 0 };
  int n = sn_argc > 1 ? sn_argc - 1 : 0;
  if (n > 0) { r.data = sn_alloc((size_t)n * sizeof(sn_str)); r.cap = n; }
  for (int i = 0; i < n; i++) r.data[r.len++] = sn_str_cstr(sn_argv[i + 1]);
  return r;
}
static SN_RES_STR u_read_file(const sn_str *path) {
  FILE *f = fopen(path->p, "rb");
  if (!f) return (SN_RES_STR){ .tag = 1, .u.v1 = sn_io_err(path->p) };
  int64_t cap = 256; sn_str r = { 0 }; r.p = sn_alloc((size_t)cap); r.len = 0; size_t k;
  while ((k = fread(r.p + r.len, 1, (size_t)(cap - 1 - r.len), f)) > 0) {
    r.len += (int64_t)k;
    if (r.len + 1 >= cap) { cap *= 2; r.p = sn_realloc(r.p, (size_t)cap); }
  }
  if (ferror(f)) { sn_str e = sn_io_err(path->p); fclose(f); sn_free(r.p); return (SN_RES_STR){ .tag = 1, .u.v1 = e }; }
  fclose(f);
  r.p[r.len] = 0;
  return (SN_RES_STR){ .tag = 0, .u.v0 = r };
}
static SN_RES_UNIT u_write_file(const sn_str *path, const sn_str *s) {
  size_t n = (size_t)path->len + 16; char *tmp = malloc(n);
  if (!tmp) sn_panic("内存不足");
  snprintf(tmp, n, "%s.sinter-tmp", path->p);
  FILE *f = fopen(tmp, "wb");
  if (!f) { sn_str e = sn_io_err(path->p); free(tmp); return (SN_RES_UNIT){ .tag = 1, .u.v1 = e }; }
  bool ok = fwrite(s->p, 1, (size_t)s->len, f) == (size_t)s->len;
  if (!ok) { sn_str e = sn_io_err(path->p); fclose(f); remove(tmp); free(tmp); return (SN_RES_UNIT){ .tag = 1, .u.v1 = e }; }
  if (fclose(f) != 0 || rename(tmp, path->p) != 0) { sn_str e = sn_io_err(path->p); remove(tmp); free(tmp); return (SN_RES_UNIT){ .tag = 1, .u.v1 = e }; }
  free(tmp);
  return (SN_RES_UNIT){ .tag = 0 };
}

/* 字节串：read_bytes 就是 read_file 换一个结果类型（表示相同），write_bytes 就是 write_file；
   str_from_utf8 检查过（Lean `Library.Utf8Check`）才复制成字符串 */
static SN_RES_BYTES u_read_bytes(const sn_str *path) {
  SN_RES_STR r = u_read_file(path); SN_RES_BYTES o; o.tag = r.tag;
  if (r.tag == 0) o.u.v0 = r.u.v0; else o.u.v1 = r.u.v1;
  return o;
}
static SN_RES_UNIT u_write_bytes(const sn_str *path, const sn_str *b) { return u_write_file(path, b); }
static SN_OPT_STR u_str_from_utf8(const sn_str *b) {
  if (!sn_utf8_valid((const unsigned char *)b->p, b->len)) return (SN_OPT_STR){ .tag = 1 };
  return (SN_OPT_STR){ .tag = 0, .u.v0 = u_str_clone(b) };
}

/* 直接读进区域（Lean `Strings.Slice` 的二：只在区域后面追加新的缓冲区）。sn_read_all_in 把 f 剩下的内容读进区域里的一块缓冲区：
   普通文件按 fstat 的大小分配（多留一个字节，读完再读一次才知道到头），大小没变时只分配一次、不复制；
   满了就换一块两倍大的（旧的留在区域里，区域结束时一起释放）。read_chunk 最多读 n 个字节，到头时得到空的切片 */
static sn_view sn_read_all_in(sn_arena *a, FILE *f) {
  struct stat st; size_t cap = 4096, n = 0, k;
  if (fstat(fileno(f), &st) == 0 && S_ISREG(st.st_mode) && st.st_size > 0) cap = (size_t)st.st_size + 2;
  char *p = sn_arena_alloc(a, cap);
  while ((k = fread(p + n, 1, cap - 1 - n, f)) > 0) {
    n += k;
    if (n + 1 >= cap) { char *q = sn_arena_alloc(a, cap * 2); memcpy(q, p, n); p = q; cap *= 2; }
  }
  p[n] = 0;
  return (sn_view){ p, (int64_t)n };
}
static SN_RES_VIEW u_read_file_in(sn_arena *a, const sn_str *path) {
  FILE *f = fopen(path->p, "rb");
  if (!f) return (SN_RES_VIEW){ .tag = 1, .u.v1 = sn_io_err(path->p) };
  sn_view v = sn_read_all_in(a, f);
  if (ferror(f)) { sn_str e = sn_io_err(path->p); fclose(f); return (SN_RES_VIEW){ .tag = 1, .u.v1 = e }; }
  fclose(f);
  return (SN_RES_VIEW){ .tag = 0, .u.v0 = v };
}
static sn_view u_read_stdin_in(sn_arena *a) { fflush(stdout); return sn_read_all_in(a, stdin); }
static sn_view u_read_chunk(sn_arena *a, int64_t n) {
  if (n <= 0) sn_panic("read_chunk 的大小必须大于 0");
  fflush(stdout);
  char *p = sn_arena_alloc(a, (size_t)n + 1);
  size_t k = fread(p, 1, (size_t)n, stdin); p[k] = 0;
  return (sn_view){ p, (int64_t)k };
}

/* std.os。出错说明和文件读写一样是“路径：系统的说明”；os_output 的退出码不是 0 时是“程序：退出码 n” */
static SN_OPT_STR u_os_env(const sn_str *n) {
  const char *v = getenv(n->p);
  if (!v) return (SN_OPT_STR){ .tag = 1 };
  return (SN_OPT_STR){ .tag = 0, .u.v0 = sn_str_cstr(v) };
}
static SN_RES_UNIT u_os_make_dir(const sn_str *p) {
  if (mkdir(p->p, 0777) != 0) return (SN_RES_UNIT){ .tag = 1, .u.v1 = sn_io_err(p->p) };
  return (SN_RES_UNIT){ .tag = 0 };
}
static SN_RES_UNIT u_os_remove(const sn_str *p) {
  if (remove(p->p) != 0) return (SN_RES_UNIT){ .tag = 1, .u.v1 = sn_io_err(p->p) };
  return (SN_RES_UNIT){ .tag = 0 };
}
static SN_RES_VEC_STR u_os_list_dir(const sn_str *p) {
  DIR *d = opendir(p->p);
  if (!d) return (SN_RES_VEC_STR){ .tag = 1, .u.v1 = sn_io_err(p->p) };
  SN_VEC_STR v = { 0, 0, 0 }; struct dirent *e;
  while ((e = readdir(d)) != 0) {
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) continue;
    if (v.len == v.cap) { int64_t c = v.cap ? v.cap * 2 : 8; v.data = sn_realloc(v.data, (size_t)c * sizeof(sn_str)); v.cap = c; }
    v.data[v.len++] = sn_str_cstr(e->d_name);
  }
  closedir(d);
  if (v.len > 1) qsort(v.data, (size_t)v.len, sizeof(sn_str), sn_str_cmp);
  return (SN_RES_VEC_STR){ .tag = 0, .u.v0 = v };
}
static char **sn_argvec(const sn_str *prog, const SN_VEC_STR *args) {
  char **a = malloc((size_t)(args->len + 2) * sizeof(char *));
  if (!a) sn_panic("内存不足");
  a[0] = prog->p;
  for (int64_t i = 0; i < args->len; i++) a[i + 1] = args->data[i].p;
  a[args->len + 1] = 0;
  return a;
}
static SN_RES_I64 u_os_run(const sn_str *prog, const SN_VEC_STR *args) {
  fflush(stdout);
  char **a = sn_argvec(prog, args); pid_t pid;
  int r = posix_spawnp(&pid, prog->p, 0, 0, a, environ);
  free(a);
  if (r != 0) { errno = r; return (SN_RES_I64){ .tag = 1, .u.v1 = sn_io_err(prog->p) }; }
  return (SN_RES_I64){ .tag = 0, .u.v0 = sn_wait_code(pid) };
}
static SN_RES_STR u_os_output(const sn_str *prog, const SN_VEC_STR *args) {
  fflush(stdout);
  int fd[2];
  if (pipe(fd) != 0) return (SN_RES_STR){ .tag = 1, .u.v1 = sn_io_err(prog->p) };
  posix_spawn_file_actions_t fa; posix_spawn_file_actions_init(&fa);
  posix_spawn_file_actions_adddup2(&fa, fd[1], 1);
  posix_spawn_file_actions_addclose(&fa, fd[0]);
  posix_spawn_file_actions_addclose(&fa, fd[1]);
  char **a = sn_argvec(prog, args); pid_t pid;
  int r = posix_spawnp(&pid, prog->p, &fa, 0, a, environ);
  free(a); posix_spawn_file_actions_destroy(&fa); close(fd[1]);
  if (r != 0) { close(fd[0]); errno = r; return (SN_RES_STR){ .tag = 1, .u.v1 = sn_io_err(prog->p) }; }
  int64_t cap = 256; sn_str s = { 0 }; s.p = sn_alloc((size_t)cap); s.len = 0; ssize_t k;
  while ((k = read(fd[0], s.p + s.len, (size_t)(cap - 1 - s.len))) != 0) {
    if (k < 0) { if (errno == EINTR) continue; break; }
    s.len += (int64_t)k;
    if (s.len + 1 >= cap) { cap *= 2; s.p = sn_realloc(s.p, (size_t)cap); }
  }
  close(fd[0]);
  s.p[s.len] = 0;
  int64_t c = sn_wait_code(pid);
  if (c != 0) {
    sn_free(s.p);
    char b[512]; int n = snprintf(b, sizeof b, "%s：退出码 %lld", prog->p, (long long)c);
    if (n < 0) n = 0; if (n >= (int)sizeof b) n = (int)sizeof b - 1;
    return (SN_RES_STR){ .tag = 1, .u.v1 = sn_str_make(b, n) };
  }
  return (SN_RES_STR){ .tag = 0, .u.v0 = s };
}
