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

/* std.net：套接字（Lean `Library.FileIO` 的形状：只读写外部的状态、返回新的值）。套接字是文件描述符，
   std/net.sn 把它包进 linear 结构体，所以每个连接都必须显式关闭。地址在这里是文本形式的 IP（"127.0.0.1"、"::1"）加端口。
   出错说明是“操作 地址：系统的说明”。写入断开的连接时不发 SIGPIPE，而是得到 Err。 */
static sn_str sn_net_err(const char *what, const char *ip, int64_t port, int e) {
  char b[512]; int n;
  if (ip && strchr(ip, ':')) n = snprintf(b, sizeof b, "%s [%s]:%lld：%s", what, ip, (long long)port, strerror(e));
  else if (ip) n = snprintf(b, sizeof b, "%s %s:%lld：%s", what, ip, (long long)port, strerror(e));
  else n = snprintf(b, sizeof b, "%s：%s", what, strerror(e));
  if (n < 0) n = 0; if (n >= (int)sizeof b) n = (int)sizeof b - 1;
  return sn_str_make(b, n);
}
/* 文本形式的 IP 和端口 → sockaddr；不是合法的 IP 或端口时返回 0 */
static socklen_t sn_sockaddr(const char *ip, int64_t port, struct sockaddr_storage *ss) {
  memset(ss, 0, sizeof *ss);
  if (port < 0 || port > 65535) return 0;
  struct sockaddr_in *a = (struct sockaddr_in *)ss;
  if (inet_pton(AF_INET, ip, &a->sin_addr) == 1) {
    a->sin_family = AF_INET; a->sin_port = htons((uint16_t)port); return sizeof *a;
  }
  struct sockaddr_in6 *b = (struct sockaddr_in6 *)ss;
  if (inet_pton(AF_INET6, ip, &b->sin6_addr) == 1) {
    b->sin6_family = AF_INET6; b->sin6_port = htons((uint16_t)port); return sizeof *b;
  }
  return 0;
}
static sn_str sn_addr_ip(const struct sockaddr_storage *ss) {
  char b[INET6_ADDRSTRLEN] = "";
  if (ss->ss_family == AF_INET) inet_ntop(AF_INET, &((const struct sockaddr_in *)ss)->sin_addr, b, sizeof b);
  else if (ss->ss_family == AF_INET6) inet_ntop(AF_INET6, &((const struct sockaddr_in6 *)ss)->sin6_addr, b, sizeof b);
  return sn_str_cstr(b);
}
static int64_t sn_addr_port(const struct sockaddr_storage *ss) {
  if (ss->ss_family == AF_INET) return ntohs(((const struct sockaddr_in *)ss)->sin_port);
  if (ss->ss_family == AF_INET6) return ntohs(((const struct sockaddr_in6 *)ss)->sin6_port);
  return 0;
}
static void sn_nosigpipe(int fd) {
#ifdef SO_NOSIGPIPE
  int one = 1; setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#else
  (void)fd;
#endif
}
#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif
/* 规范的文本形式（"::0001" → "::1"）；不是 IP 时 .None */
static SN_OPT_STR u_net_ip_canon(const sn_str *ip) {
  struct sockaddr_storage ss;
  if (!sn_sockaddr(ip->p, 0, &ss)) return (SN_OPT_STR){ .tag = 1 };
  return (SN_OPT_STR){ .tag = 0, .u.v0 = sn_addr_ip(&ss) };
}
/* 域名解析：所有地址的文本形式，按系统给的顺序，去掉重复的 */
static SN_RES_VEC_STR u_net_lookup(const sn_str *host) {
  struct addrinfo hint, *res = 0;
  memset(&hint, 0, sizeof hint); hint.ai_family = AF_UNSPEC; hint.ai_socktype = SOCK_STREAM;
  int e = getaddrinfo(host->p, 0, &hint, &res);
  if (e != 0) {
    char b[512]; int n = snprintf(b, sizeof b, "lookup %s：%s", host->p, gai_strerror(e));
    if (n < 0) n = 0; if (n >= (int)sizeof b) n = (int)sizeof b - 1;
    return (SN_RES_VEC_STR){ .tag = 1, .u.v1 = sn_str_make(b, n) };
  }
  SN_VEC_STR v = { 0, 0, 0 };
  for (struct addrinfo *p = res; p; p = p->ai_next) {
    struct sockaddr_storage ss; memset(&ss, 0, sizeof ss); memcpy(&ss, p->ai_addr, p->ai_addrlen);
    sn_str s = sn_addr_ip(&ss);
    bool dup = false;
    for (int64_t i = 0; i < v.len; i++) if (v.data[i].len == s.len && memcmp(v.data[i].p, s.p, (size_t)s.len) == 0) dup = true;
    if (dup || s.len == 0) { sn_free(s.p); continue; }
    if (v.len == v.cap) { int64_t c = v.cap ? v.cap * 2 : 4; v.data = sn_realloc(v.data, (size_t)c * sizeof(sn_str)); v.cap = c; }
    v.data[v.len++] = s;
  }
  freeaddrinfo(res);
  return (SN_RES_VEC_STR){ .tag = 0, .u.v0 = v };
}
/* 套接字都是非阻塞的：调用得到 EAGAIN 时 sn_wait_fd 等它就绪（任务让出工作线程，主线程用 poll 阻塞），再重试。
   让出之后 errno 要经过 sn_errno 取（见 sinter.h 的调度器）。 */
#define SN_AGAIN(e) ((e) == EAGAIN || (e) == EWOULDBLOCK)
static SN_RES_I64 sn_net_open(const char *what, const sn_str *ip, int64_t port, int type, bool listen_) {
  struct sockaddr_storage ss; socklen_t n = sn_sockaddr(ip->p, port, &ss);
  if (!n) return (SN_RES_I64){ .tag = 1, .u.v1 = sn_net_err(what, ip->p, port, EINVAL) };
  int fd = socket(ss.ss_family, type, 0);
  if (fd < 0) return (SN_RES_I64){ .tag = 1, .u.v1 = sn_net_err(what, ip->p, port, sn_errno()) };
  sn_nosigpipe(fd);
  sn_nonblock(fd);
  int r, e = 0;
  if (listen_ || type == SOCK_DGRAM) {
    int one = 1; setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    r = bind(fd, (struct sockaddr *)&ss, n);
    if (r == 0 && listen_) r = listen(fd, 1024);
    if (r != 0) e = sn_errno();
  } else {
    r = connect(fd, (struct sockaddr *)&ss, n);
    if (r != 0) e = sn_errno();
    if (r != 0 && (e == EINPROGRESS || e == EINTR)) {
      /* 连接在进行中：等可写，再看结果 */
      sn_wait_fd(fd, 1);
      socklen_t l = sizeof e;
      if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &e, &l) != 0) e = sn_errno();
      r = e == 0 ? 0 : -1;
    }
    if (r == 0) { int one = 1; setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one); }
  }
  if (r != 0) { close(fd); return (SN_RES_I64){ .tag = 1, .u.v1 = sn_net_err(what, ip->p, port, e) }; }
  return (SN_RES_I64){ .tag = 0, .u.v0 = fd };
}
static SN_RES_I64 u_net_listen(const sn_str *ip, int64_t port) { return sn_net_open("listen", ip, port, SOCK_STREAM, true); }
static SN_RES_I64 u_net_connect(const sn_str *ip, int64_t port) { return sn_net_open("connect", ip, port, SOCK_STREAM, false); }
static SN_RES_I64 u_net_udp_bind(const sn_str *ip, int64_t port) { return sn_net_open("bind", ip, port, SOCK_DGRAM, false); }
static SN_RES_I64 u_net_accept(int64_t fd) {
  int c;
  for (;;) {
    c = accept((int)fd, 0, 0);
    if (c >= 0) break;
    int e = sn_errno();
    if (e == EINTR || e == ECONNABORTED) continue;
    if (SN_AGAIN(e)) { sn_wait_fd((int)fd, 0); continue; }
    return (SN_RES_I64){ .tag = 1, .u.v1 = sn_net_err("accept", 0, 0, e) };
  }
  sn_nosigpipe(c);
  sn_nonblock(c);   /* Linux 上 accept 得到的套接字不继承 O_NONBLOCK */
  int one = 1; setsockopt(c, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
  return (SN_RES_I64){ .tag = 0, .u.v0 = c };
}
/* 最多读 n 个字节；连接的另一端关闭写入之后是空的 Bytes */
static SN_RES_BYTES u_net_read(int64_t fd, int64_t n) {
  if (n <= 0) sn_panic("read 的大小必须大于 0");
  sn_str b = { 0 }; b.p = sn_alloc((size_t)n + 1);
  ssize_t k;
  for (;;) {
    k = recv((int)fd, b.p, (size_t)n, 0);
    if (k >= 0) break;
    int e = sn_errno();
    if (e == EINTR) continue;
    if (SN_AGAIN(e)) { sn_wait_fd((int)fd, 0); continue; }
    sn_free(b.p);
    return (SN_RES_BYTES){ .tag = 1, .u.v1 = sn_net_err("read", 0, 0, e) };
  }
  b.len = k; b.p[k] = 0;
  return (SN_RES_BYTES){ .tag = 0, .u.v0 = b };
}
/* 全部写完才返回 */
static SN_RES_UNIT u_net_write(int64_t fd, const sn_str *b) {
  int64_t i = 0;
  while (i < b->len) {
    ssize_t k = send((int)fd, b->p + i, (size_t)(b->len - i), MSG_NOSIGNAL);
    if (k < 0) {
      int e = sn_errno();
      if (e == EINTR) continue;
      if (SN_AGAIN(e)) { sn_wait_fd((int)fd, 1); continue; }
      return (SN_RES_UNIT){ .tag = 1, .u.v1 = sn_net_err("write", 0, 0, e) };
    }
    i += k;
  }
  return (SN_RES_UNIT){ .tag = 0 };
}
static void u_net_shutdown(int64_t fd) { shutdown((int)fd, SHUT_WR); }
static void u_net_close(int64_t fd) { close((int)fd); }
static sn_str u_net_local_ip(int64_t fd) {
  struct sockaddr_storage ss; socklen_t n = sizeof ss; memset(&ss, 0, sizeof ss);
  getsockname((int)fd, (struct sockaddr *)&ss, &n); return sn_addr_ip(&ss);
}
static int64_t u_net_local_port(int64_t fd) {
  struct sockaddr_storage ss; socklen_t n = sizeof ss; memset(&ss, 0, sizeof ss);
  getsockname((int)fd, (struct sockaddr *)&ss, &n); return sn_addr_port(&ss);
}
static sn_str u_net_peer_ip(int64_t fd) {
  struct sockaddr_storage ss; socklen_t n = sizeof ss; memset(&ss, 0, sizeof ss);
  getpeername((int)fd, (struct sockaddr *)&ss, &n); return sn_addr_ip(&ss);
}
static int64_t u_net_peer_port(int64_t fd) {
  struct sockaddr_storage ss; socklen_t n = sizeof ss; memset(&ss, 0, sizeof ss);
  getpeername((int)fd, (struct sockaddr *)&ss, &n); return sn_addr_port(&ss);
}
static SN_RES_UNIT u_net_send_to(int64_t fd, const sn_str *b, const sn_str *ip, int64_t port) {
  struct sockaddr_storage ss; socklen_t n = sn_sockaddr(ip->p, port, &ss);
  if (!n) return (SN_RES_UNIT){ .tag = 1, .u.v1 = sn_net_err("send_to", ip->p, port, EINVAL) };
  for (;;) {
    ssize_t k = sendto((int)fd, b->p, (size_t)b->len, MSG_NOSIGNAL, (struct sockaddr *)&ss, n);
    if (k >= 0) break;
    int e = sn_errno();
    if (e == EINTR) continue;
    if (SN_AGAIN(e)) { sn_wait_fd((int)fd, 1); continue; }
    return (SN_RES_UNIT){ .tag = 1, .u.v1 = sn_net_err("send_to", ip->p, port, e) };
  }
  return (SN_RES_UNIT){ .tag = 0 };
}
/* 收一个数据报（最多 n 个字节，多的丢掉）。发送方记在当前任务里（主线程是 sn_from_main），紧接着用 recv_ip、recv_port 取。
   不用线程局部变量：任务等数据时可能换了工作线程 */
static struct sockaddr_storage sn_from_main;
static __attribute__((noinline)) struct sockaddr_storage *sn_from_slot(void) {
  sn_co *t = *sn_cur_slot();
  return t ? &t->from : &sn_from_main;
}
static SN_RES_BYTES u_net_recv_from(int64_t fd, int64_t n) {
  if (n <= 0) sn_panic("recv_from 的大小必须大于 0");
  sn_str b = { 0 }; b.p = sn_alloc((size_t)n + 1);
  struct sockaddr_storage from;
  ssize_t k;
  for (;;) {
    socklen_t l = sizeof from; memset(&from, 0, sizeof from);
    k = recvfrom((int)fd, b.p, (size_t)n, 0, (struct sockaddr *)&from, &l);
    if (k >= 0) break;
    int e = sn_errno();
    if (e == EINTR) continue;
    if (SN_AGAIN(e)) { sn_wait_fd((int)fd, 0); continue; }
    sn_free(b.p);
    return (SN_RES_BYTES){ .tag = 1, .u.v1 = sn_net_err("recv_from", 0, 0, e) };
  }
  *sn_from_slot() = from;
  b.len = k; b.p[k] = 0;
  return (SN_RES_BYTES){ .tag = 0, .u.v0 = b };
}
static sn_str u_net_recv_ip(void) { return sn_addr_ip(sn_from_slot()); }
static int64_t u_net_recv_port(void) { return sn_addr_port(sn_from_slot()); }
