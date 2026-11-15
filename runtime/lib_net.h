/* std.net：套接字（Lean `Library.FileIO` 的形状：只读写外部的状态、返回新的值）。套接字是文件描述符，
   std/net.sn 把它包进 linear 结构体，所以每个连接都必须显式关闭。地址在这里是文本形式的 IP（"127.0.0.1"、"::1"）加端口。
   出错说明是“操作 地址：系统的说明”。写入断开的连接时不发 SIGPIPE，而是得到 Err。 */
/* e 是 -1 时说明是“超时” */
static sn_str sn_net_err(const char *what, const char *ip, int64_t port, int e) {
  char b[512]; int n;
  const char *m = e < 0 ? "超时" : strerror(e);
  if (ip && strchr(ip, ':')) n = snprintf(b, sizeof b, "%s [%s]:%lld：%s", what, ip, (long long)port, m);
  else if (ip) n = snprintf(b, sizeof b, "%s %s:%lld：%s", what, ip, (long long)port, m);
  else n = snprintf(b, sizeof b, "%s：%s", what, m);
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
/* 每个套接字在运行时这边的状态：超时（毫秒，0 是不限）和读缓冲（read_line 多读的部分）。按 fd 存，close 时释放。
   一个连接同时只属于一个任务（linear，移进任务），所以拿到条目之后不用加锁；表本身变大时要锁。 */
typedef struct { char *p; int64_t len, cap; int64_t timeout; } sn_sock;
static sn_sock **sn_socks;
static int sn_socks_cap;
static pthread_mutex_t sn_socks_mu = PTHREAD_MUTEX_INITIALIZER;
static sn_sock *sn_sock_of(int fd) {
  pthread_mutex_lock(&sn_socks_mu);
  if (fd >= sn_socks_cap) {
    int c = sn_socks_cap ? sn_socks_cap : 64;
    while (c <= fd) c *= 2;
    sn_sock **t = realloc(sn_socks, (size_t)c * sizeof *t);
    if (!t) sn_panic("内存不足");
    memset(t + sn_socks_cap, 0, (size_t)(c - sn_socks_cap) * sizeof *t);
    sn_socks = t; sn_socks_cap = c;
  }
  if (!sn_socks[fd]) { sn_socks[fd] = calloc(1, sizeof(sn_sock)); if (!sn_socks[fd]) sn_panic("内存不足"); }
  sn_sock *s = sn_socks[fd];
  pthread_mutex_unlock(&sn_socks_mu);
  return s;
}
static int64_t sn_timeout_of(int fd) { return sn_sock_of(fd)->timeout; }
static void u_net_set_timeout(int64_t fd, int64_t ms) { sn_sock_of((int)fd)->timeout = ms > 0 ? ms : 0; }
static int64_t u_net_timeout(int64_t fd) { return sn_timeout_of((int)fd); }
/* 新的套接字：丢掉这个 fd 号以前留下的状态（正常不会有，close 时已经释放） */
static void sn_sock_fresh(int fd) {
  pthread_mutex_lock(&sn_socks_mu);
  if (fd >= 0 && fd < sn_socks_cap && sn_socks[fd]) { free(sn_socks[fd]->p); free(sn_socks[fd]); sn_socks[fd] = 0; }
  pthread_mutex_unlock(&sn_socks_mu);
}
static void u_net_close(int64_t fd) {
  pthread_mutex_lock(&sn_socks_mu);
  if (fd >= 0 && fd < sn_socks_cap && sn_socks[fd]) { free(sn_socks[fd]->p); free(sn_socks[fd]); sn_socks[fd] = 0; }
  pthread_mutex_unlock(&sn_socks_mu);
  close((int)fd);
}
/* ms：connect 的超时（毫秒，<= 0 是不限） */
static SN_RES_I64 sn_net_open(const char *what, const sn_str *ip, int64_t port, int type, bool listen_, int64_t ms) {
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
      if (sn_wait_fd(fd, 1, sn_deadline(ms))) { close(fd); return (SN_RES_I64){ .tag = 1, .u.v1 = sn_net_err(what, ip->p, port, -1) }; }
      socklen_t l = sizeof e;
      if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &e, &l) != 0) e = sn_errno();
      r = e == 0 ? 0 : -1;
    }
    if (r == 0) { int one = 1; setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one); }
  }
  if (r != 0) { close(fd); return (SN_RES_I64){ .tag = 1, .u.v1 = sn_net_err(what, ip->p, port, e) }; }
  sn_sock_fresh(fd);
  return (SN_RES_I64){ .tag = 0, .u.v0 = fd };
}
static SN_RES_I64 u_net_listen(const sn_str *ip, int64_t port) { return sn_net_open("listen", ip, port, SOCK_STREAM, true, 0); }
static SN_RES_I64 u_net_connect(const sn_str *ip, int64_t port, int64_t ms) { return sn_net_open("connect", ip, port, SOCK_STREAM, false, ms); }
static SN_RES_I64 u_net_udp_bind(const sn_str *ip, int64_t port) { return sn_net_open("bind", ip, port, SOCK_DGRAM, false, 0); }
static SN_RES_I64 u_net_accept(int64_t fd) {
  int64_t dl = sn_deadline(sn_timeout_of((int)fd));
  int c;
  for (;;) {
    c = accept((int)fd, 0, 0);
    if (c >= 0) break;
    int e = sn_errno();
    if (e == EINTR || e == ECONNABORTED) continue;
    if (SN_AGAIN(e)) { if (!sn_wait_fd((int)fd, 0, dl)) continue; e = -1; }   /* 超时：落到下面的出错返回 */
    return (SN_RES_I64){ .tag = 1, .u.v1 = sn_net_err("accept", 0, 0, e) };
  }
  sn_nosigpipe(c);
  sn_nonblock(c);   /* Linux 上 accept 得到的套接字不继承 O_NONBLOCK */
  int one = 1; setsockopt(c, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
  sn_sock_fresh(c);
  return (SN_RES_I64){ .tag = 0, .u.v0 = c };
}
/* 最多读 n 个字节；连接的另一端关闭写入之后是空的 Bytes */
/* 从套接字读最多 n 个字节到 dst（等的时候用套接字的超时）：返回读到的字节数（0 是另一端关闭了写入），出错返回 -1，*err 是 errno（超时是 -1） */
static ssize_t sn_recv_raw(int fd, char *dst, int64_t n, int *err) {
  int64_t dl = sn_deadline(sn_timeout_of(fd));
  for (;;) {
    ssize_t k = recv(fd, dst, (size_t)n, 0);
    if (k >= 0) return k;
    int e = sn_errno();
    if (e == EINTR) continue;
    if (SN_AGAIN(e)) { if (!sn_wait_fd(fd, 0, dl)) continue; e = -1; }
    *err = e;
    return -1;
  }
}
/* 最多读 n 个字节：先给 read_line 多读进缓冲的部分；缓冲是空的才读套接字。另一端关闭写入之后是空的 Bytes */
static SN_RES_BYTES u_net_read(int64_t fd, int64_t n) {
  if (n <= 0) sn_panic("read 的大小必须大于 0");
  sn_sock *s = sn_sock_of((int)fd);
  sn_str b = { 0 };
  if (s->len > 0) {
    int64_t k = n < s->len ? n : s->len;
    b.p = sn_alloc((size_t)k + 1); memcpy(b.p, s->p, (size_t)k); b.p[k] = 0; b.len = k;
    memmove(s->p, s->p + k, (size_t)(s->len - k)); s->len -= k;
    return (SN_RES_BYTES){ .tag = 0, .u.v0 = b };
  }
  b.p = sn_alloc((size_t)n + 1);
  int e = 0;
  ssize_t k = sn_recv_raw((int)fd, b.p, n, &e);
  if (k < 0) { sn_free(b.p); return (SN_RES_BYTES){ .tag = 1, .u.v1 = sn_net_err("read", 0, 0, e) }; }
  b.len = k; b.p[k] = 0;
  return (SN_RES_BYTES){ .tag = 0, .u.v0 = b };
}
/* 读一行：到 \n 为止（含 \n）；连接在一行的中间关闭时是已经读到的部分（不含 \n），什么都没有了是空的 Bytes。
   每次从套接字读一块进缓冲，一行之后多读的留在缓冲里给下一次 read / read_line。一行超过 max 字节时 Err */
static SN_RES_BYTES u_net_read_line(int64_t fd, int64_t max) {
  sn_sock *s = sn_sock_of((int)fd);
  int64_t from = 0;
  for (;;) {
    char *q = s->len > from ? memchr(s->p + from, '\n', (size_t)(s->len - from)) : 0;
    int64_t take = q ? (int64_t)(q - s->p) + 1 : -1;
    if (take < 0 && s->len >= max) {
      char m[96]; snprintf(m, sizeof m, "read_line：一行超过 %lld 字节", (long long)max);
      return (SN_RES_BYTES){ .tag = 1, .u.v1 = sn_str_cstr(m) };
    }
    if (take < 0) {
      from = s->len;
      if (s->cap - s->len < 4096) {
        int64_t c = s->cap ? s->cap * 2 : 8192;
        while (c - s->len < 4096) c *= 2;
        char *p = realloc(s->p, (size_t)c);
        if (!p) sn_panic("内存不足");
        s->p = p; s->cap = c;
      }
      int e = 0;
      ssize_t k = sn_recv_raw((int)fd, s->p + s->len, s->cap - s->len, &e);
      if (k < 0) return (SN_RES_BYTES){ .tag = 1, .u.v1 = sn_net_err("read_line", 0, 0, e) };
      if (k > 0) { s->len += k; continue; }
      take = s->len;   /* 另一端关闭了写入：剩下的都给出去 */
    }
    sn_str b = { 0 };
    b.p = sn_alloc((size_t)take + 1); memcpy(b.p, s->p, (size_t)take); b.p[take] = 0; b.len = take;
    memmove(s->p, s->p + take, (size_t)(s->len - take)); s->len -= take;
    return (SN_RES_BYTES){ .tag = 0, .u.v0 = b };
  }
}
/* 全部写完才返回 */
static SN_RES_UNIT u_net_write(int64_t fd, const sn_str *b) {
  int64_t dl = sn_deadline(sn_timeout_of((int)fd));
  int64_t i = 0;
  while (i < b->len) {
    ssize_t k = send((int)fd, b->p + i, (size_t)(b->len - i), MSG_NOSIGNAL);
    if (k < 0) {
      int e = sn_errno();
      if (e == EINTR) continue;
      if (SN_AGAIN(e)) { if (sn_wait_fd((int)fd, 1, dl)) return (SN_RES_UNIT){ .tag = 1, .u.v1 = sn_net_err("write", 0, 0, -1) }; continue; }
      return (SN_RES_UNIT){ .tag = 1, .u.v1 = sn_net_err("write", 0, 0, e) };
    }
    i += k;
  }
  return (SN_RES_UNIT){ .tag = 0 };
}
static void u_net_shutdown(int64_t fd) { shutdown((int)fd, SHUT_WR); }
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
    if (SN_AGAIN(e)) { sn_wait_fd((int)fd, 1, -1); continue; }
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
  int64_t dl = sn_deadline(sn_timeout_of((int)fd));
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
    if (SN_AGAIN(e)) { if (!sn_wait_fd((int)fd, 0, dl)) continue; e = -1; }   /* 超时：落到下面的出错返回 */
    sn_free(b.p);
    return (SN_RES_BYTES){ .tag = 1, .u.v1 = sn_net_err("recv_from", 0, 0, e) };
  }
  *sn_from_slot() = from;
  b.len = k; b.p[k] = 0;
  return (SN_RES_BYTES){ .tag = 0, .u.v0 = b };
}
static sn_str u_net_recv_ip(void) { return sn_addr_ip(sn_from_slot()); }
static int64_t u_net_recv_port(void) { return sn_addr_port(sn_from_slot()); }
