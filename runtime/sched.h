/* 结构化并发（Lean `Concurrency.Scope`）：scope 是一组任务，scope_join 等它们全部结束（结构体也随之释放）。
   类型检查保证任务和父任务的足迹不冲突（只读共享或移进任务），程序在 C 内存模型里没有数据竞争，
   所以结果和在 spawn 处按顺序执行任务相同；下面的调度方式只影响快慢。
   spawn#… 由 src/emit_c.ml 生成：把捕获放进环境，交给 sn_spawn；任务结束时释放环境。
   scope_keep 只是让只读共享的变量在 scope 结束之前一直被锁住（borrow.ml），没有代码。

   M:N 调度：任务是协程（自己的栈 8 MB，按需占用物理内存，低端一页不可访问），第一次 spawn 时启动和 CPU 核数一样多的工作线程，
   从一个全局队列里取任务运行。任务等套接字时（sn_wait_fd）让出工作线程：登记到 kqueue（macOS、BSD）或 epoll（Linux），
   由轮询线程在可读写时放回队列；任务里的 scope_join 也让出，最后一个子任务结束时放回。主线程不是协程，在这两处直接阻塞。
   文件、标准输入、子进程这些调用仍然阻塞所在的工作线程。
   协程会换工作线程继续运行，所以让出之后不能再用之前取到的线程局部变量的地址（errno 也是）：
   这些都经过 noinline、带内存屏障的函数取（sn_cur_slot、sn_errno），编译器不会把它们合并到让出之前。 */
#include <sys/mman.h>
#include <poll.h>
#include <time.h>
#include <fcntl.h>
#if defined(__APPLE__) || defined(__FreeBSD__)
#include <sys/event.h>
#define SN_KQUEUE 1
#else
#include <sys/epoll.h>
#endif

/* 切换协程：把当前的被调用者保存寄存器压栈，栈指针存进 *from，换到 to 的栈，弹出它的寄存器后返回到它上次切走的地方。
   新协程的栈预先摆成“刚切走”的样子，返回地址是 sn_co_entry。 */
void sn_ctx_switch(void **from, void *to);
#if defined(__APPLE__)
#define SN_ASM_SYM "_sn_ctx_switch"
#else
#define SN_ASM_SYM "sn_ctx_switch"
#endif
#if defined(__x86_64__)
__asm__(".text\n.globl " SN_ASM_SYM "\n.p2align 4\n" SN_ASM_SYM ":\n"
        "pushq %rbp\npushq %rbx\npushq %r12\npushq %r13\npushq %r14\npushq %r15\n"
        "movq %rsp, (%rdi)\nmovq %rsi, %rsp\n"
        "popq %r15\npopq %r14\npopq %r13\npopq %r12\npopq %rbx\npopq %rbp\nret\n");
#elif defined(__aarch64__)
__asm__(".text\n.globl " SN_ASM_SYM "\n.p2align 2\n" SN_ASM_SYM ":\n"
        "sub sp, sp, #176\n"
        "stp x19, x20, [sp, #0]\nstp x21, x22, [sp, #16]\nstp x23, x24, [sp, #32]\nstp x25, x26, [sp, #48]\n"
        "stp x27, x28, [sp, #64]\nstp x29, x30, [sp, #80]\nstp d8, d9, [sp, #96]\nstp d10, d11, [sp, #112]\n"
        "stp d12, d13, [sp, #128]\nstp d14, d15, [sp, #144]\n"
        "mov x9, sp\nstr x9, [x0]\nmov sp, x1\n"
        "ldp x19, x20, [sp, #0]\nldp x21, x22, [sp, #16]\nldp x23, x24, [sp, #32]\nldp x25, x26, [sp, #48]\n"
        "ldp x27, x28, [sp, #64]\nldp x29, x30, [sp, #80]\nldp d8, d9, [sp, #96]\nldp d10, d11, [sp, #112]\n"
        "ldp d12, d13, [sp, #128]\nldp d14, d15, [sp, #144]\n"
        "add sp, sp, #176\nret\n");
#else
#error "Sinter 的调度器只支持 x86_64 和 arm64"
#endif

struct sn_scope;
typedef struct sn_co {
  void *sp;                       /* 切走时的栈指针 */
  char *stack;                    /* mmap 得到的栈（含不可访问的一页） */
  void (*fn)(void *); void *env;  /* 任务函数和它的环境 */
  struct sn_scope *scope;         /* 所属的 scope */
  struct sn_scope *joining;       /* 正在等的 scope（任务里的 scope_join） */
  void (*hook)(struct sn_co *);   /* 让出后由工作线程执行（登记等待）：这时协程的状态已经保存，别人放回队列也安全 */
  int done;
  int wfd, wwrite;                /* 等的套接字和方向 */
  int64_t deadline;               /* 等到什么时候（单调时钟的毫秒），-1 是一直等 */
  int heap_idx;                   /* 在超时的堆里的位置，-1 是不在 */
  int timedout;                   /* 这次等待是超时结束的 */
  struct sn_co *next;             /* 队列、等待链表 */
  struct sockaddr_storage from;   /* 最近一次 recv_from 的发送方（std.net） */
} sn_co;
typedef struct sn_scope { pthread_mutex_t mu; pthread_cond_t cv; int64_t pending; sn_co *waiter; } sn_scope;

static __thread sn_co *sn_cur_co;
static __thread void *sn_worker_sp;
static __attribute__((noinline)) sn_co **sn_cur_slot(void) { sn_co **p = &sn_cur_co; __asm__ volatile("" : "+r"(p) : : "memory"); return p; }
static __attribute__((noinline)) void **sn_wsp_slot(void) { void **p = &sn_worker_sp; __asm__ volatile("" : "+r"(p) : : "memory"); return p; }
static __attribute__((noinline)) int sn_errno(void) { int e = errno; __asm__ volatile("" : : : "memory"); return e; }

#define SN_STACK ((size_t)8 << 20)
static pthread_mutex_t sn_mu = PTHREAD_MUTEX_INITIALIZER;   /* 保护运行队列和栈的缓存 */
static pthread_cond_t sn_cv = PTHREAD_COND_INITIALIZER;
static sn_co *sn_rq_head, *sn_rq_tail;
static char *sn_stack_cache[64];
static int sn_stack_cached;

static void sn_rq_push(sn_co *t) {
  pthread_mutex_lock(&sn_mu);
  t->next = 0;
  if (sn_rq_tail) sn_rq_tail->next = t; else sn_rq_head = t;
  sn_rq_tail = t;
  pthread_cond_signal(&sn_cv);
  pthread_mutex_unlock(&sn_mu);
}
static sn_co *sn_rq_pop(void) {
  pthread_mutex_lock(&sn_mu);
  while (!sn_rq_head) pthread_cond_wait(&sn_cv, &sn_mu);
  sn_co *t = sn_rq_head;
  sn_rq_head = t->next;
  if (!sn_rq_head) sn_rq_tail = 0;
  pthread_mutex_unlock(&sn_mu);
  return t;
}
static char *sn_stack_get(void) {
  pthread_mutex_lock(&sn_mu);
  char *s = sn_stack_cached > 0 ? sn_stack_cache[--sn_stack_cached] : 0;
  pthread_mutex_unlock(&sn_mu);
  if (s) return s;
  size_t pg = (size_t)sysconf(_SC_PAGESIZE);
  s = mmap(0, SN_STACK + pg, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (s == MAP_FAILED) sn_panic("内存不足（任务的栈）");
  mprotect(s, pg, PROT_NONE);
  return s;
}
static void sn_stack_put(char *s) {
  pthread_mutex_lock(&sn_mu);
  if (sn_stack_cached < 64) { sn_stack_cache[sn_stack_cached++] = s; s = 0; }
  pthread_mutex_unlock(&sn_mu);
  if (s) munmap(s, SN_STACK + (size_t)sysconf(_SC_PAGESIZE));
}

/* 让出工作线程：hook 在切回工作线程之后执行 */
static void sn_co_park(void (*hook)(sn_co *)) {
  sn_co *t = *sn_cur_slot();
  t->hook = hook;
  sn_ctx_switch(&t->sp, *sn_wsp_slot());
}
static void sn_co_entry(void) {
  sn_co *t = *sn_cur_slot();
  t->fn(t->env);
  t->done = 1;
  sn_ctx_switch(&t->sp, *sn_wsp_slot());
  __builtin_unreachable();
}
static void sn_co_finish(sn_co *t) {
  sn_scope *s = t->scope;
  sn_stack_put(t->stack);
  free(t);
  pthread_mutex_lock(&s->mu);
  if (--s->pending == 0) {
    if (s->waiter) { sn_rq_push(s->waiter); s->waiter = 0; }
    pthread_cond_broadcast(&s->cv);
  }
  pthread_mutex_unlock(&s->mu);
}
static void *sn_worker(void *arg) {
  (void)arg;
  for (;;) {
    sn_co *t = sn_rq_pop();
    *sn_cur_slot() = t;
    sn_ctx_switch(sn_wsp_slot(), t->sp);
    *sn_cur_slot() = 0;
    if (t->done) sn_co_finish(t);
    else if (t->hook) { void (*h)(sn_co *) = t->hook; t->hook = 0; h(t); }
  }
  return 0;
}
static pthread_once_t sn_workers_once = PTHREAD_ONCE_INIT;
static void sn_workers_start(void) {
  long n = sysconf(_SC_NPROCESSORS_ONLN);
  if (n < 1) n = 1;
  if (n > 256) n = 256;
  for (long i = 0; i < n; i++) {
    pthread_t th;
    if (pthread_create(&th, 0, sn_worker, 0)) sn_panic("无法创建工作线程");
    pthread_detach(th);
  }
}

static inline int64_t u_scope_new(void) {
  sn_scope *s = calloc(1, sizeof *s);
  if (!s) sn_panic("内存不足");
  pthread_mutex_init(&s->mu, 0);
  pthread_cond_init(&s->cv, 0);
  return (int64_t)(intptr_t)s;
}
static inline void sn_spawn(int64_t h, void (*fn)(void *), void *env) {
  pthread_once(&sn_workers_once, sn_workers_start);
  sn_scope *s = (sn_scope *)(intptr_t)h;
  sn_co *t = calloc(1, sizeof *t);
  if (!t) sn_panic("内存不足");
  t->fn = fn; t->env = env; t->scope = s; t->stack = sn_stack_get();
  /* 栈顶 16 字节对齐，摆成 sn_ctx_switch 刚切走的样子 */
  uintptr_t top = ((uintptr_t)(t->stack + (size_t)sysconf(_SC_PAGESIZE) + SN_STACK)) & ~(uintptr_t)15;
#if defined(__x86_64__)
  void **sp = (void **)top;
  *--sp = 0;                       /* sn_co_entry 的“返回地址”：进入时 rsp 按 ABI 是 16k+8 */
  *--sp = (void *)sn_co_entry;     /* ret 到这里 */
  for (int i = 0; i < 6; i++) *--sp = 0;   /* rbp rbx r12–r15 */
  t->sp = sp;
#else
  void **sp = (void **)(top - 176);
  memset(sp, 0, 176);
  sp[11] = (void *)sn_co_entry;    /* x30 的位置（偏移 88） */
  t->sp = sp;
#endif
  pthread_mutex_lock(&s->mu);
  s->pending++;
  pthread_mutex_unlock(&s->mu);
  sn_rq_push(t);
}
static void sn_join_park(sn_co *t) {
  sn_scope *s = t->joining;
  pthread_mutex_lock(&s->mu);
  if (s->pending == 0) sn_rq_push(t); else s->waiter = t;
  pthread_mutex_unlock(&s->mu);
}
static inline void u_scope_join(int64_t h) {
  sn_scope *s = (sn_scope *)(intptr_t)h;
  sn_co *t = *sn_cur_slot();
  if (t) {
    /* 任务里的 scope：让出，最后一个子任务结束时放回队列 */
    t->joining = s;
    sn_co_park(sn_join_park);
  } else {
    pthread_mutex_lock(&s->mu);
    while (s->pending > 0) pthread_cond_wait(&s->cv, &s->mu);
    pthread_mutex_unlock(&s->mu);
  }
  pthread_mutex_destroy(&s->mu);
  pthread_cond_destroy(&s->cv);
  free(s);
}

/* 等套接字 fd 可读（wwrite = 0）或可写，最多等到 deadline（单调时钟的毫秒，-1 是一直等）；超时返回 1。
   每个 fd 一对等待链表（只读共享的监听套接字可以被几个任务同时 accept），登记用一次性的事件，
   触发时唤醒这个方向上所有等着的任务，它们重试，没抢到的再登记。
   有期限的任务同时放进按期限排的最小堆；轮询线程等到最早的期限为止，到期的任务从等待链表里摘下来，标上 timedout 放回队列。
   新的期限比堆顶早时往自管道里写一个字节，叫醒轮询线程重新算要等多久。sn_wq_mu 保护等待链表和堆。 */
typedef struct { sn_co *rd, *wr; } sn_waitq;
static pthread_mutex_t sn_wq_mu = PTHREAD_MUTEX_INITIALIZER;
static sn_waitq *sn_wq;
static int sn_wq_cap;
static int sn_poll_fd = -1;
static int sn_wake_pipe[2] = { -1, -1 };
static pthread_once_t sn_poll_once = PTHREAD_ONCE_INIT;
static sn_co **sn_heap;
static int sn_heap_len, sn_heap_cap;

static int64_t sn_now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}
static void sn_heap_set(int i, sn_co *t) { sn_heap[i] = t; t->heap_idx = i; }
static void sn_heap_up(int i) {
  while (i > 0) {
    int p = (i - 1) / 2;
    if (sn_heap[p]->deadline <= sn_heap[i]->deadline) break;
    sn_co *a = sn_heap[p]; sn_heap_set(p, sn_heap[i]); sn_heap_set(i, a); i = p;
  }
}
static void sn_heap_down(int i) {
  for (;;) {
    int l = 2 * i + 1, r = l + 1, m = i;
    if (l < sn_heap_len && sn_heap[l]->deadline < sn_heap[m]->deadline) m = l;
    if (r < sn_heap_len && sn_heap[r]->deadline < sn_heap[m]->deadline) m = r;
    if (m == i) break;
    sn_co *a = sn_heap[m]; sn_heap_set(m, sn_heap[i]); sn_heap_set(i, a); i = m;
  }
}
static void sn_heap_push(sn_co *t) {
  if (sn_heap_len == sn_heap_cap) {
    int c = sn_heap_cap ? sn_heap_cap * 2 : 64;
    sn_co **h = realloc(sn_heap, (size_t)c * sizeof *h);
    if (!h) sn_panic("内存不足");
    sn_heap = h; sn_heap_cap = c;
  }
  sn_heap_set(sn_heap_len++, t);
  sn_heap_up(t->heap_idx);
}
static void sn_heap_remove(sn_co *t) {
  int i = t->heap_idx;
  if (i < 0) return;
  t->heap_idx = -1;
  sn_heap_len--;
  if (i == sn_heap_len) return;
  sn_heap_set(i, sn_heap[sn_heap_len]);
  sn_heap_up(i);
  sn_heap_down(sn_heap[i]->heap_idx);
}
/* 唤醒链表上的任务（持有 sn_wq_mu）：顺便从堆里拿掉 */
static void sn_wake_list(sn_co *l) {
  while (l) { sn_co *n = l->next; sn_heap_remove(l); sn_rq_push(l); l = n; }
}
static void sn_unlink(sn_co **l, sn_co *t) {
  for (; *l; l = &(*l)->next) if (*l == t) { *l = t->next; return; }
}
static void sn_arm(int fd) {   /* 持有 sn_wq_mu 时调用：按两个链表重新登记 */
  sn_waitq *q = &sn_wq[fd];
#ifdef SN_KQUEUE
  struct kevent ev[2]; int n = 0;
  if (q->rd) { EV_SET(&ev[n], fd, EVFILT_READ, EV_ADD | EV_ONESHOT, 0, 0, 0); n++; }
  if (q->wr) { EV_SET(&ev[n], fd, EVFILT_WRITE, EV_ADD | EV_ONESHOT, 0, 0, 0); n++; }
  if (n && kevent(sn_poll_fd, ev, n, 0, 0, 0) < 0) {   /* 登记不了（不该发生）：直接唤醒，让它们重试 */
    sn_wake_list(q->rd); sn_wake_list(q->wr); q->rd = q->wr = 0;
  }
#else
  struct epoll_event ev;
  memset(&ev, 0, sizeof ev);
  ev.events = (q->rd ? EPOLLIN : 0) | (q->wr ? EPOLLOUT : 0) | EPOLLONESHOT;
  ev.data.fd = fd;
  if (!(q->rd || q->wr)) return;
  if (epoll_ctl(sn_poll_fd, EPOLL_CTL_MOD, fd, &ev) < 0 && epoll_ctl(sn_poll_fd, EPOLL_CTL_ADD, fd, &ev) < 0) {
    sn_wake_list(q->rd); sn_wake_list(q->wr); q->rd = q->wr = 0;
  }
#endif
}
/* 到期的任务：从等待链表摘下，标上超时，放回队列（持有 sn_wq_mu） */
static void sn_expire(int64_t now) {
  while (sn_heap_len > 0 && sn_heap[0]->deadline <= now) {
    sn_co *t = sn_heap[0];
    sn_heap_remove(t);
    sn_waitq *q = &sn_wq[t->wfd];
    sn_unlink(t->wwrite ? &q->wr : &q->rd, t);
    t->timedout = 1;
    sn_rq_push(t);
  }
}
static void *sn_poller(void *arg) {
  (void)arg;
  for (;;) {
    pthread_mutex_lock(&sn_wq_mu);
    int64_t wait = -1;
    if (sn_heap_len > 0) { wait = sn_heap[0]->deadline - sn_now_ms(); if (wait < 0) wait = 0; }
    pthread_mutex_unlock(&sn_wq_mu);
#ifdef SN_KQUEUE
    struct kevent evs[64];
    struct timespec ts = { (time_t)(wait / 1000), (long)(wait % 1000) * 1000000 };
    int n = kevent(sn_poll_fd, 0, 0, evs, 64, wait < 0 ? 0 : &ts);
    pthread_mutex_lock(&sn_wq_mu);
    for (int i = 0; i < n; i++) {
      int fd = (int)evs[i].ident;
      if (fd == sn_wake_pipe[0]) { char b[64]; while (read(fd, b, sizeof b) > 0) {} continue; }
      if (fd < 0 || fd >= sn_wq_cap) continue;
      sn_waitq *q = &sn_wq[fd];
      if (evs[i].filter == EVFILT_READ) { sn_wake_list(q->rd); q->rd = 0; }
      else if (evs[i].filter == EVFILT_WRITE) { sn_wake_list(q->wr); q->wr = 0; }
    }
#else
    struct epoll_event evs[64];
    int n = epoll_wait(sn_poll_fd, evs, 64, wait < 0 ? -1 : (int)(wait > 1000000 ? 1000000 : wait));
    pthread_mutex_lock(&sn_wq_mu);
    for (int i = 0; i < n; i++) {
      int fd = evs[i].data.fd;
      if (fd == sn_wake_pipe[0]) { char b[64]; while (read(fd, b, sizeof b) > 0) {} continue; }
      if (fd < 0 || fd >= sn_wq_cap) continue;
      sn_waitq *q = &sn_wq[fd];
      uint32_t e = evs[i].events;
      if (e & (EPOLLIN | EPOLLERR | EPOLLHUP)) { sn_wake_list(q->rd); q->rd = 0; }
      if (e & (EPOLLOUT | EPOLLERR | EPOLLHUP)) { sn_wake_list(q->wr); q->wr = 0; }
      sn_arm(fd);   /* 还有另一个方向在等 */
    }
#endif
    sn_expire(sn_now_ms());
    pthread_mutex_unlock(&sn_wq_mu);
  }
  return 0;
}
static void sn_poll_start(void) {
#ifdef SN_KQUEUE
  sn_poll_fd = kqueue();
#else
  sn_poll_fd = epoll_create1(EPOLL_CLOEXEC);
#endif
  if (sn_poll_fd < 0 || pipe(sn_wake_pipe) != 0) sn_panic("无法创建事件队列");
  for (int i = 0; i < 2; i++) {
    int f = fcntl(sn_wake_pipe[i], F_GETFL, 0);
    fcntl(sn_wake_pipe[i], F_SETFL, f | O_NONBLOCK);
  }
#ifdef SN_KQUEUE
  struct kevent ev;
  EV_SET(&ev, sn_wake_pipe[0], EVFILT_READ, EV_ADD, 0, 0, 0);
  if (kevent(sn_poll_fd, &ev, 1, 0, 0, 0) < 0) sn_panic("无法创建事件队列");
#else
  struct epoll_event ev;
  memset(&ev, 0, sizeof ev);
  ev.events = EPOLLIN; ev.data.fd = sn_wake_pipe[0];
  if (epoll_ctl(sn_poll_fd, EPOLL_CTL_ADD, sn_wake_pipe[0], &ev) < 0) sn_panic("无法创建事件队列");
#endif
  pthread_t th;
  if (pthread_create(&th, 0, sn_poller, 0)) sn_panic("无法创建轮询线程");
  pthread_detach(th);
}
static void sn_wait_hook(sn_co *t) {
  int fd = t->wfd;
  pthread_mutex_lock(&sn_wq_mu);
  if (fd >= sn_wq_cap) {
    int c = sn_wq_cap ? sn_wq_cap : 64;
    while (c <= fd) c *= 2;
    sn_waitq *q = realloc(sn_wq, (size_t)c * sizeof *q);
    if (!q) sn_panic("内存不足");
    memset(q + sn_wq_cap, 0, (size_t)(c - sn_wq_cap) * sizeof *q);
    sn_wq = q; sn_wq_cap = c;
  }
  sn_waitq *q = &sn_wq[fd];
  if (t->wwrite) { t->next = q->wr; q->wr = t; } else { t->next = q->rd; q->rd = t; }
  int earliest = 0;
  if (t->deadline >= 0) { sn_heap_push(t); earliest = t->heap_idx == 0; }
  sn_arm(fd);
  pthread_mutex_unlock(&sn_wq_mu);
  if (earliest) { char c = 1; ssize_t w = write(sn_wake_pipe[1], &c, 1); (void)w; }
}
static int sn_wait_fd(int fd, int write, int64_t deadline) {
  sn_co *t = *sn_cur_slot();
  if (!t) {   /* 主线程：直接阻塞 */
    struct pollfd p = { fd, (short)(write ? POLLOUT : POLLIN), 0 };
    for (;;) {
      int ms = -1;
      if (deadline >= 0) { int64_t d = deadline - sn_now_ms(); if (d <= 0) return 1; ms = d > 1000000 ? 1000000 : (int)d; }
      int r = poll(&p, 1, ms);
      if (r > 0) return 0;
      if (r == 0 && deadline >= 0 && sn_now_ms() >= deadline) return 1;
      if (r < 0 && sn_errno() != EINTR) return 0;
    }
  }
  pthread_once(&sn_poll_once, sn_poll_start);
  t->wfd = fd; t->wwrite = write; t->deadline = deadline; t->heap_idx = -1; t->timedout = 0;
  sn_co_park(sn_wait_hook);
  t = *sn_cur_slot();
  return t->timedout;
}
/* 超时（毫秒，<= 0 是不限）→ 期限 */
static int64_t sn_deadline(int64_t ms) { return ms > 0 ? sn_now_ms() + ms : -1; }
static void sn_nonblock(int fd) { int f = fcntl(fd, F_GETFL, 0); if (f >= 0) fcntl(fd, F_SETFL, f | O_NONBLOCK); }
#define u_scope_keep(p) ((void)(p))
