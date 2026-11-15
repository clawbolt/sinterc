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
