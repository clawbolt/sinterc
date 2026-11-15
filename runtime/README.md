# C 运行时

sinterc 生成的 C 文件里原样放进这些代码。构建时 `dune` 把它们按顺序拼回两个文件（见 `dune`）：

- `sinter.h`：放在生成的 C 文件开头，和程序无关，单独就能通过 C 编译器的检查（`test/runtime.t`）。
- `sinter_lib.h`：用到 `Option<Str>`、`Result<Str, Str>` 等内建实例类型，放在按程序生成的类型定义之后。

| 文件 | 拼进 | 行数 | 内容 |
|---|---|---:|---|
| `base.h` | `sinter.h` | 234 | 头文件、`()`、整数和浮点运算（溢出检查）、分配计数（泄漏检查）、打印、`Str`、`Bytes`、UTF-8、`os.exit` 和路径检查 |
| `values.h` | `sinter.h` | 68 | 区域（arena）、存起来的闭包 `Fn(…)`、字符串切片 `&str`、Vec 内建函数的宏 `SN_VEC_*` |
| `sched.h` | `sinter.h` | 423 | 结构化并发：`scope` 和任务、协程切换（x86_64 和 arm64 的汇编）、工作线程的调度、kqueue / epoll 上的非阻塞 I/O 和超时 |
| `entry.h` | `sinter.h` | 24 | 入口：`sn_main`（运行 `main`、检查泄漏）、`sn_batch`（对照测试） |
| `lib_io.h` | `sinter_lib.h` | 214 | 字符、解析数字、命令行参数、读行、文件和标准输入、环境变量、目录、子进程 |
| `lib_net.h` | `sinter_lib.h` | 306 | `std.net`：地址、域名解析、TCP 连接和监听、UDP、带超时的读写 |

拼出来的 C 要和以前逐字节相同，所以每个文件开头没有另加说明，说明都在这里；文件里原有的注释照旧。
改了这里的代码之后，`sh baseline.sh check --with-c` 会显示生成的 C 的变化。
