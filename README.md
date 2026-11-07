# sinterc：Sinter 编译器（C0 骨架）

附录 F 的第一个里程碑 C0：解析、类型检查、生成 C。只支持 `I64`、`Bool`、`()`、结构体、函数、
`let` / 赋值、`if`（可作表达式）、`while`、`return`，以及内置的 `print_i64`、`print_bool`。
还没有所有权、投影、`linear`、区域等检查（从 C1 开始按 Lean 模型逐项加入）。

## 依赖

OCaml 5.3、dune ≥ 3.0、menhir，以及系统的 C 编译器（`cc`，需要支持 `__builtin_add_overflow` 一类内建函数，GCC 和 Clang 都支持）。

```
opam install dune menhir
```

## 构建与测试

```
cd compiler
dune build
dune test                      # 运行 test/c0.t 里的端到端测试
dune exec sinterc -- 文件.sn -o 文件.c
cc -std=gnu11 文件.c -o 文件 && ./文件
```

## 约定

- 语句之间用换行分隔（也可以用 `;`），圆括号里的换行忽略；块的最后一条若是表达式，就是块的值。
- 整数运算检查溢出和除以零，出错时打印 `panic: …` 并以退出码 101 结束。
- 函数参数、结构体字段一律从左到右求值（降级到 MIR 时操作数依次存进临时变量，不依赖 C 的求值顺序）。

## 编译流程

解析（`lexer.mll`、`parser.mly`）→ 类型检查（`typeck.ml`）→ 降级为 MIR 控制流图（`mir.ml`）→ 生成 C（`emit_c.ml`，每个基本块一个标签，块之间用 `goto`）。
- 错误信息格式：`文件:行:列: 错误：…`，退出码 1。
