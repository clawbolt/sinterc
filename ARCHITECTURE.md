# sinterc 的结构

这份文档写编译器内部怎么组织：现在是什么样、要整理成什么样、整理到哪一步了。
语言本身见 README.md，规则的证明见 ../lean。

## 现状（重构前，master 的 “Add HTTP and URL libraries”）

### 流水线

```
bin/main.ml   命令行、整条流水线、--batch 对照、调用 C 编译器、报错的渲染，都在这一个文件
   │
   ▼
lexer.mll / parser.mly ──► Ast.program            位置是 {line; col}，第 i 个文件的行号从 i × 1,000,000 开始
   │
modules.ml ──► 合成一个 Ast.program               use 引入的模块给名字加前缀（m.f、m.T），靠改名合并
   │
typeck.ml（include 六个 typeck_*.ml）             类型检查，结果写进模块级的表；去糖和泛型实例化也在这里
   │
mir.ml ──► Mir.func list                          降级；要查 Typeck 的十几张表
   │
own.ml ──► borrow.ml                              所有权与析构展开；投影锁定与排他性（MIR 上的独立检查）
   │
emit_c.ml / emit_core.ml                          生成 C（运行时 runtime/*.h 原样拼进去）/ 导出给 Lean 的 CFG
```

`i18n.ml` 把中文的报错整条匹配成英文；`fmt.ml` 是按记号的格式化，和其余部分几乎不相干。

### 各文件

| 文件 | 行数 | 做什么 |
|---|---:|---|
| `bin/main.ml` | 395 | 见上图 |
| `src/lexer.mll`、`parser.mly` | 179、648 | 词法、语法 |
| `src/ast.ml` | 207 | 语法树，另有 6 张全局表（`files`、`type_file`、`inherent`、`next_id`、`prims`、`statics`） |
| `src/modules.ml` | 521 | 模块、`use`、pub、改名（`rename` 一个函数近 300 行） |
| `src/typeck.ml` | 598 | `prepare`（整个程序的登记，368 行）、逐个函数检查、`--batch` 的入口 |
| `src/typeck_env.ml` | 163 | 类型 `t`、环境、`Error` 异常，12 张结果表 |
| `src/typeck_builtins.ml` | 193 | 内建函数和方法 |
| `src/typeck_regions.ml` | 131 | 区域、协变；两个前向引用 `covariant_fwd`、`instantiate_fwd` 绕开循环依赖 |
| `src/typeck_generics.ml` | 461 | 泛型、trait、impl、实例化，24 张表 |
| `src/typeck_pattern.ml` | 233 | 模式和覆盖性 |
| `src/typeck_expr.ml` | 1130 | 表达式和语句；`expr1` 232 行、`block` 190 行、`call_named` 176 行 |
| `src/mir.ml` | 789 | MIR 的定义（清楚，有注释和 Lean 对应）和降级 |
| `src/own.ml`、`borrow.ml` | 357、258 | 所有权、投影检查 |
| `src/emit_c.ml`、`emit_core.ml` | 484、394 | 两个后端 |
| `src/i18n.ml` | 465 | 英文报错（按中文格式串匹配） |
| `src/fmt.ml` | 170 | 格式化 |
| `runtime/sinter.h`、`sinter_lib.h` | 749、520 | C 运行时 |
| `tools/migrate.ml` | 594 | 标准库重新划分时的一次性迁移工具 |

### 读起来难的地方

1. **状态在模块级的表里。** `src/` 里共 54 个模块级的 `Hashtbl` / `ref`：`typeck_generics.ml` 24 个、`typeck_env.ml` 12 个、
   `typeck_regions.ml` 6 个、`ast.ml` 6 个，`typeck_builtins.ml` 2 个，`typeck_expr.ml`、`typeck_pattern.ml`、`emit_c.ml`、`i18n.ml` 各 1 个。`Typeck.prepare` 开头手工 `reset`，漏一张就会在 `--batch` 里串到下一个程序。
2. **类型检查兼做去糖和实例化。** 闭包提升、`spawn`、`to_str` 记在 `rewrites`，嵌套模式记在 `pdesugar`，`for` 的展开记在 `for_vec`，
   都按表达式 id 索引；实例化时复制函数体、分配新 id，再按 id 套上这些改写。
3. **后面的阶段读类型检查的内部表。** `mir.ml` 用到 `Typeck` 的 `types`、`call_inst`、`scope_shares`、`let_regions`、`for_vec`、`struct_tbl` 等；
   `emit_c.ml` 用 `packs`、`spawns`、`vec_calls`、`fcalls`、`instantiate`；`own.ml`、`borrow.ml`、`emit_core.ml` 也读 `struct_tbl`、`region_names`。
4. **所有阶段共用 `Typeck.Error`。** 降级、所有权、投影检查报错都 `raise Typeck.Error`。
5. **值是元组。** `prims` 是 5 元组，`inherent`、`gparams` 的值也是元组，要回头查字段的顺序。

### 已经不错、要保留的

- MIR 的定义、`own.ml` 和 `borrow.ml` 的算法：它们直接对应 Lean 的检查，三方对照在验证它们，重构只换接口。
- 词法、语法、`fmt.ml`：和其余部分耦合很少。
- 注释的风格：中文、说明为什么、标出对应的 Lean 模块。

## 目标

见重构规划（sinterc 重构规划）的第 II 节。简单说：

- 每个阶段只通过“输入 → 输出”交接：syntax → resolve → collect → typeck（带类型的语法树 Tast）→ desugar → mono → lower → own / borrow → emit。
- 没有模块级的可变状态；每个模块有 `.mli`；诊断是数据（`Diag.t`），文字按键从 `messages.ml` 取中英文。
- 目录分成 `base/ syntax/ resolve/ types/ elab/ mir/ back/ driver/`（dune 的 `include_subdirs unqualified`，模块名不变长）。

## 重构的进度

每一步结束时下面这些都要通过才进下一步：`dune test`（不改 `.t` 文件）、`sh fmtcheck.sh`、`sh fuzz.sh`、
`sh baseline.sh check`（和 `_baseline/` 比较）；有 ../lean 时再加 `sh validate.sh`、`sh diff.sh`（计数和基线相同）。

| 步 | 内容 | 状态 |
|---|---|---|
| R0 | 基线：`sh baseline.sh record --with-lean`；本文档的现状部分 | 完成 |
| R1a | `Source`：位置是 `{file; line; col}`，取代行号里乘 1,000,000 的编码 | 待验证 |
| R1b | `Diag` 和按键的 `messages.ml` 取代 `Typeck.Error` 和 `i18n.ml` 的匹配 | |
| R2 | `main.ml` 拆成 `cli`、`pipeline`、`batch`、`cc` | |
| R3 | `Syntax` 去掉全局表；`resolve` 返回 `Resolved.program` | |
| R4.0 | 类型检查的全局表原样收进一个显式传递的 `Typing.t`（只搬家，不改逻辑） | |
| R4.1 | 先写好 `tast.ml`、`defs.mli` 并评审，再动类型检查 | |
| R4a–d | `collect`；`check_expr` 等输出 Tast；模式和区域；捕获 | |
| R5 | `elab/` 的去糖和 `mono`，去掉按表达式 id 的表 | |
| R6 | `lower.ml` 只读 Tast；own、borrow 返回诊断 | |
| R7 | C 后端拆成 4 个文件 | |
| R8 | 运行时按职责拆成 4 个头文件，构建时拼回 | |
| R9 | 收尾：README、CHANGELOG；编译时间不比基线慢 20% 以上 | |

R4.0 和 R4.1 是这次重做时加的：R4.0 先把最影响阅读的全局状态解决掉，之后即使 Tast 那一步要退回，这一步也不白做；
R4.1 让 R4 到 R6 围绕的两个接口先定下来，避免边改边调。
