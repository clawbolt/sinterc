# sinterc 的结构

这份文档写编译器内部怎么组织：现在是什么样、要整理成什么样、整理到哪一步了。
语言本身见 README.md，规则的证明见 ../lean。

## 现在的结构

### 流水线

```
bin/main.ml ─► cli.ml（参数）─► pipeline.ml（load → check → emit）；batch.ml 是 --batch 对照，report.ml 渲染报错，cc.ml 调 C 编译器
   │
lexer.mll / parser.mly ──► Ast.program                   位置是 Source.pos {file; line; col}
   │
resolve.ml ──► Resolved.t                                 合成的程序 + 类型所在的文件、类型自己的方法、内建函数、静态函数
   │
collect.ml ──► Defs.t                                     整个程序的登记：结构体、枚举、签名、trait、impl（20 个按步骤命名的函数）
   │
check.ml（include ctx、region、generics 等）──► Tast.program    逐个函数检查，直接产出带类型的语法树；状态集中在 check_state.ml（各组表有说明）
   │
mir.ml ──► Mir.func                                       降级：只读 Tast 和 Defs
   │
own.ml ──► borrow.ml                                      所有权与析构展开；投影锁定与排他性。只读 MIR 和 Defs；mir_check.ml 把降级和这两步接起来，返回 result
   │
emit_c.ml（c_names、c_types、c_func、c_closures）/ emit_core.ml       生成 C / 导出给 Lean 的 CFG
```

各阶段报错都是 `Diag.t`（键是中文格式串，加参数），`report.ml` 渲染，英文按键查 `messages.ml`；`test/messages.t` 检查表和代码对得上。

### 阶段之间交接的东西

| 类型 | 定义在 | 谁产生 | 谁读 |
|---|---|---|---|
| `Ast.program` | `ast.ml` | 语法 | 模块解析 |
| `Resolved.t` | `resolved.ml` | `Resolve.load` | `collect`、类型检查 |
| `Defs.t` | `defs.ml` | `collect`（现在指向 `check_state.ml` 第一组的表，检查时还会加进泛型实例） | 类型检查、降级、`own`、`borrow`、两个后端 |
| `Tast.program`、`Tast.generated` | `tast.ml` | 类型检查（`check_expr.ml` 建节点，`mono.ml` 建实例，`Check.generated` 列出要写 C 辅助函数的东西） | `mir.ml`、C 后端 |
| `Mir.func` | `mir.ml` | `mir.ml` | `own`、`borrow`、两个后端 |
| `Diag.t` | `diag.ml` | 所有阶段 | `report.ml`、`batch.ml` |

`Defs.variants`、`is_linear`、`needs_drop`、`region_names`、`erase` 这些按定义回答的问题只有 `defs.ml` 里一份。

### 目录

`src/` 按阶段分目录，`src/dune` 用 `(include_subdirs unqualified)`：子目录里的模块照样直接按名字用（模块名不能重复）。
依赖只该往上指（base ← syntax ← ir ← 各阶段），但这只是约定，dune 不检查；以后要检查的话，每个目录做成一个 library。

```
src/
  base/      source、diag、messages              位置、诊断、英文信息表
  syntax/    ast、lexer.mll、parser.mly、fmt     语法树（include ir/ty）、词法、语法、格式化；dune 里是 ocamllex、menhir
  resolve/   resolve                             模块、use、pub、改名
  ir/        ty、resolved、defs、tast            类型和阶段之间交接的数据（MIR 的定义还在 mir/mir.ml 里，和降级在一起）
  types/     collect、check_state、ctx、builtin、region、generics、pattern、check_expr、check   类型检查
  elab/      mono                                泛型函数的实例（单态化）
  mir/       mir、own、borrow、mir_check         降级、所有权、投影与排他性
  back/      c_names、c_types、c_func、c_closures、emit_c、emit_core   C 和 Lean 的 CFG
  dune       另外生成 stdlib_src.ml（std/*.sn）、runtime_src.ml（runtime/*.h）
bin/         main、cli、pipeline、batch、report、cc、fmt_cmd
```

改名（D3）之后的名字：`typeck_env` → `ctx`、`typeck_builtins` → `builtin`、`typeck_regions` → `region`、`typeck_generics` → `generics`、`typeck_pattern` → `pattern`、`typeck_expr` → `check_expr`、`typing` → `check_state`、`typeck` → `check`、`modules` → `resolve`。`ast` 不改：注释和文档里“Ast”指的就是没有类型的语法树。下面的历史记录保留当时的名字。

### 还没整理到的

- 类型检查内部还按表达式 id 记两张表：`types`（几处要看之前检查过的子表达式的类型）、`method_res`（生成特化时用）。它们不再出类型检查。
- 类型检查报错时仍然抛 `Diag.Error` / `Diag.Errors`；类型检查之后的几步（`mir_check.ml`）已经返回结果。
- 第一到第三组的表仍然是模块级的（集中在 `check_state.ml`，`reset ()` 清空）；第四组（正在检查的函数）已经在环境里（R4.0b.1）。`.mli` 只有 `source`、`diag`。

## 重构之前（master 的 “Add HTTP and URL libraries”）

下面是 R0 时记下的样子，留着对照。

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
| R1a | `Source`：位置是 `{file; line; col}`，取代行号里乘 1,000,000 的编码 | 完成 |
| R1b.1 | `Diag.Error` / `Diag.Errors` 取代 `Lexer.Error`、`Ast.Syntax`、`Typeck.Error`、`Typeck.Errors`，文字不变 | 完成 |
| R1b.2 | 诊断带键和参数（键是中文格式串）；`messages.ml` 按键查英文，取代 `i18n.ml` 的整句匹配；`test/messages.t` 检查表和代码对得上 | 完成 |
| R2 | `main.ml` 拆成 `cli`、`pipeline`、`report`、`cc`、`fmt_cmd`、`batch`；`main.ml` 只剩 15 行的分派 | 完成 |
| R3 | `Ast` 的 `type_file`、`inherent`、`prims`、`statics` 收进 `Resolved.t`，由 `Modules.load` 返回；值从元组换成记录。`Ast.next_id` 留到 R5（随按 id 的表一起去掉），词法分析的状态留到 R4.0 | 完成 |
| R4.0 | 类型检查的 47 个表和 ref 搬进 `typing.ml`，按用途分五组、各有说明；`prepare` 开头 30 处手工清空换成 `Typing.reset ()`。只搬家，访问方式不变 | 完成 |
| R4.0b | 显式传递：按组换成参数（第四组进检查函数的环境，第一组随 R4a 变成 `Defs`，第二组随 R4b、R5 去掉），不再一次性改 380 处 | 进行中 |
| R4.0b.1 | 第四组：当前函数（名字、会不会生成代码、let 参数、绑定的区域）放进 `env.cur`，区域、循环层数、闭包、scope 是 `env` 的字段。进出循环、闭包、scope、arena 改成建新的环境，不再手工保存恢复（去掉了 3 个 `Fun.protect` 和出错时的 `regions := []`）；`expect`、`join`、`sub`、`outlives` 多一个 `env` 参数 | 完成 |
| — | `baseline.sh`：`examples/ok/scope.sn` 的两个任务在不同线程上打印，先后不定；它的标准输出排序后再记（`_baseline` 里已有的一份也排过序） | 完成 |
| R4.1 | `src/tast.ml`、`src/defs.ml` 两个接口草稿（只有类型，还没有代码用它们）；评审定下来再动类型检查 | 完成（内建函数各一个构造子；泛型实例是 Defs 里的可变子记录） |
| R4a.1 | `prepare`（336 行）拆成 `collect.ml` 里 20 个按步骤命名的函数（最长的 impl_signatures 约 55 行），执行顺序不变；仍写 `Typing` 的第一组表 | 完成 |
| R4a.2 | `Defs.t` 定下来：字段就是第一组的表，`Typing.defs` 指向它们。`variants`、`is_linear`、`needs_drop`、`region_names` 等只留 `defs.ml` 里一份（类型检查里是部分应用）。`own.ml`、`borrow.ml`、`emit_core.ml` 显式接收 `Defs.t`，不再依赖 `Typeck`。`mir.ml`、`emit_c.ml` 还要读第二组的表，随 R6、R7 再换 | 完成 |
| R4b.1 | `to_tast.ml` 把 Ast 和类型检查的旁路表翻成 Tast；`mir.ml` 改成只读 Tast 和 `Defs`，不再依赖 `Typeck`（按 id 的表、`arm_for`、别名、签名都只在 `to_tast.ml` 里查；泛型实例的返回借用也改在那里登记） | 完成 |
| R4b.2 | `emit_c.ml` 只读 MIR、`Defs` 和 `Tast.generated`（闭包、Fn 调用、任务、Vec 内建函数，按以前遍历表的顺序列出）；代码生成要用的 `Option<T>` 实例改在 `To_tast.generated` 里建好，代码生成不再改 `Defs`。`closure` 类型搬进 `tast.ml`，`erase` 搬进 `defs.ml`。之后 `src/` 里类型检查以外只有 `to_tast.ml` 依赖 `Typeck` | 完成 |
| R4b–d | 表达式、语句、块的检查直接返回 Tast 节点，`to_tast.ml` 去掉；`rewrites`、`pdesugar`、`for_vec`、`scope_shares`、`errdefer_targets` 五张按 id 的表去掉（调用改写、嵌套模式、for 的展开、scope 的共享变量、errdefer 的目标直接写进节点）；`Typeck.program` 返回 `Tast.program`（要降级的函数和类型名），代码生成不再读 Ast | 完成 |
| D1 | 按阶段分目录（只搬文件，不改名、不改代码；`(include_subdirs unqualified)`）。两个 cram 测试里的源文件路径跟着改，期望的输出不变 | 完成 |
| D2 | 类型 `ty`、`mode` 和取类型名字的函数从 `ast.ml` 搬到 `ir/ty.ml`，`ast.ml` include 它（用法不变） | 完成 |
| D3 | 改名：`types/` 里的 `typeck_*`、`typing` 和 `resolve/modules`（见“目录”一节），只改名字，不改代码 | 完成 |
| R5 | 泛型函数的实例在 Tast 上单态化（`elab/mono.ml`），不再复制 Ast 重新检查。M1：两条路同时走、逐个比较（`SINTER_MONO=check`，`mono_check.sh`）；M2：相同的换成单态化；M3：函数体里有闭包、调用 Fn(…) 值的也支持；之后去掉 `call_inst`、`map_body` 的复制 | M1–M3 完成（全部例子的实例逐个相同，闭包的提升、特化、Fn#、spawn#、call# 和检查器共用 lift、spec_register）；M4 完成（实例不再重新检查，--batch 时按需先检查泛型函数；重新检查只留在对照模式）；M5 完成：去掉 call_inst（types 留着，borrow_region 推断泛型调用时会读先检查过的实参的类型，换掉要改检查的顺序） | 完成 |
| R6 | 降级只读 Tast（R4b.1 已经做到，仍叫 `mir.ml`）；`Own.elaborate`、`Borrow.check` 返回 `(func, Diag.t) result`，降级、所有权、投影三步由 `mir_check.ml` 接起来，调用者（pipeline、batch、fuzz）不再各自 try。阶段内部仍用异常在第一个错误处停下（每个函数只报第一个错误） | 完成 |
| R7 | C 后端拆成 4 个文件：`c_names`（C 名字、C 类型）、`c_types`（类型定义、析构、Vec 宏、运行时库）、`c_func`（MIR 函数）、`c_closures`（闭包、任务、Fn 调用）；`emit_c.ml` 只剩按顺序拼接和入口。代码原样搬，生成的 C 逐字节不变（`--with-c`）。同时删掉 `tools/migrate.ml` | 完成 |
| R8 | 运行时按职责拆成六个文件（`base`、`values`、`sched`、`entry` 拼成 `sinter.h`；`lib_io`、`lib_net` 拼成 `sinter_lib.h`），`runtime/dune` 构建时拼回，内容逐字节不变；各文件的说明在 `runtime/README.md` | 完成 |
| R9 | 收尾（第一阶段）：`ARCHITECTURE.md` 写现在的结构和还没整理到的，README、CHANGELOG、`runtime/README.md` 跟上；编译时间和基线相同（examples/ok 生成 C 十遍 5 秒），生成的 C、报错、Lean 对照逐字节不变 | 完成 |

R4.0 和 R4.1 是这次重做时加的：R4.0 先把最影响阅读的全局状态解决掉，之后即使 Tast 那一步要退回，这一步也不白做；
R4.1 让 R4 到 R6 围绕的两个接口先定下来，避免边改边调。

R4b.1、R4b.2 也是重做时加的：先加一层翻译，让降级和 C 后端只读 Tast 和 `Defs`，不必先改写 1100 行的类型检查。
剩下的 R4.0b、R4b–d、R5、R6 的后一半都在类型检查里面或者紧挨着它，可以一步一步做，每一步照样和基线比较。
