# sinterc 更新记录 / Changelog

证明在单独的仓库 [sinter-formal](https://github.com/clawbolt/sinter-formal)，和编译器的版本一起更新。

## 未发布（0.3 的计划全部完成，0.4 进行中）

**语言**
- 嵌套模式：`.Some((0, s))`、`Ok(.None)`、`(0, _)`，可以 match 元组；检查穷尽（没覆盖时报一个例子）和永远匹配不到的分支（C55，Lean M24、M24b）。
- trait：内建的 `ToStr`，字符串插值用它，可以给多种类型各写一个 `fn to_str`，泛型代码里写 `T: ToStr`；
  默认方法（trait 里写了函数体的方法，impl 可以不写）；泛型 impl `impl<T: ToStr> ToStr for Vec<T>`，
  重叠的 impl 被拒绝（C56，Lean M25）。`std.bigint` 的 BigInt 可以直接插值。
- 字节串 `Bytes`：任意字节，`get`、`set`、`push`、`append`、`slice` 等；`read_bytes`、`write_bytes` 读写二进制文件，
  `print_bytes` 原样输出；`s.to_bytes()`、`str_from_utf8(b)`（只在合法的 UTF-8 时得到 `.Some`，C57，Lean S11）。
- 可以存起来的闭包 `Fn(T) -> U`：存进变量、字段、`Vec`，也可以返回；外面的变量按值拿进来，闭包可以改自己的环境（计数器），
  调用时闭包按 `inout`；`Fn` 变量可以传给 `fn(…)` 参数（C59，Lean M26）。
- 字符串切片 `&str in a`：`text(a, s)` 把字符串交给区域，`sub`、`byte`、`find`、`eq` 不分配，切片和引用一样不能活过它的区域；
  字符串字面量不再每次分配，修改时才复制（C61，Lean M27）。
- 类型自己的方法 `impl 类型 { … }`：可以给结构体、内建类型（`I64`、`Str`、`Vec<T>`、`&str in a`）和具体实例
  （`Vec<I64>`、`Vec<F64>` 各有一个 `sum`）写方法，方法可以是泛型的、可以有 `fn(…)` 参数；`x.m()` 先按 `x` 的类型找，
  在定义处查一次；同名方法的头部重叠时拒绝（C64，Lean M29）。
- trait 的方法名不再占用全局的名字：只能写成方法调用 `a.cmp(b)`，按接收者的类型在约束和 impl 里找；
  自己的函数可以叫 `cmp`，两个 trait 可以有同名的方法（接收者两个都满足时报错）。**不兼容**：`cmp(a, b)` 这样直接调用
  trait 的方法要改成 `a.cmp(b)`，编译器会提示（C65，Lean M29）。
- 预导入的核心部分 `std/core/`：每个程序自动读进来，pub 的名字直接可见（本文件的同名定义优先），只输出用到的函数。
  现有 `traits`（`Eq`、`Ord`、`Hash`、`enum Ordering`、`min`、`max`），以及 `I64`、`F64`、`Char`、`Option`、`Result`、`Str`、
  `&str in a`、`Bytes`、`Vec<T>` 的方法（`s.trim()`、`v.sort()`、`line.split(",")`…，和旧模块的同名函数结果相同，见 `test/std_core.t`）。
  **不兼容**：`Eq`、`Ord` 从 `std.math`、`Hash` 从 `std.hashmap` 移到核心部分，不再写 `math.Ord`；
  `a.cmp(b)` 返回 `Ordering`（`.Less`、`.Equal`、`.Greater`，有 `is_lt()`、`to_i64()` 等），不再是 -1 / 0 / 1（C66）。

**标准库**
- `std.bigint`：任意大小的整数，加、减、乘、比较、除以小数、十进制转换，实现 `Eq`、`Ord`（C54，Lean S10）。
- `std.os`：`env`、`list_dir`、`exists`、`is_dir`、`make_dir`、`remove`，启动子进程 `run`（返回退出码）和 `output`
  （收集标准输出），不经过 shell；`exit(code)`（C58）。
- `eprint(s)`、`eprint_str(s)`：写到标准错误（C60）。
- `std.view`：字符串切片的 `split`、`lines`、`trim`、`starts_with`、`ends_with`、`to_i64`，结果是同一块缓冲区里的切片（C61）。
- `read_file_in(a, path)`、`read_stdin_in(a)`、`read_chunk(a, n)`：把文件、标准输入（任意字节）直接读进 arena，得到切片（C62）。
- `std.view` 的 `line_end`、`trim_cr`、`field`：逐行、逐字段访问，不建 `Vec`（C62）。
- `std.str` 的 `find`、`split`、`replace`、`starts_with`、`ends_with` 逐字节比较，不再为每个位置复制子串（C62）。
- `std.view` 的 `line_reader()`、`next_block(r, a, n)`：逐块读标准输入，跨块的行先拼起来，峰值内存和输入大小无关（C63，Lean M28）。
- 系统层的模块：`std.io` 加了 `read_line`、`read_all`、`read_all_in`、`read_chunk`、`lines`、`next_block_in`；
  新的 `std.fs`（`read`、`read_in`、`read_bytes`、`write`、`write_bytes`、`read_lines`、`write_lines`、`exists`、`is_dir`、`list_dir`、`make_dir`、`remove`）、
  `std.env`（`args`、`get`、`exit`）、`std.process`（`run`、`output`）。预导入的 `println`、`eprintln`。
  标准库里可以写 `intrinsic.f(…)` 调用内建函数 `f`，即使模块里有同名的函数；别的代码里报错（C67）。
- **不兼容**：标准库按类型和领域重新划分（C68）。
  - 去掉 `std.str`、`std.char`、`std.option`、`std.result`、`std.view`、`std.os`：它们的函数是核心部分里的方法（`str.trim(s)` → `s.trim()`），
    或者搬进了 `std.io`、`std.fs`、`std.env`、`std.process`（`os.exists(p)` → `fs.exists(p)`，`view.line_reader()` → `io.lines()`）。
  - `std.vec` 只剩构造函数 `range`；`std.math` 只剩 `pi`、`e`，其余是 `I64`、`F64` 的方法（`math.abs(x)` → `x.abs()`，`math.fabs` → `abs`），
    `min_of`、`max_of` 是预导入的 `min`、`max`。
  - 有序 Vec 的 `std.map` 去掉，AVL 树的 `std.treemap` 改名为 `std.map`（`TreeMap` → `Map`）；`Map`、`HashMap`、`BigInt` 的操作是方法：
    `m.insert(k, v)`、`m.get(k, |v| v)`、`a.add(b)`，构造函数留在模块里（`map.new()`、`bigint.from_i64(x)`）。
  - 打印同 Rust：`println` 换行，`print` 不换行，`eprintln`、`eprint` 同样；`print_i64(x)` 等写成 `println("{x}")`。
  - `tools/migrate.ml` 把旧的写法改成新的：`dune exec tools/migrate.exe -- --modules -w 文件…`（`.sn`、cram 测试 `.t`、Markdown 里的代码块）。
- **不兼容**：标准库分层（C69）。
  - 方法调用 `x.f(…)` 只找类型自己的方法、内建类型的方法和 trait 的方法，不再落到普通函数 `f(x, …)`；报错 `P 没有方法 f`，并提示改法。
  - 内建函数只有标准库能直接调用：用户代码写成方法（`vec_len(v)` → `v.len()`、`str_from_i64(n)` → `n.to_str()`、`vec_get(v, i)` → `v[i]`、
    `char_from(n)` → `n.to_char()`）或者系统模块的函数（`read_line()` → `io.read_line()`、`args()` → `env.args()`、`read_file(p)` → `fs.read(p)`、
    `print_byte(b)` → `io.write_byte(b)`）。预导入的 `vec_new`、`bytes_new`、`box`、`unbox`、`consume`、`alloc`、`text`、`panic`、`choose` 照旧。
    读写外部的内建函数在标准库里也只有 `std.io`、`std.fs`、`std.env`、`std.process` 和打印能调用。
  - 切片的 `find(p)`、`find_from(p, i)` 和 `Str` 的一样返回 `Option<I64>`（原来的 `find(p, from)` 没找到时是 -1）。
  - `migrate` 默认做这一步的改写；上一步的改写要加 `--modules`（每一步只能跑一次）。
- **不兼容**：去掉重复的内建函数和方法（C70）。`Str` 的 `to_i64()`、`to_f64()` 去掉，用核心部分的 `parse_i64()`、`parse_f64()`
  （和切片的 `parse_i64` 同名）；内建函数 `print_i64`、`print_bool`、`print_f64`、`print_char` 去掉，调用时提示写成 `println("{x}")`。
  直接调用内建函数的报错按参数个数给出写法（`s.len()`、`s.push(…)`）。
- 内建方法在核心部分里声明（C71）：`impl Str { intrinsic fn len(s: Str) -> I64 = str_len }`，没有函数体，方法调用直接换成内建函数，
  签名由编译器和内建函数的签名核对（参数约定、类型都要相同；Vec 和切片的类型不固定，只核对有这个内建函数）。
  编译器里 Str、Bytes、切片的三张方法表，Vec 的五个方法和各类型 `to_str` 的特判都去掉了，方法只按类型查找一种办法；
  核心部分里只转发一次的包装（`x.sqrt()`、`s.parse_i64()` 等）也改成 `intrinsic fn`。直接调用内建函数时的写法提示由这些声明生成。
  `intrinsic fn` 只能写在标准库的 `impl 类型 { … }` 里。
- 字符串的字面量标记和长度共用 8 字节，`Str` 保持 16 字节（C61 最初让它变成了 24 字节）。

**验证**
- AVL 平衡：`std.treemap` 的插入、删除保持平衡，树高是对数（Lean S6b）。
- 切片的边界、arena 只增不改、静态字面量的分配计数（Lean M27）。

**工具**
- `sinterc build --release`、`sinterc run --release`：`-O2`，不统计分配、不做退出时的泄漏检查（C60）。
- 编辑器的语法高亮：`editor/vscode`（VS Code，含字符串插值）和 `editor/vim`（T3）。
- 随机测试 `sh fuzz.sh`：fmt 对空白不敏感、格式化前后记号不变且幂等，编译器对随机改坏的程序不出内部错误；CI 每次换一个种子跑（T5）。

**整理**（不改行为）
- 测试、例子和 Lean 证明按内容命名：`test/c40.t` 改成 `test/closure_param.t`，`lean/Core/M27.lean` 改成 `lean/Core/Strings/Slice.lean`（模块 `Core.Strings.Slice`），等等。
  Lean 证明按主题分进 `Ownership/`、`Regions/`、`Library/` 等目录。对照表见文末。
- 代码注释、测试说明和 README 里不再写里程碑编号，引用证明时写 Lean 的模块名。README 改成按主题组织。
- `validate.sh` 先清空 `lean/Validate/` 再导出，删掉或改名的例子不会留下过时的文件。
- 删掉 `i18n.ml` 里 4 条已经没有地方用的翻译。
- C 运行时从 `emit_c.ml` 的字符串里搬到 `runtime/sinter.h` 和 `runtime/sinter_lib.h`，构建时编进编译器：Vec 的辅助函数改成 `SN_VEC_*` 宏，I/O、解析、`std.os` 的函数用 `SN_OPT_STR` 等宏代替类型名，入口改成 `sn_main`、`sn_batch`。`emit_c.ml` 只生成按类型变化的部分（原来 981 行，现在 466 行）。`test/runtime.t` 检查 `sinter.h` 单独能编译、生成的 C 以它开头。
- `typeck.ml`（2571 行）按依赖的顺序拆成 `typeck_env`、`typeck_builtins`、`typeck_regions`、`typeck_generics`、`typeck_pattern`、`typeck_expr` 和 `typeck`（整个程序），代码不变，其他模块照旧通过 `Typeck` 访问。

## 0.2.0-pre（2026-11）

第一个公开的预发布版本，按 MIT 或 Apache-2.0 双许可发布。和 0.1.0 相比：

**语言**
- 模块和 `pub`：一个文件一个模块，`use geom`、`use geom { area, Point }`（C34）。
- 闭包 `|x| e`，参数类型 `fn(T) -> U`；可以捕获引用和区域（C40、C41，Lean M21、M22）。
- `()` 作类型实参，`Result<(), E>`（C37，Lean M9u）；`panic(msg)`（C38，Lean M19）。
- 元组 `(A, B)`、`let (x, y) = e`（C45）。
- `match` 的 `_` 分支和字面量分支（C46，Lean M23）。
- 浮点数 `F64`（C47）；字符 `Char` 和 UTF-8 字符串操作（C48，Lean S7）。
- 字符串插值 `"x = {x}"`、`x.to_str()`（C49）；字符串转义 `\r`（C52）。
- `for x in e` 可以遍历函数的返回值（C48）。
- 下标 `v[i]`，按下标赋值 `v[i] = x`（先析构旧值，越界时 panic；C53，Lean M18）。
- 命令行参数和文件读写：`args()`、`read_file`、`write_file`（C36，Lean S3）；不换行的输出 `print_str`、原始字节 `print_byte`（C53）。

**标准库**（`std/`，用 Sinter 写）
- `std.math`、`std.option`、`std.result`、`std.vec`、`std.str`（C35）；`std.char`（C48）；`std.io`（C52）。
- `std.map`（有序 Vec，二分查找，Lean S4、S5）、`std.treemap`（AVL 树，Lean S6）、`std.hashmap`（开放寻址，Lean S9）。
- `vec.sort`、`vec.sort_by`（堆排序，Lean S8）；`map`、`filter`、`fold` 等高阶函数；`replace`、`lines`、`gcd` 等常用函数。

**工具**
- 报错时显示出错的那一行并标出位置（T1）；英文错误信息 `--lang=en`。
- `sinterc fmt [--check | -w]`（T2）。
- CI：每次提交在 Linux 和 macOS 上构建、跑全部测试和格式检查；取来证明仓库 sinter-formal，检查证明、翻译验证和三方对照（T4）。

**验证**
- 42000 个程序的三方对照（Lean、sinterc、运行结果），包括移动、调用、闭包和泛型（C44，Lean C5c）。
- `validate.sh`：所有示例编译出的控制流图由 Lean 独立检查。

**不兼容的改动**
- 字符串字面量里的 `{` `}` 现在是插值，字面的花括号要写成 `{{` `}}`。

## 0.1.0

第一个能试用的版本：结构体、枚举、`Box`、`Vec`、`Str`、所有权和借用、`linear struct`、`Result` 和 `?`、
`errdefer`、arena 和引用、泛型和 trait、`for` 循环、`read_line`。

## 旧编号对照

文件按内容命名以后，代码、测试和 README 里不再写里程碑编号；编号只留在本文件和设计报告里。下面是旧名字和新名字的对照。

**Lean 证明**（sinter-formal 仓库，模块 `Core.X.Y` 就是文件 `Core/X/Y.lean`）

| 旧编号 | 新模块 | 内容 |
|---|---|---|
| Basic | `Core.Basic` | 基础定义 |
| M1 | `Core.Ownership.Move` | 所有权与移动（直线代码） |
| M2 | `Core.Ownership.Drop` | 作用域结束时自动释放，没有泄漏 |
| M2b | `Core.Ownership.Branch` | 分支与 drop flag |
| M2n | `Core.Ownership.Nested` | 任意嵌套的分支 |
| M2L | `Core.Ownership.Loop` | 循环 |
| M13 | `Core.Ownership.Partial` | 部分移动 |
| M3 | `Core.Calls.Modes` | 参数约定与排他性（两参数调用） |
| M3n | `Core.Calls.NArgs` | n 个参数的调用与求值顺序 |
| M4 | `Core.Projection.Lock` | 投影与源对象锁定 |
| M4L | `Core.Projection.Liveness` | 活跃性 |
| M4C | `Core.Projection.Branches` | 投影与分支、循环 |
| M4D | `Core.Projection.CallArg` | 投影作为调用参数 |
| M4E | `Core.Projection.Dataflow` | 分支、循环上的活跃性分析 |
| M5 | `Core.Regions.Model` | 区域的基本模型 |
| M5b | `Core.Regions.Variance` | 协变与 Mut |
| M5c | `Core.Regions.NoDrop` | 区域里不放带 Drop / linear 的值 |
| M5d | `Core.Regions.Params` | 函数的区域参数之间没有顺序 |
| M14 | `Core.Regions.Generic` | 区域参数与泛型实例化 |
| M15 | `Core.Regions.Data` | 含引用的数据类型 |
| M16 | `Core.Regions.DataVariance` | 含引用的数据类型的协变 |
| M6 | `Core.Linearity.Linear` | linear 值 |
| M6e | `Core.Linearity.Errdefer` | errdefer 与出错退出 |
| M7 | `Core.Unified.Base` | 合并成一个演算（定义部分） |
| M7b | `Core.Unified.Call` | 合并演算加入调用 |
| M7c | `Core.Unified.Errdefer` | 合并演算加入 errdefer 与出错退出 |
| M7d | `Core.Unified.RegionDefs` | 合并演算加入区域（定义部分） |
| M7e | `Core.Unified.RegionProofs` | 合并演算加入区域（证明部分） |
| M7f | `Core.Unified.NoDrop` | 合并演算里区域不放带 Drop 的值 |
| M7g | `Core.FlowGraph.Check` | CFG 上的数据流检查（所有权 + 线性值） |
| M7h | `Core.FlowGraph.Region` | CFG 加入区域与 errdefer |
| M7i | `Core.FlowGraph.Lower` | 结构化程序到 CFG 的翻译与对照测试 |
| M7j | `Core.FlowGraph.Simulation` | 降级正确：模拟关系 |
| M7k | `Core.FlowGraph.Embed` | 降级正确：lower 满足嵌入关系 |
| M19 | `Core.FlowGraph.Panic` | panic：中止程序 |
| C5 | `Core.Checking.Validate` | 编译器输出的翻译验证 |
| C5b | `Core.Checking.Print` | 把对照测试的程序打印成 Sinter 源码 |
| C5c | `Core.Checking.Suite` | 对照测试的第三组：整体移动、调用约定、闭包、泛型 |
| M8 | `Core.Datatypes.Enum` | 枚举：构造、按移动的 match、按标签析构 |
| M11 | `Core.Datatypes.Recursive` | 递归类型：经过 Box 的链与树 |
| M12 | `Core.Datatypes.Vec` | 一般的拥有关系与动态数组 Vec<T> |
| M17 | `Core.Datatypes.Str` | 字符串 Str |
| M18 | `Core.Datatypes.VecInout` | for x in inout v：循环里改 Vec 的元素 |
| M20 | `Core.Datatypes.VecSwap` | vec_swap：交换 Vec 的两个元素 |
| M8b | `Core.Matching.Borrow` | 按借用的 match |
| M23 | `Core.Matching.Wildcard` | match 的通配分支 _ |
| M24 | `Core.Matching.Nested` | 嵌套模式 |
| M24b | `Core.Matching.Coverage` | 嵌套模式的覆盖性检查 |
| M9 | `Core.Generics.Check` | 泛型：定义处按抽象类型检查 |
| M9u | `Core.Generics.UnitArg` | () 作类型实参 |
| M10 | `Core.Traits.Static` | trait 约束与静态派发 |
| M25 | `Core.Traits.ImplSearch` | 泛型 impl 的查找 |
| M21 | `Core.Closures.Param` | 闭包（当实参传，可以读、改外面的变量） |
| M22 | `Core.Closures.CapRef` | 闭包捕获引用 |
| M26 | `Core.Closures.Stored` | 可以存起来的闭包 Fn(T) -> U 和 trait 对象 dyn Tr |
| M27 | `Core.Strings.Slice` | 字符串切片和静态的字符串字面量 |
| M28 | `Core.Strings.ChunkLines` | 逐块读行 |
| M29 | `Core.Traits.Inherent` | 类型自己的方法（按接收者类型查找） |
| S1 | `Core.Library.Math` | std.math 的规格和定律 |
| S2 | `Core.Library.StrVec` | std.str、std.vec 的规格和定律 |
| S3 | `Core.Library.FileIO` | 文件与参数 IO 的规格 |
| S4 | `Core.Library.Map` | std.map 的规格 |
| S5 | `Core.Library.MapSearch` | std.map 的二分查找 |
| S6 | `Core.Library.TreeMap` | 平衡树的映射 std.treemap |
| S6b | `Core.Library.TreeMapAvl` | std.treemap 的 AVL 平衡 |
| S7 | `Core.Library.Utf8` | UTF-8：Char 和按字符遍历字符串 |
| S11 | `Core.Library.Utf8Check` | UTF-8 的合法性检查 str_from_utf8 |
| S8 | `Core.Library.HeapSort` | 堆排序：std.vec 的 sort、sort_by |
| S9 | `Core.Library.HashMap` | 开放寻址的哈希表 std.hashmap |
| S10 | `Core.Library.BigInt` | 大整数 std.bigint |

**测试**（`test/`）

| 旧 | 新 |
|---|---|
| `c0.t` | `basics.t` |
| `c1.t` | `own_box.t` |
| `c2.t` | `own_params.t` |
| `c3.t` | `linear_errdefer.t` |
| `c4.t` | `region_arena.t` |
| `c6.t` | `data_enum.t` |
| `c7.t` | `match_borrow.t` |
| `c8.t` | `generic_basic.t` |
| `c9.t` | `trait_basic.t` |
| `c10.t` | `data_recursive.t` |
| `c11.t` | `own_unbox.t` |
| `c12.t` | `vec_basic.t` |
| `c13.t` | `syntax_methods.t` |
| `c14.t` | `own_partial_move.t` |
| `c15.t` | `own_field_assign.t` |
| `c18.t` | `own_partial_nested.t` |
| `c19.t` | `region_generic.t` |
| `c20.t` | `region_data.t` |
| `c24.t` | `region_variance.t` |
| `c25.t` | `cli_run_build.t` |
| `c26.t` | `io_print.t` |
| `c27.t` | `cli_lang_en.t` |
| `c28.t` | `diag_multi.t` |
| `c29.t` | `str_basic.t` |
| `c30.t` | `loop_for.t` |
| `c31.t` | `loop_break.t` |
| `c32.t` | `io_input.t` |
| `c33.t` | `loop_for_inout.t` |
| `c34.t` | `module_use.t` |
| `c35.t` | `std_basic.t` |
| `c36.t` | `io_files.t` |
| `c37.t` | `generic_unit.t` |
| `c38.t` | `panic.t` |
| `c39.t` | `std_map.t` |
| `c40.t` | `closure_param.t` |
| `c41.t` | `closure_capref.t` |
| `c42.t` | `std_map_bsearch.t` |
| `c43.t` | `std_treemap.t` |
| `c45.t` | `data_tuple.t` |
| `c46.t` | `match_wildcard.t` |
| `c47.t` | `num_float.t` |
| `c48.t` | `str_char.t` |
| `c49.t` | `str_interp.t` |
| `c50.t` | `std_sort.t` |
| `c51.t` | `std_hashmap.t` |
| `c52.t` | `std_common.t` |
| `c53.t` | `vec_index.t` |
| `c54.t` | `std_bigint.t` |
| `c55.t` | `match_nested.t` |
| `c56.t` | `trait_tostr_default.t` |
| `c57.t` | `str_bytes.t` |
| `c58.t` | `std_os.t` |
| `c59.t` | `closure_stored.t` |
| `c60.t` | `io_stderr_release.t` |
| `c61.t` | `str_slice.t` |
| `c62.t` | `io_read_in.t` |
| `c63.t` | `io_line_reader.t` |
| `t1.t` | `diag_caret.t` |
| `t2.t` | `fmt.t` |
| `t3.t` | `editor_syntax.t` |

**翻译验证的例子**（`examples/`）

| 旧 | 新 |
|---|---|
| `ok/c0_ok.sn` | `ok/basics.sn` |
| `ok/c0_order.sn` | `ok/basics_order.sn` |
| `ok/c1_ok.sn` | `ok/own_box.sn` |
| `ok/c2_ok.sn` | `ok/own_params.sn` |
| `ok/c2_nll.sn` | `ok/own_params_nll.sn` |
| `ok/c3_ok.sn` | `ok/linear_errdefer.sn` |
| `ok/c4_ok.sn` | `ok/region_arena.sn` |
| `ok/c14_partial.sn` | `ok/own_partial_move.sn` |
| `ok/c15_refill.sn` | `ok/own_field_refill.sn` |
| `ok/c18_nested.sn` | `ok/own_partial_nested.sn` |
| `ok/c19_generic.sn` | `ok/region_generic.sn` |
| `ok/c20_data.sn` | `ok/region_data.sn` |
| `ok/c24_cov.sn` | `ok/region_variance.sn` |
| `ok/c29_str.sn` | `ok/str_basic.sn` |
| `ok/c30_for.sn` | `ok/loop_for.sn` |
| `ok/c31_break.sn` | `ok/loop_break.sn` |
| `ok/c32_input.sn` | `ok/io_input.sn` |
| `ok/c33_inout.sn` | `ok/loop_for_inout.sn` |
| `ok/c35_std.sn` | `ok/std_basic.sn` |
| `ok/c36_io.sn` | `ok/io_files.sn` |
| `ok/c37_unit.sn` | `ok/generic_unit.sn` |
| `ok/c38_panic.sn` | `ok/panic.sn` |
| `ok/c39_map.sn` | `ok/std_map.sn` |
| `ok/c40_closure.sn` | `ok/closure_param.sn` |
| `ok/c41_capref.sn` | `ok/closure_capref.sn` |
| `ok/c42_bsearch.sn` | `ok/std_map_bsearch.sn` |
| `ok/c43_treemap.sn` | `ok/std_treemap.sn` |
| `ok/c45_tuple.sn` | `ok/data_tuple.sn` |
| `ok/c46_pattern.sn` | `ok/match_wildcard.sn` |
| `ok/c47_float.sn` | `ok/num_float.sn` |
| `ok/c48_char.sn` | `ok/str_char.sn` |
| `ok/c49_interp.sn` | `ok/str_interp.sn` |
| `ok/c50_sort.sn` | `ok/std_sort.sn` |
| `ok/c51_hashmap.sn` | `ok/std_hashmap.sn` |
| `ok/c52_common.sn` | `ok/std_common.sn` |
| `ok/c53_index.sn` | `ok/vec_index.sn` |
| `ok/c54_bigint.sn` | `ok/std_bigint.sn` |
| `ok/c55_pattern.sn` | `ok/match_nested.sn` |
| `ok/c56_traits.sn` | `ok/trait_tostr_default.sn` |
| `ok/c57_bytes.sn` | `ok/str_bytes.sn` |
| `ok/c58_os.sn` | `ok/std_os.sn` |
| `ok/c59_fn.sn` | `ok/closure_stored.sn` |
| `ok/c61_view.sn` | `ok/str_slice.sn` |
| `bad/c1_branch.sn` | `bad/own_move_branch.sn` |
| `bad/c1_loop.sn` | `bad/own_move_loop.sn` |
| `bad/c1_move.sn` | `bad/own_use_after_move.sn` |
| `bad/c2_lock.sn` | `bad/own_params_lock.sn` |
| `bad/c3_branch.sn` | `bad/linear_branch.sn` |
| `bad/c3_double.sn` | `bad/linear_double.sn` |
| `bad/c3_forget.sn` | `bad/linear_forget.sn` |
| `bad/c3_noguard.sn` | `bad/linear_noguard.sn` |
| `bad/c14_twice.sn` | `bad/own_partial_twice.sn` |
| `bad/c14_whole.sn` | `bad/own_partial_whole.sn` |
| `bad/c22_escape.sn` | `bad/region_escape.sn` |
| `bad/c22_outer.sn` | `bad/region_outer.sn` |
