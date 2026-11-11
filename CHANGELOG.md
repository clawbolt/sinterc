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

**标准库**
- `std.bigint`：任意大小的整数，加、减、乘、比较、除以小数、十进制转换，实现 `Eq`、`Ord`（C54，Lean S10）。
- `std.os`：`env`、`list_dir`、`exists`、`is_dir`、`make_dir`、`remove`，启动子进程 `run`（返回退出码）和 `output`
  （收集标准输出），不经过 shell；`exit(code)`（C58）。
- `eprint(s)`、`eprint_str(s)`：写到标准错误（C60）。
- `std.view`：字符串切片的 `split`、`lines`、`trim`、`starts_with`、`ends_with`、`to_i64`，结果是同一块缓冲区里的切片（C61）。
- `read_file_in(a, path)`、`read_stdin_in(a)`、`read_chunk(a, n)`：把文件、标准输入（任意字节）直接读进 arena，得到切片（C62）。
- `std.str` 的 `find`、`split`、`replace`、`starts_with`、`ends_with` 逐字节比较，不再为每个位置复制子串（C62）。

**验证**
- AVL 平衡：`std.treemap` 的插入、删除保持平衡，树高是对数（Lean S6b）。
- 切片的边界、arena 只增不改、静态字面量的分配计数（Lean M27）。

**工具**
- `sinterc build --release`、`sinterc run --release`：`-O2`，不统计分配、不做退出时的泄漏检查（C60）。
- 编辑器的语法高亮：`editor/vscode`（VS Code，含字符串插值）和 `editor/vim`（T3）。
- 随机测试 `sh fuzz.sh`：fmt 对空白不敏感、格式化前后记号不变且幂等，编译器对随机改坏的程序不出内部错误；CI 每次换一个种子跑（T5）。

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
