# sinterc 更新记录 / Changelog

证明在单独的仓库 [sinter-formal](https://github.com/clawbolt/sinter-formal)，和编译器的版本一起更新。

## 0.2.0-pre（2026-11）

第一个公开的预发布版本。和 0.1.0 相比：

**语言**
- 模块和 `pub`：一个文件一个模块，`use geom`、`use geom { area, Point }`（C34）。
- 闭包 `|x| e`，参数类型 `fn(T) -> U`；可以捕获引用和区域（C40、C41，Lean M21、M22）。
- `()` 作类型实参，`Result<(), E>`（C37，Lean M9u）；`panic(msg)`（C38，Lean M19）。
- 元组 `(A, B)`、`let (x, y) = e`（C45）。
- `match` 的 `_` 分支和字面量分支（C46，Lean M23）。
- 浮点数 `F64`（C47）；字符 `Char` 和 UTF-8 字符串操作（C48，Lean S7）。
- 字符串插值 `"x = {x}"`、`x.to_str()`（C49）；字符串转义 `\r`（C52）。
- `for x in e` 可以遍历函数的返回值（C48）。
- 命令行参数和文件读写：`args()`、`read_file`、`write_file`（C36，Lean S3）。

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
