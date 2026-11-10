# 试用 Sinter（0.2.0-pre）

[English](TRY.en.md)

Sinter 是一个简化的 Rust：有所有权、移动、借用和区域（arena），但没有生命周期标注；
编译器检查的核心规则在 Lean 4 里证明过。`sinterc` 把 `.sn` 源文件编译成 C，再交给系统的 C 编译器。

这是一个**预发布版本**：语言和标准库还会变，欢迎试用和反馈（见最后一节）。

## 安装

需要：OCaml 5.1 以上（用 opam 安装）、dune 3、menhir，以及 GCC 或 Clang。

```sh
opam install dune menhir
git clone https://github.com/clawbolt/sinterc.git compiler
cd compiler
dune build
dune install        # 把 sinterc 装进当前 opam 环境；不想安装的话用 dune exec sinterc -- …
sinterc --version   # sinterc 0.2.0-pre
```

想自己检查证明的话，另外需要 Lean 4（用 elan 安装）。证明在单独的仓库
[sinter-formal](https://github.com/clawbolt/sinter-formal)，要克隆到 `compiler` 旁边的 `lean` 目录：

```sh
git clone https://github.com/clawbolt/sinter-formal.git lean   # 和 compiler 放在同一个目录下
cd lean && lake build              # 所有证明（Lean 的版本见 lean-toolchain）
cd ../compiler && sh validate.sh   # 示例程序编译出的控制流图交给 Lean 独立检查
sh diff.sh                         # 42000 个程序：Lean、sinterc、运行结果三方对照
```

## 第一个程序

`examples/tour/` 里有两个一览程序：

```sh
sinterc run examples/tour/tour.sn       # 结构体、参数约定、Box、Vec、match、arena
echo "a b a" | sinterc run examples/tour/words.sn   # 词频统计：文件、字符串、HashMap、排序、闭包
sinterc build examples/tour/tour.sn     # 生成可执行文件 examples/tour/tour
sinterc examples/tour/tour.sn -o t.c    # 只生成 C
sinterc fmt -w examples/tour/*.sn       # 格式化
```

C 编译器用环境变量 `CC` 指定，默认是 `cc`。

## 语言一览

- **类型**：`I64`、`F64`、`Bool`、`Char`、`Str`、`()`、结构体、枚举、元组 `(A, B)`、`Box<T>`、`Vec<T>`、
  `Result<T, E>`、`Option<T>`、引用 `&T in a`。数字之间不做隐式转换（`to_f64`、`to_i64`）；整数溢出、除以零会 panic。
- **变量**：`let` 不可重新赋值，`var` 可以。
- **循环**：`while c { … }`；`for i in 0..n { … }`；`for x in v { … }` 按借用遍历 `Vec`（循环中 `v` 被锁住），
  `for x in inout v` 可以改元素；`v` 也可以是函数的返回值（`for c in s.chars()`）；`break`、`continue`。
- **下标**：`v[i]` 是第 i 个元素（就是 `vec_get(v, i)`），可以读、借用、改字段（`v[i].x = 1`），也可以整体赋值
  `v[i] = x`（旧值先析构）；越界时 panic。元素不能直接移走，要拿走用 `v.pop()`。
- **参数约定**：默认 `let`（只读借用，调用后实参还能用）；`inout`（修改调用方的变量或字段）；`sink`（拿走所有权）。
- **所有权**：`Box`、`Vec`、`Str` 和含有它们的值会被移动，作用域结束时自动析构；移走之后再用是编译错误。
  可以只移走结构体的某个字段，之后再放回。
- **`linear struct`**：必须显式 `consume`，忘了处理就是编译错误；配合 `Result`、`?`、`errdefer` 写出错路径。
  `panic(msg)` 立刻结束程序（退出码 101）。
- **枚举和 `match`**：`.Some(x)`、`.None`；`_` 分支；整数、字符、字符串、`true`/`false` 字面量分支（最后要有 `_`）。
- **泛型和 trait**：`fn same<T: math.Eq>(a: T, b: T) -> Bool`，单态化、静态派发。方法调用 `v.push(x)` 等同于 `push(v, x)`。
- **闭包**：参数类型写 `fn(T) -> U`，实参写 `|x| e`。闭包只能当实参传（不能存、不能返回），
  可以读外面的变量，也可以改外面的 `var`。
- **引用只能来自 arena**：`arena |a| { let r = alloc(a, 1) … }`，引用不能活过它的 arena。
- **模块**：一个文件一个模块，`use geom` 引入同目录的 `geom.sn`，用 `geom.area(…)` 访问标了 `pub` 的条目。
- **字符串**：`Str` 存 UTF-8 字节。`s.len()` 是字节数，`s.chars()` 逐个解出 `Char`，`s.char_count()` 是字符数，
  `s.push(t)`、`s.push_char(c)`、`s.slice(a, b)`、`s.to_i64()`、`s.to_f64()`。
  插值：`"x = {x}, 和 = {a + b}"`，`{{`、`}}` 是花括号本身；I64、F64、Char、Bool、Str 以外的类型要写 `fn to_str(x: T) -> Str`。
  转义有 `\n`、`\r`、`\t`、`\\`、`\"`；字符字面量 `'a'`、`'中'`、`'\n'`。
- **输入输出**：`print(s)`、`print_i64`、`print_f64`、`print_char`（都会换行）；`print_str(s)` 不换行，
  `print_byte(b)` 写一个原始字节（0–255）；`read_line()`、`args()`、`read_file`、`write_file`。

## 标准库

用 `use std.名字` 引入，之后用 `名字.函数(…)` 调用。源码在 `std/` 下。

| 模块 | 内容 |
|---|---|
| `std.math` | trait `Eq`、`Ord`；`min`、`max`、`abs`、`pow`、`gcd`、`lcm`；F64 的 `sqrt`、`floor`、`round`、`pi` 等 |
| `std.option`、`std.result` | `is_some`、`unwrap_or`、`unwrap`、`expect` 等 |
| `std.vec` | `map`、`filter`、`fold`、`any`、`all`、`count`、`sum`、`max`、`min`、`reverse`、`contains`、`range`、`sort`、`sort_by`、`binary_search`、`dedup` |
| `std.str` | `split`、`join`、`trim`、`find`、`contains`、`starts_with`、`replace`、`lines`、`to_upper`、`to_lower`、`repeat` |
| `std.char` | `is_digit`、`is_alpha`、`is_whitespace`、`to_upper`、`to_digit` 等（只认 ASCII） |
| `std.io` | `read_lines`、`write_lines`、`input_lines` |
| `std.map` | 有序映射（按键排好的两个 Vec，二分查找） |
| `std.treemap` | 有序映射（AVL 树，对数时间的插入和删除） |
| `std.hashmap` | 哈希表（开放寻址；键实现 `Eq` 和 `hashmap.Hash`，I64 和 Str 已实现） |
| `std.bigint` | 任意大小的整数：`from_i64`、`parse`、`to_str`、`add`、`sub`、`mul`、`pow`、`divmod_small`（除以不超过 10^9 的数），实现了 `Eq`、`Ord` |

排序、二分查找、三种映射和大整数的核心算法都在 Lean 里证明过（sinter-formal 仓库 README 的 S4–S10）。

## 工具

- `sinterc run 文件 [参数…]`、`sinterc build 文件 [-o 程序]`、`sinterc 文件 [-o 输出.c]`。
- `sinterc fmt [--check | -w] 文件…`：格式化。只调整空白（缩进、运算符两边的空格、空行），不改变记号和换行的位置，注释原样保留。
- 错误信息默认中文，`--lang=en`（或环境变量 `SINTER_LANG=en`）换成英文。报错时显示出错的那一行，用 `^` 标出位置。

各个特性更完整的例子在 `test/*.t`：每个文件开头写了这一步加了什么，后面是能运行的程序和应该被拒绝的程序。

## 已知限制

- 闭包不能存进变量或数据结构，也不能作返回值；不能嵌套定义闭包。
- `match` 的模式不能嵌套（`.Some((a, b))` 要分两层写）。
- `to_str` 不能按类型重载，一个程序里只能给一种自定义类型写（以后由 trait 解决）。
- `std.char` 和大小写转换只认 ASCII；`sort` 不稳定（相等的元素先后可能变）。
- `Box`、`Vec` 里的类型参数在实例之间算不变，比 Rust 严：有些 Rust 能写的程序这里会被拒绝。
- 生成的 C 需要 GCC 或 Clang（用到了 GNU C 扩展）。还没有编辑器插件和语言服务器。
- 还没有：trait 对象（动态派发）、大整数除以大整数、正则、线程和网络、目录遍历和启动子进程、包管理。
  `Str` 必须是合法的 UTF-8，二进制数据目前只能用 `print_byte` 写到标准输出，还不能写进文件。

## 反馈

最有用的反馈：
- 你觉得应该通过、却被拒绝的程序（附上源文件和错误信息）；
- 你觉得应该被拒绝、却通过了或者运行时崩溃、泄漏的程序——这是最严重的问题，请一定告诉我们；
- 看不懂的错误信息。

## 许可证

MIT 或 Apache-2.0，任选其一（[LICENSE-MIT](LICENSE-MIT)、[LICENSE-APACHE](LICENSE-APACHE)）。
