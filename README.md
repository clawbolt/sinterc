# sinterc：Sinter 编译器

版本 0.2.0-pre（预发布）。上手看 [TRY.md](TRY.md)（[English](TRY.en.md)），每一步加了什么看 [CHANGELOG.md](CHANGELOG.md)。
证明在单独的仓库 [sinter-formal](https://github.com/clawbolt/sinter-formal)：克隆到本仓库旁边的 `lean` 目录后，
`sh validate.sh`（翻译验证）和 `sh diff.sh`（三方对照）才能运行。下文的 Lean 模块（如 `Ownership.Move`）都在那个仓库的 `Core/` 下。

本文件是各部分的约定细节；语言的完整一览（包括 `F64`、`Char`、`Str`、`Bytes`、`Vec`、元组、闭包、模块、标准库）见 [TRY.md](TRY.md)。

## 依赖

OCaml 5.3、dune ≥ 3.0、menhir，以及系统的 C 编译器（`cc`，需要支持 `__builtin_add_overflow` 一类内建函数和语句表达式，GCC 和 Clang 都支持）。

```
opam install dune menhir
eval $(opam env)               # 新开的 shell 里找不到 menhir 时先运行这一行（或者用 opam exec -- dune build）
```

## 构建与测试

在本仓库的根目录下：

```
dune build
dune test                      # test/*.t 里的端到端测试
sh fmtcheck.sh                 # 示例和标准库都已格式化
sh validate.sh                 # 翻译验证（需要 ../lean）
sh diff.sh                     # 三方对照（需要 ../lean）
sh fuzz.sh [轮数] [种子]         # 随机测试
dune exec sinterc -- 文件.sn -o 文件.c
cc -std=gnu11 文件.c -o 文件 && ./文件
```

脚本的临时文件（复制出来的 `sinterc`、对照用的程序）放在 `_batch/`，可以随时删掉。

## 目录

| 目录 | 内容 |
|---|---|
| `bin/main.ml` | 命令行：`run`、`build`、`fmt`、`--emit=core`、`--batch` |
| `src/` | 编译器本身，见下面的编译流程 |
| `runtime/` | C 运行时：`sinter.h`（分配计数、字符串、切片、Vec 的宏、入口）放在生成的 C 文件开头，`sinter_lib.h`（I/O、解析、`std.os`，用到 `Option<Str>` 等内建实例类型）放在类型定义之后 |
| `std/` | 标准库（Sinter 写的，构建时由 `gen/embed.ml` 编进编译器） |
| `test/` | 端到端测试，一个文件一个主题，文件名前缀是主题：`own_` 所有权、`region_` 区域、`str_` 字符串、`io_` 输入输出、`std_` 标准库，等等 |
| `examples/ok`、`examples/bad` | 翻译验证用的程序：`ok` 的每个函数都要被 Lean 接受，`bad` 的要被 Lean 独立拒绝 |
| `examples/tour` | TRY.md 里的一览程序 |
| `fuzz/` | 随机测试 |
| `editor/` | VS Code、Vim 的语法高亮 |

## 编译流程

解析（`lexer.mll`、`parser.mly`）→ 模块（`modules.ml`）→ 类型检查（`typeck.ml` 和 `typeck_*.ml`）→ 降级为 MIR 控制流图（`mir.ml`，作用域结束和 `return` 前插入 `Drop`）
→ 所有权检查与析构展开（`own.ml`）→ 投影锁定与排他性检查（`borrow.ml`）→ 生成 C（`emit_c.ml`，每个基本块一个标签，块之间用 `goto`；C 运行时在 `runtime/`）。
`emit_core.ml` 把 MIR 导出成 Lean 的 CFG（翻译验证），`fmt.ml` 是格式化，`i18n.ml` 是英文错误信息。

`own.ml` 是 `Ownership.Drop` / `FlowGraph.Check` 规则的实现：每个局部变量处于“已初始化 / 未初始化 / 可能已初始化”之一，
前向数据流、分支汇合取并、循环迭代到不动点。读取不是“已初始化”的变量报错；
`Drop` 和给 `var` 重新赋值前的析构，按状态变成直接析构、去掉，或者按 drop flag 析构。

`borrow.ml` 对应 `Calls.NArgs` 和 `Projection.*`：后向数据流算出每个程序点之后还会用到的投影，
投影活着时检查它的源对象有没有被移动、修改、以 `inout` 传入或析构；调用时再两两检查实参的排他性。

## 语法和基本规则

- 语句之间用换行分隔（也可以用 `;`），圆括号里的换行忽略，花括号里重新分隔：`f(v, |x| { 语句 \n 语句 })` 里闭包体可以写多行；块的最后一条若是表达式，就是块的值。
- 只有 `var` 声明的变量和 `inout` / `sink` 参数能重新赋值；`let` 声明的变量和 `let` 参数不能。
- 函数参数、结构体字段一律从左到右求值（降级到 MIR 时操作数依次存进临时变量，不依赖 C 的求值顺序）。
- 整数运算检查溢出和除以零，出错时打印 `panic: …` 并以退出码 101 结束。
- 堆上的值有分配计数：程序结束时还有没释放的，打印 `leak: …` 并以退出码 102 结束（`--release` 时不计数，见下）。
- 错误信息格式：`文件:行:列: 错误：…`，下面两行是出错的那一行和标出列的 `^`（中文字符占 2 格，和整个程序有关的错误不显示源码），退出码 1。
  每个函数报它的第一个错误，最多 20 个。`--lang=en` 或环境变量 `SINTER_LANG=en` 时用英文。
- 模块：一个文件一个模块。文件开头写 `use geom` 引入同目录的 `geom.sn`，之后用 `geom.area(…)`、`geom.Point`、`T: geom.Show` 访问它标了 `pub` 的函数、结构体、枚举、trait；
  `use geom { area, Point }` 另外把这几个名字直接引入。没标 `pub` 的只能在本文件里用；`impl` 不写 `pub`，到处可见；trait 的方法名不加模块前缀。
  `consume(x)` 只能在定义这个 `linear` 类型的文件里用。变量不能和引入的模块同名。模块里的错误报在模块的文件里。
- 元组：`(a, b)` 构造，`t.0` 取元素，`t.0.1 = e` 赋值，`let (x, y) = e` 拆开；类型写成 `(A, B)`，可以嵌套、作返回值和泛型实参（`Vec<(Str, I64)>`）。
  它是内建的泛型结构体 `Tuple#n`（字段 `0`、`1`、…，名字用户写不出来），规则和证明沿用泛型结构体（`Datatypes.Enum`、`Ownership.Partial`）。元素都可复制时元组可复制，否则只能移动；2 到 6 个元素。
- 浮点数 `F64`：字面量 `1.5`、`2e-3`；`+ - * /` 和比较要求两边类型相同（不做隐式转换，用 `to_f64`、`to_i64`，后者向零取整，超出范围或 NaN 时 panic）；`%` 只用于 I64。
  按 IEEE 754，不检查溢出和除以零。`print_f64` / `str_from_f64` 打印能还原的最短写法（整数值带 `.0`），`str_to_f64` / `s.to_f64()` 解析。
- `()`（Lean `Generics.UnitArg`）：只有一个值的 copy 类型，可以作泛型函数、`Vec`、`Option` 等的类型实参，也可以是变量、参数、字段、`Box` 里的值；
  `Result<(), E>` 的 `Ok` 有一个 `()` 载荷，可以省略不写（`Ok` 就是 `Ok(())`，分支可以写 `Ok =>`）。还不能引用 `()`，区域里也不能放 `()`。
- `panic(msg)`（Lean `FlowGraph.Panic`）：打印说明后立刻结束进程（退出码 101），类型是 Never。不析构任何值、不执行 `errdefer`、
  不要求线性值已被消费——线性值的保证只对正常返回和 `Err` 返回成立；需要清理的出错情况用 `Result`。

## 所有权、参数和投影

- `Box<T>`、`Vec<T>`、`Str` 和含它们的结构体需要析构，只能移动、不能复制；`I64`、`Bool` 和只含它们的结构体按值复制。
  变量结构体可以直接移走单个字段；不能直接通过 `Box`、`Vec` 或多层字段移出，改用 `unbox`、`vec_pop` 或先拆开结构体。
- `a.f = e` 要求 `a` 以可修改的 `var` / `inout` / `sink` 位置为根；旧字段需要析构时先析构，`linear` 字段不能直接赋值。
- 参数约定：`let`（不写时的默认）只读借用，`inout` 可变借用（实参必须是可修改的位置），`sink` 转移所有权（可复制的类型按值复制）。
  `let` / `inout` 参数在 C 里是指针，不拥有值，不能移走；给 `inout` 参数整体赋值会先析构旧值。
- 排他性：同一次调用里同一个变量出现两次，两次都必须是只读的（`let` 或复制）。
- 投影：`let a = p.a` 这类绑定（值需要析构、又不能整个移走）不拷贝，`a` 指向 `p` 内部。
  `a` 之后还会用到时，`p` 不能被移动、修改、以 `inout` 传入或析构；锁在 `a` 最后一次使用之后结束。锁住的是整个 `p`。
- `for x in inout v`（Lean `Datatypes.VecInout`）：`x` 是第 i 个元素本身，循环体里不能用 `v`。`vec_swap(inout v, i, j)`（也写 `v.swap(i, j)`，Lean `Datatypes.VecSwap`）：交换两个元素，越界时 panic。

## 线性值和出错路径

- `linear` 值必须被显式消费：移动走（`sink` 实参、返回）或 `consume(x)`（析构它的字段）。
  作用域结束、`return`、出错返回三种出口上还没被消费都报错；重新赋值前也必须先消费。
- `Result<T, E>` 是变体为 `Ok(T)`、`Err(E)` 的枚举，构造、`match`（按移动或按借用）、析构都和普通枚举相同。
  `Ok(e)` / `Err(e)` 的类型由上下文确定：`let` 的类型标注、返回值、实参、字段。
- `e?`：出错时把错误值包成函数返回的 `Err`，执行生效的 `errdefer`，然后返回；错误类型必须和函数返回的相同。
- `errdefer f(…)`：只在出错返回（`?` 的出错分支和 `return Err(…)`）时执行，内层的先执行，作用到所在块结束。
  调用必须以 `sink` 方式传入恰好一个 `linear` 变量，实参只能是变量、字段或常量。
  登记时这个变量必须还没被消费，并且不能已经有生效的 `errdefer`。

## 区域

- `alloc(a, e)` 得到只读引用 `&T in a`，块结束时整块释放，`return`、`?` 穿过区域块时也释放（Lean `Regions.Model`、`Unified.RegionDefs`）。
  带区域 `a` 的值不能作为块的值带出 `a`（R3）；外层区域的引用可以当作内层的用，反过来不行（D2）；
  函数的区域参数（`Arena` 参数和参数类型里出现的区域名）之间没有顺序，调用时按实参实例化（D3）；
  需要析构或 `linear` 的值不能放进区域（D4）。区域不能存进变量、字段或返回。
- 泛型函数、`Vec`、用户的泛型结构体和枚举的类型实参可以是引用（`Regions.Generic`、`Regions.Data`），只读的数据类型协变（`Regions.DataVariance`）。

## 枚举和 match

- 每个变体至多一个载荷（`A(T)`，也可以写 `A(x: T)`，名字只作说明），载荷不能是 `()`、`linear` 值或引用（Lean `Datatypes.Enum`）。
  `.A(e)` / `.B` 的类型和 `Ok` / `Err` 一样由上下文确定。`match` 必须穷尽；
  按移动拆开：带载荷的分支把载荷移进分支变量（写 `_` 也会移走，在分支结束时析构），枚举本身变成已移走。
  各分支结束时状态不同的变量按 drop flag 析构；没被 `match` 的枚举在作用域结束时按运行时标签析构载荷。
  载荷都可复制的枚举整体按值复制。
- 按借用的 `match`（Lean `Matching.Borrow`）：`match let x { … }` 显式借用；被匹配的位置不能整个移走时
  （`let` / `inout` 参数、字段、解引用）自动借用，和 `let` 绑定投影的规则相同。枚举本身不动，`match` 之后照常可用。
  需要析构的载荷绑定成投影（不拥有，不能移走）；投影之后还会用到时，被匹配的值不能被移动、修改、以 `inout` 传入或析构。
- `_ => e` 覆盖没写到的变体（Lean `Matching.Wildcard`：最多一个、放在最后、还有没写到的变体才能写；按移动匹配时载荷在分支结束时析构）。
  字面量分支 `0 =>`、`-1 =>`、`true =>`、`"add" =>` 展开成 if 链（字符串用 str_eq），最后必须有 `_`。
- 嵌套模式 `.Some((0, s))`、`Ok(.None)`（Lean `Matching.Nested`、`Matching.Coverage`）：展开成只读的测试和拆开取值，覆盖检查按代表值枚举。

## 泛型和 trait

- 泛型（Lean `Generics.Check`）：类型参数写在名字后面，`fn f<T, U: linear>(…)`、`struct Pair<A, B>`、`enum Option<T>`；
  用到时写 `Option<I64>`。函数的类型实参从实参类型和期望的返回类型推出来，结构体字面量的从期望的类型或字段的值推出来。
  定义处按抽象类型检查：`T` 的值只能移动（即使之后只用 `I64` 实例化）；普通的 `T` 在作用域结束时自动析构，
  `T: linear` 必须交出去（`sink` 实参或返回，泛型代码里不能 `consume`）；不能对 `T` 做运算、比较、取字段或 `match`（要靠 trait）。
  调用处检查约束：普通的 `T` 不能是 `linear` 类型，`T: linear` 什么都可以。
  单态化：用到的每个实例（`f<I64>`、`Option<Box<I64>>`）生成一份具体的代码，照常做所有权检查；泛型函数本身也检查，但不输出。
  类型实参不能是 `Arena`；泛型结构体、枚举的类型实参暂不能是 `linear` 值；泛型函数的签名里不能直接写区域。
- trait（Lean `Traits.Static`）：`trait Eq { fn eq(a: Self, b: Self) -> Bool }` 只写方法签名，参数约定照常写；
  `impl Eq for I64 { fn eq(a: I64, b: I64) -> Bool { … } }`（方法里也可以写 `Self`），`impl` 的方法要和 trait 一一对应，
  参数约定、参数类型和返回类型相同（`Self` 换成实现的类型）；同一个类型对同一个 trait 只能有一个 `impl`。
  方法可以写成 `eq(x, y)` 或 `x.eq(y)`，接收者就是第一个实参；`Self` 从实参类型推出来，方法名全局唯一（暂不支持重载）。
  约束写 `fn f<T: Eq + Show + linear>(…)`：泛型代码里只能调用约束里写了的 trait 的方法；调用处每个约束都要有 `impl`
  （类型实参本身是类型参数时，要在当前函数里写了同样的约束）。单态化时方法调用直接换成那个 `impl` 里的函数，没有虚表。
- 默认方法和泛型 impl（Lean `Traits.ImplSearch`）：trait 里写了函数体的方法是默认方法，impl 可以不写；泛型 impl `impl<T: ToStr> ToStr for Vec<T> { … }`，
  查找时递归地查约束。头部不能是单独的类型参数，impl 的类型参数要出现在头部里，同一个 trait 的两个 impl 不能重叠。
- 内建的 trait `ToStr`（`fn to_str(x: Self) -> Str`）：I64、F64、Char、Bool、Str 已经实现，字符串插值用它；
  `fn to_str(x: T) -> Str` 是 `impl ToStr for T` 的简写，可以给多种类型各写一个。
- 暂不支持：关联类型、trait 对象、结构体和枚举的类型参数写约束。

## 闭包

- 当实参传的闭包（Lean `Closures.Param`）：参数类型写 `fn(T, inout U) -> R`（返回 `()` 时写 `fn(T)`），实参写闭包 `|x, y| 表达式`，没有参数写 `|| 表达式`：
  `vec.map(v, |x| x * k)`、`vec.for_each(v, |x| { total = total + x })`。
  `fn(…)` 只能作函数参数的类型，函数参数只能调用或原样传给另一个 `fn(…)` 参数；不能存进变量、字段、Vec，也不能返回（要存起来用下面的 `Fn(…)`）。
- 闭包用到的外层变量是捕获：只读的按 `let`，赋值、作 `inout` 实参的按 `inout`（要求是 `var` 变量）。调用处把捕获当作额外的实参，
  和别的实参一起检查排他性，所以 `vec.for_each(v, |x| { v.push(x) })` 被拒绝。
- 实现是按闭包特化：每个闭包提升成普通函数（捕获在前），`h(a, |x| …)` 换成调用 `h` 的特化 `h[闭包]`。之后的所有权检查、排他性检查和
  Lean 翻译验证看到的都是普通函数。带 `fn(…)` 参数的函数本身只做类型检查，不生成代码。
- 闭包可以用含引用的变量和区域本身（Lean `Closures.CapRef`）：提升出的函数里这些区域换成区域参数 `'c00`、`'c01`…，按闭包所在位置的嵌套顺序编号，
  只保留那里真实成立的先后关系（外层 arena 比内层的活得久；函数自己的区域参数之间没有顺序）。闭包里同样不能把内层区域的引用放进外层区域的 Vec。
- 可以存起来的闭包 `Fn(T, inout U) -> R`（Lean `Closures.Stored`）：闭包写在需要 `Fn(…)` 的地方（`let` / `var` 的类型标注、返回值、实参、字段、`Vec` 元素）时，
  用到的外层变量**按值**拿进来：会移动的值（`Str`、`Vec`、`Box`、`Fn`…）被移走，可复制的值复制一份，存在堆上的环境里。
  - `Fn` 值和 `Box` 一样：只能移动，作用域结束时析构（连同环境）；可以存进变量、字段、`Vec`，也可以返回。
  - 调用写 `f(x)`（`f` 是变量或参数）或 `f.call(x)`（字段、`Vec` 元素），闭包按 `inout`：闭包可以改自己环境里的变量（计数器），
    所以 `f` 要是 `var` 变量或 `inout` / `sink` 参数。闭包体访问的环境和实参不重叠（`env_disjoint`），
    所以闭包和当实参传的一样提升成普通函数检查就够了；`w.g.call(w)` 这种闭包所在的位置又当实参的调用照常被排他性检查拒绝。
  - `Fn` 变量可以传给 `fn(…)` 参数（例如 `vec.map(v, f)`）：当作闭包 `|x| f(x)`。
  - 环境里不能有引用、区域和 `linear` 值（存起来的值不指向任何栈帧，返回之后也不会悬空）。
  - C 里是 `sn_fn`（环境指针、转发函数、析构函数）；每个闭包一个环境结构体，每种 `Fn` 类型一个 `call#…`。
    导出给 Lean 时 `Fn` 是需要析构的值，和 `Str` 一样，`validate.sh` 照常检查。
- 暂不支持：闭包里再写闭包；闭包用函数参数；`fn(…)` 里有引用；trait / impl 的方法有 `fn(…)` 参数；`errdefer` 调用函数参数。

## 字符串、字节串和切片

- `Str`（Lean `Datatypes.Str`）存 UTF-8 字节，末尾多一个 0。字面量是静态的（Lean `Strings.Slice`）：指向程序里的字符串常量，不分配；复制字面量还是字面量；析构什么都不做；
  修改（`push`、`push_char`、`Bytes` 的 `set`、`push`）前 `sn_str_own` 先复制到堆上。命令行参数、环境变量、目录项不归程序所有，复制成堆上的 `Str`。
  C 里 `lit` 和 `len` 共用 8 字节（位域，`len` 63 位），`sn_str` 是 16 字节。
- 字符 `Char`（Lean `Library.Utf8`）：一个 Unicode 标量值，可以复制；字面量 `'a'`、`'中'`、`'😀'`，转义 `'\n'`、`'\t'`、`'\\'`、`'\''`、`'\0'`；按码点比较。
  `s.chars()` 逐个解码（不合法的字节解成 U+FFFD，只吃一个字节），`s.char_count()` 是字符数，`s.push_char(c)`、`str_from_char(c)` 按 UTF-8 写入；
  `c.code()`、`char_from(n)`（代理区、超出范围时是 `.None`）在码点和字符间转换。`for x in e` 的 `e` 不是变量或字段时（`for c in s.chars()`），先放进隐藏变量再遍历，循环结束后析构。
- 字符串插值：`"x = {x}, 和 = {a + b}"`，花括号里是任意表达式（可以嵌套字符串），`{{`、`}}` 是花括号本身。值按类型用 `ToStr` 转成字符串，
  泛型代码里插值要写约束 `T: ToStr`。展开成 `{ var #s = …; str_push(#s, …); #s }`，不需要新的推理。
- 字节串 `Bytes`：任意字节，C 里和 `Str` 的表示相同，所有权也一样。
  `bytes_new`、`len`、`get`、`set`、`push`（字节是 0 到 255 的 I64，超出时 panic）、`append`、`slice`、`clone`、`eq`（`==` 不能比较字节串）；
  `str_to_bytes`（`s.to_bytes()`）、`str_from_utf8`（Lean `Library.Utf8Check`：合法时才 `.Some`）、`read_bytes`、`write_bytes`、`print_bytes`。
- 切片 `&str in a`（Lean `Strings.Slice`）：在编译器里就是区域 `a` 里的一种引用（`TRef (TView, a)`），所以“切片活不过缓冲区”
  直接由已有的区域检查保证，导出给 Lean 时也按引用复核（`validate.sh` 检查 `examples/ok/str_slice.sn`）。
  缓冲区由 `text(a, s)` 交给区域 `a`（字面量直接指向静态存储，不复制），之后不再改动，区域结束时整块释放。
  - 方法：`len`、`byte(i)`、`sub(i, j)`（越界时 panic）、`find(p, from)`、`eq(s)`、`eq_view(w)`、`print_str()`，都不分配；`to_str()` 复制成 `Str`。
  - `std.view`：`split`、`lines`、`trim`、`starts_with`、`ends_with`、`to_i64`；逐行、逐字段访问不建 `Vec` 的 `line_end(v, pos)`、`trim_cr(v)`、`field(v, sep, i)`。
  - C 里是 `sn_view { p, len }`，按值传。
  - 暂不支持：对切片做字符串插值、`match` 切片。

## 输入输出

- 输出：`print`（换行）、`print_str`、`print_byte`、`print_i64` 等；`eprint(s)`、`eprint_str(s)` 写到标准错误，写之前先刷新标准输出，所以两者合到一处（`2>&1`）时顺序和程序里一样。
- 文件与参数（Lean `Library.FileIO`）：`args()` 是命令行参数（不含程序名，`sinterc run 源文件.sn 参数…` 把后面的参数交给程序）；
  `read_file(path)` 返回 `Result<Str, Str>`；`write_file(path, s)` 返回 `Result<(), Str>`，先写 `path.sinter-tmp` 再 `rename`，
  失败时原文件不变、不留临时文件。出错说明是“路径：系统的说明”。`read_line()` 读一行，读到末尾时是 `.None`。
- 直接读进区域，不经过 `Str`：
  - `read_file_in(a, path) -> Result<&str in a, Str>`：普通文件按 `fstat` 的大小在区域里分配一块缓冲区，大小没变时只读一次、不复制。
  - `read_stdin_in(a) -> &str in a`：读完标准输入，任意字节（包括 0）。
  - `read_chunk(a, n) -> &str in a`：从标准输入最多读 `n` 个字节（`n ≤ 0` 时 panic），到头时是空的切片。
  - 都只是在区域后面追加新的缓冲区（Lean `Strings.Slice` 的 `fresh_valid`、`grow_read`），区域结束时一起释放。
- 逐块读行（Lean `Strings.ChunkLines`）：`std.view` 的 `line_reader()` 和 `next_block(r, a, n)` 每次从标准输入最多读 `n` 个字节，
  得到放在区域 `a` 里、以换行结束的一段完整的行；上一块最后一个换行之后不完整的部分留在 `LineReader` 里，接在下一块前面。
  一块里没有换行（行比块长）时接着读；输入最后没有换行时，最后一段是剩下的部分；读完时是 `.None`。
  每次循环开一个内层的 `arena |b| { … }`，在里面 `next_block(r, b, n)` 并处理这一段，峰值内存大约是两块加上最长的一行，和输入大小无关。
  不管输入怎么切成块，得到的行和整个读进来再 `lines` 的相同（`stream_eq`）；`io_line_reader.t` 用块大小 1 到 4096 检查。
- `std.os`：内建函数 `os_env`、`os_list_dir`（不含 `.` 和 `..`，按字节排好）、`os_exists`、`os_is_dir`、`os_make_dir`、`os_remove`、
  `os_run`（`posix_spawnp`，不经过 shell，返回退出码，被信号结束时是 128 + 信号）、`os_output`（另外用管道收集标准输出，退出码不是 0 时是 Err）、`os_exit`；
  `std/os.sn` 给它们短的名字。和文件读写一样只读外部状态、返回新的值；`os_exit` 和 `panic` 一样以 Halt 结尾、不析构。

## 标准库

`use std.math` 引入编进编译器的 `std/math.sn`，之后照样写 `math.min(…)`。标准库用 Sinter 写，和用户代码一样检查；
同一个程序里不能既有自己的 `math.sn` 又用 `std.math`。

- `std.math`（Lean `Library.Math`）：`min`、`max`、`abs`、`sign`、`clamp`、`pow`、`gcd`、`lcm`；trait `Eq`（`eq`）、`Ord`（`cmp`，返回 -1/0/1），I64、Bool、Str 实现了 `Eq`，I64、Str 实现了 `Ord`；
  `min_of`、`max_of`（任意 `Ord`）；浮点的 `sqrt floor ceil round fabs fmin fmax is_nan pi powi`。
- `std.option`：`is_some`、`is_none`、`unwrap_or`、`unwrap`、`expect`。`std.result`：`is_ok`、`is_err`、`unwrap_or`、`ok`、`unwrap`、`expect`（没有值或出错时 panic）。
- `std.vec`：`sum`、`max`、`min`、`sum_f64`、`range`、`contains`、`index_of`（元素要实现 `math.Eq`）、`reverse`（`inout`）、`binary_search`（排好序的 Vec，返回第一个匹配的下标）、`dedup`（原地去掉相邻重复）；
  接收闭包的 `map`、`filter`（`sink v`）、`any`、`all`、`count`、`for_each`、`fold`。
  `sort(v)`（元素实现 `Ord`）和 `sort_by(v, |a, b| a < b)`（`less` 要是严格弱序），都是 `inout` 原地排序（Lean `Library.HeapSort`）：堆排序，只用 `vec_swap` 交换元素，O(n log n)，不稳定。
- `std.str`（按字节，Lean `Library.StrVec`）：`starts_with`、`ends_with`、`find`、`contains`、`trim`、`split`、`join`、`repeat`、`to_upper`、`to_lower`（ASCII）、`replace`、`lines`（去掉行尾的 `\r`）。逐字节比较，不为每个位置复制子串。
- `std.char`：ASCII 的 `is_digit`、`is_alpha`、`to_upper`、`to_digit` 等。
- `std.io`：`read_lines`、`write_lines`、`input_lines`。
- `std.view`：见上面的切片和逐块读行。
- `std.map`（Lean `Library.Map`、`Library.MapSearch`）：`Map<K, V>`，键要实现 `math.Ord`，按键从小到大存成两个 Vec（`m.keys`、`m.vals`，只读）。
  `new`、`len`、`index_of`（取值写 `vec_get(m.vals, i)`）、`contains`、`insert`（`inout`，已有键时换值并返回旧值）、`remove`（返回去掉的值）。
  查找用二分查找，是对数时间；插入、删除要挪动元素，是线性时间。
- `std.treemap`（Lean `Library.TreeMap`、`Library.TreeMapAvl`）：`TreeMap<K, V>`，存成 AVL 树，查找、插入、删除都是对数时间。
  `new`、`len`、`height`、`contains`、`insert`、`remove`（和 `std.map` 一样）；值通过闭包拿：`get(m, k, |v| …)` 返回 `Option`，
  `for_each(m, |k, v| …)` 按键从小到大遍历。
- `std.hashmap`（Lean `Library.HashMap`）：`HashMap<K, V>`，键实现 `math.Eq` 和 `hashmap.Hash`（I64、Str 已实现）；`new`、`len`、`contains`、`get(m, k, |v| …)`、`insert`（已有键时返回旧值）、`remove`（返回值）、
  `for_each`（按插入顺序，删除会把最后一个元素换到被删的位置）。键值紧凑地存在两个 Vec 里，另有槽位表存下标；开放寻址、线性探测不绕回（槽位表长 2 倍容量，元素超过 3/4 容量时翻倍）、删除时后移回填，不需要墓碑。平均 O(1)。
- `std.bigint`（Lean `Library.BigInt`）：大整数，各位从低到高、基数 10^9。
- `std.os`：见输入输出。

## 工具

- `sinterc build --release`、`sinterc run --release`：用 `-O2` 编译，并定义 `SN_RELEASE`——`sn_alloc` / `sn_free` 不再计数，
  退出时也就不做泄漏检查。默认是 `-O1` 加泄漏检查，测试和对照都用默认的方式。只生成 C 时不接受 `--release`，自己编译时加 `-DSN_RELEASE`。
- 格式化 `sinterc fmt`：按记号格式化，不改变记号和换行的位置，注释、字符串原样保留；按没闭合的括号重新缩进（每层 4 个空格），统一 `,` `:` `=` `==` `=>` `->` 等两边的空格和 `{ }` 里的空格，去掉行尾空白，连续空行合成一个。
  `-w` 写回，`--check` 列出需要格式化的文件（退出码 1）。每次都确认格式化前后记号相同、再格式化一次不变，否则拒绝。`fmtcheck.sh` 检查示例和标准库都已格式化。
- 编辑器：`editor/vscode`（TextMate 语法，含字符串插值）和 `editor/vim`，安装见 `editor/README.md`。`test/editor_syntax.t` 检查两份关键字表和词法分析一致。
- 随机测试（`fuzz/fuzz.ml`）：真实源文件随机改空白（不改“有没有空白”），格式化结果必须和原文件的一样；真实记号随机拼成的行必须满足 `Fmt.check`；
  示例程序随机删、复制、交换、替换一个记号后走完整个编译器，只能报语法、类型、所有权错误，不能抛别的异常。不一致的输入写到 `_batch/fuzz/`；CI 每次用运行编号作种子跑 20 轮。

## 翻译验证

```
sh validate.sh
```

`sinterc 文件.sn --emit=core -o 文件.lean` 把每个函数检查、展开之后的 MIR 翻译成 Lean `FlowGraph.Region` 的 CFG（`emit_core.ml`），
生成的文件调用 `lean/Core/Checking/Validate.lean` 的 `report`，用已证明健全的 `cfgOk` 复核；注解由 Lean 自己推断，编译器的推断不参与。

- `examples/ok/`：正常编译，每个函数都必须被 Lean 接受；
- `examples/bad/`：编译器本来会拒绝的程序，用 `--unchecked` 跳过编译器的所有权检查后导出，Lean 必须独立地拒绝。

导出范围：所有权（含调用约定和排他性）、`linear`、`errdefer` 展开后的出错出口、区域。
不导出的：投影的锁表（通过投影读取算作读源对象）、`Result` 里的引用、errdefer 的登记规则（前端规则，编译器检查）。
按 drop flag 析构导出为空操作，之后再给这个变量赋值时 Lean 会保守地拒绝。

### 三方对照

```
sh diff.sh
```

`lean/Core/Checking/Print.lean` 把 `FlowGraph.Lower` 的 22400 个程序和 `Checking.Suite` 的 19600 个（整体移动、let / inout 调用、闭包读写捕获、泛型实例，共 42000 个）打印成 `_batch/suite.sn`，每个函数名 `p<编号>_<y|n>` 带着 Lean（`acceptsG`）的结论。
`sinterc --batch` 逐个检查这些函数并统计：Lean 接受的 sinterc 必须接受（sinterc 会自动析构 `Box`，所以反过来不要求）；
sinterc 接受的生成 C，每个按所有长度不超过 3 的选择序列（`choose()` 依次返回的真假）各跑一遍，不能泄漏 `Box` 或区域内存。
为此加了测试用的内建函数 `choose()`，以及先不给初值的声明 `var x: T`（读之前必须赋值，由所有权检查保证）。

## 许可证 / License

MIT 或 Apache-2.0，任选其一（[LICENSE-MIT](LICENSE-MIT)、[LICENSE-APACHE](LICENSE-APACHE)）。
除非另有说明，你提交的贡献按同样的双许可发布。

Licensed under either of MIT or Apache-2.0 at your option. Unless you state otherwise, any contribution you
submit is dual licensed in the same way.
