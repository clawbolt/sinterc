# sinterc：Sinter 编译器（C37）

附录 F 的里程碑：C0 解析、类型检查、生成 C；C1a 降级为 MIR 控制流图；C1b 所有权；C2 参数约定、投影与排他性；C3 linear 值与出错路径；C4 区域；C5 翻译验证；C6 枚举；C7 按借用的 match；C8 泛型；C9 trait；C10 递归类型；C11 从 Box 里移出值与拆开结构体；C12 Vec；C13 方法调用语法（UFCS）；C14 部分移动；C15 给字段赋值；C16、C17 部分移动和按 drop flag 析构接入翻译验证；C18 多层部分移动；C19 区域和泛型（泛型函数、Vec 的类型实参可以是引用）；C20 含引用的泛型结构体、枚举；C21 含引用的值接入翻译验证；C22 区域错误的反例由 Lean 独立拒绝；C23 带出区域的值在导出时明确标成已结束的区域；C24 含引用的泛型结构体、枚举的协变；C25 试用版命令行（run、build）和上手文档 TRY.md（英文版 TRY.en.md）；C26 用 print 输出文字；C27 英文错误信息（--lang=en）；C29 字符串类型 Str（Lean M17）；C30 for 循环（a..b 和 Vec）；C31 break 和 continue；C32 读取输入（read_line）和字符串的字节、子串、整数解析；C34 模块（use、pub）；C35 标准库第一版（std.math、std.option、std.result、std.vec、std.str）；C36 文件与参数（args、read_file、write_file）；C37 () 作类型实参。
支持 `I64`、`Bool`、`()`、结构体、`Box<T>`、函数、`let` / `var` / 赋值、`if`（可作表达式）、`while`、`return`，
以及内置的 `print`、`print_i64`、`print_bool`、字符串的 `str_len` / `str_eq` / `str_concat` / `str_push` / `str_clone` / `str_from_i64` / `str_to_i64` / `str_byte` / `str_slice`、`read_line`、`box(e)`、解引用 `*b`、`unbox(b)`（取出 Box 里的值并释放），拆开结构体 `let Node { v, next } = n`。
参数约定 `let`（默认）/ `inout` / `sink`，`let` 绑定投影；`linear struct`、内建的 `Result<T, E>`、`Ok` / `Err`、`?`、`match`、`errdefer`、`consume(x)`。
区域：`arena |a| { … }`、`a: Arena` 参数、`alloc(a, e)`、`&T in a`。
枚举：`enum E { A(T), B }`、`.A(e)` / `.B`、`match e { .A(x) => …, .B => … }`、按借用的 `match let e { … }`。
泛型：`fn f<T, U: linear>(…)`、`struct Pair<A, B> { … }`、`enum E<T> { … }`，类型实参从实参和期望的类型推出来；内置 `Option<T>`。
trait：`trait Show { fn show(x: Self) -> I64 }`、`impl Show for I64 { … }`、约束 `T: Show + Eq + linear`，静态派发。
递归类型：经过 `Box` 的链表、树，例如 `struct Node { v: I64, next: Box<List> }`、`enum List { Cons(Node), Nil }`。
还没有 `inout` 投影、多个载荷的变体、嵌套模式、关联类型、默认方法、trait 对象、泛型 impl、显式的类型实参（`f::<T>()`）、区域类型（`Expr<in a>`）、`Mut`、区域里存放引用等（按 Lean 模型逐项加入）。

## 依赖

OCaml 5.3、dune ≥ 3.0、menhir，以及系统的 C 编译器（`cc`，需要支持 `__builtin_add_overflow` 一类内建函数和语句表达式，GCC 和 Clang 都支持）。

```
opam install dune menhir
```

## 构建与测试

```
cd compiler
dune build
dune test                      # 运行 test/*.t 里的端到端测试
dune exec sinterc -- 文件.sn -o 文件.c
cc -std=gnu11 文件.c -o 文件 && ./文件
```

## 约定

- 模块（C34）：一个文件一个模块。文件开头写 `use geom` 引入同目录的 `geom.sn`，之后用 `geom.area(…)`、`geom.Point`、`T: geom.Show` 访问它标了 `pub` 的函数、结构体、枚举、trait；
  `use geom { area, Point }` 另外把这几个名字直接引入。没标 `pub` 的只能在本文件里用；`impl` 不写 `pub`，到处可见；trait 的方法名不加模块前缀。
  `consume(x)` 只能在定义这个 `linear` 类型的文件里用。变量不能和引入的模块同名。模块里的错误报在模块的文件里。
- 标准库（C35）：`use std.math` 引入编进编译器的 `std/math.sn`，之后照样写 `math.min(…)`。标准库用 Sinter 写，和用户代码一样检查；
  同一个程序里不能既有自己的 `math.sn` 又用 `std.math`。现有模块：
  - `std.math`：`min`、`max`、`abs`、`sign`、`clamp`、`pow`；trait `Eq`（`eq`）、`Ord`（`cmp`，返回 -1/0/1），I64、Bool、Str 实现了 `Eq`，I64、Str 实现了 `Ord`；`min_of`、`max_of`（任意 `Ord`）。
  - `std.option`：`is_some`、`is_none`、`unwrap_or`。`std.result`：`is_ok`、`is_err`、`unwrap_or`、`ok`。
  - `std.vec`：`sum`、`max`、`contains`、`index_of`（元素要实现 `math.Eq`）、`reverse`（`inout`）。
  - `std.str`（按字节）：`starts_with`、`ends_with`、`find`、`contains`、`trim`、`split`、`join`、`repeat`。
  - 暂缺：没有 `unwrap`（用户不能主动 panic）、没有 `map` / `filter`（没有闭包）。
- `()`（C37，Lean M9u）：只有一个值的 copy 类型，可以作泛型函数、`Vec`、`Option` 等的类型实参，也可以是变量、参数、字段、`Box` 里的值；
  `Result<(), E>` 的 `Ok` 有一个 `()` 载荷，可以省略不写（`Ok` 就是 `Ok(())`，分支可以写 `Ok =>`）。还不能引用 `()`，区域里也不能放 `()`。
- 文件与参数（C36，Lean S3）：`args()` 是命令行参数（不含程序名，`sinterc run 源文件.sn 参数…` 把后面的参数交给程序）；
  `read_file(path)` 返回 `Result<Str, Str>`；`write_file(path, s)` 返回 `Result<(), Str>`，先写 `path.sinter-tmp` 再 `rename`，
  失败时原文件不变、不留临时文件。出错说明是“路径：系统的说明”。
- 语句之间用换行分隔（也可以用 `;`），圆括号里的换行忽略；块的最后一条若是表达式，就是块的值。
- 只有 `var` 声明的变量和 `inout` / `sink` 参数能重新赋值；`let` 声明的变量和 `let` 参数不能。
- 参数约定：`let`（不写时的默认）只读借用，`inout` 可变借用（实参必须是可修改的位置），`sink` 转移所有权（可复制的类型按值复制）。
  `let` / `inout` 参数在 C 里是指针，不拥有值，不能移走；给 `inout` 参数整体赋值会先析构旧值。
- 排他性：同一次调用里同一个变量出现两次，两次都必须是只读的（`let` 或复制）。
- 投影：`let a = p.a` 这类绑定（值需要析构、又不能整个移走）不拷贝，`a` 指向 `p` 内部。
  `a` 之后还会用到时，`p` 不能被移动、修改、以 `inout` 传入或析构；锁在 `a` 最后一次使用之后结束。锁住的是整个 `p`。
- `Box<T>`、`Vec<T>` 和含它们的结构体需要析构，只能移动、不能复制；`I64`、`Bool` 和只含它们的结构体按值复制。
  变量结构体可以直接移走单个字段；不能直接通过 `Box`、`Vec` 或多层字段移出，改用 `unbox`、`vec_pop` 或先拆开结构体。
- `a.f = e` 要求 `a` 以可修改的 `var` / `inout` / `sink` 位置为根；旧字段需要析构时先析构，`linear` 字段不能直接赋值。
- 整数运算检查溢出和除以零，出错时打印 `panic: …` 并以退出码 101 结束。
- 原型阶段统计堆上的 `Box`：程序结束时还有没释放的，打印 `leak: …` 并以退出码 102 结束。
- 函数参数、结构体字段一律从左到右求值（降级到 MIR 时操作数依次存进临时变量，不依赖 C 的求值顺序）。
- `linear` 值必须被显式消费：移动走（`sink` 实参、返回）或 `consume(x)`（析构它的字段；等有了模块，只允许在类型自己的方法里用）。
  作用域结束、`return`、出错返回三种出口上还没被消费都报错；重新赋值前也必须先消费。
- `Result<T, E>` 是变体为 `Ok(T)`、`Err(E)` 的枚举（还没有泛型，编译器为每个用到的 `Result<T, E>` 生成一个具体的枚举），
  构造、`match`（按移动或按借用）、析构都和普通枚举相同；`()` 载荷记作没有载荷，构造写 `Ok(())`，分支写 `Ok(_)`。
  `Ok(e)` / `Err(e)` 的类型由上下文确定：`let` 的类型标注、返回值、实参、字段。
- `e?`：出错时把错误值包成函数返回的 `Err`，执行生效的 `errdefer`，然后返回；错误类型必须和函数返回的相同。
- `errdefer f(…)`：只在出错返回（`?` 的出错分支和 `return Err(…)`）时执行，内层的先执行，作用到所在块结束。
  调用必须以 `sink` 方式传入恰好一个 `linear` 变量，实参只能是变量、字段或常量。
  登记时这个变量必须还没被消费，并且不能已经有生效的 `errdefer`。
- 区域（Lean M5、M7（四））：`alloc(a, e)` 得到只读引用 `&T in a`，块结束时整块释放，`return`、`?` 穿过区域块时也释放。
  带区域 `a` 的值不能作为块的值带出 `a`（R3）；外层区域的引用可以当作内层的用，反过来不行（D2）；
  函数的区域参数（`Arena` 参数和参数类型里出现的区域名）之间没有顺序，调用时按实参实例化（D3）；
  需要析构或 `linear` 的值不能放进区域（D4）。区域不能存进变量、字段或返回。
- 枚举（Lean M8（一））：每个变体至多一个载荷（`A(T)`，也可以写 `A(x: T)`，名字只作说明），载荷不能是 `()`、`linear` 值或引用。
  `.A(e)` / `.B` 的类型和 `Ok` / `Err` 一样由上下文确定。`match` 必须穷尽，每个变体恰好一个分支；
  按移动拆开：带载荷的分支把载荷移进分支变量（写 `_` 也会移走，在分支结束时析构），枚举本身变成已移走。
  各分支结束时状态不同的变量按 drop flag 析构；没被 `match` 的枚举在作用域结束时按运行时标签析构载荷。
  载荷都可复制的枚举整体按值复制（和结构体相同，`copy enum` 的显式标注之后再做）。
- 按借用的 `match`（Lean M8（二））：`match let x { … }` 显式借用；被匹配的位置不能整个移走时
  （`let` / `inout` 参数、字段、解引用）自动借用，和 `let` 绑定投影的规则相同。枚举本身不动，`match` 之后照常可用。
  需要析构的载荷绑定成投影（不拥有，不能移走）；投影之后还会用到时，被匹配的值不能被移动、修改、以 `inout` 传入或析构。
  可复制的载荷直接复制。
- 泛型（Lean M9（一））：类型参数写在名字后面，`fn f<T, U: linear>(…)`、`struct Pair<A, B>`、`enum Option<T>`；
  用到时写 `Option<I64>`。函数的类型实参从实参类型和期望的返回类型推出来，结构体字面量的从期望的类型或字段的值推出来。
  定义处按抽象类型检查：`T` 的值只能移动（即使之后只用 `I64` 实例化）；普通的 `T` 在作用域结束时自动析构，
  `T: linear` 必须交出去（`sink` 实参或返回，泛型代码里不能 `consume`）；不能对 `T` 做运算、比较、取字段或 `match`（要等 trait）。
  调用处检查约束：普通的 `T` 不能是 `linear` 类型，`T: linear` 什么都可以。
  单态化：用到的每个实例（`f<I64>`、`Option<Box<I64>>`）生成一份具体的代码，照常做所有权检查；泛型函数本身也检查，但不输出。
  类型实参可以是引用（C19、C20），不能是 `()` 或 `Arena`；泛型结构体、枚举的类型实参暂不能是 `linear` 值；泛型函数的签名里不能直接写区域。
- trait（Lean M10（一））：`trait Eq { fn eq(a: Self, b: Self) -> Bool }` 只写方法签名，参数约定照常写；
  `impl Eq for I64 { fn eq(a: I64, b: I64) -> Bool { … } }`（方法里也可以写 `Self`），`impl` 的方法要和 trait 一一对应，
  参数约定、参数类型和返回类型相同（`Self` 换成实现的类型）；同一个类型对同一个 trait 只能有一个 `impl`。
  方法可以写成 `eq(x, y)` 或 `x.eq(y)`，接收者就是第一个实参；`Self` 从实参类型推出来，方法名全局唯一（暂不支持重载）。
  约束写 `fn f<T: Eq + Show + linear>(…)`：泛型代码里只能调用约束里写了的 trait 的方法；调用处每个约束都要有 `impl`
  （类型实参本身是类型参数时，要在当前函数里写了同样的约束）。单态化时方法调用直接换成那个 `impl` 里的函数，没有虚表。
  暂不支持：关联类型、默认方法、trait 对象、泛型 `impl`（`impl<T> Show for Option<T>`）、结构体和枚举的类型参数写约束。
- 错误信息格式：`文件:行:列: 错误：…`，退出码 1。

## 编译流程

解析（`lexer.mll`、`parser.mly`）→ 类型检查（`typeck.ml`）→ 降级为 MIR 控制流图（`mir.ml`，作用域结束和 `return` 前插入 `Drop`）
→ 所有权检查与析构展开（`own.ml`）→ 投影锁定与排他性检查（`borrow.ml`）→ 生成 C（`emit_c.ml`，每个基本块一个标签，块之间用 `goto`）。

`own.ml` 是 Lean M2 / M7（五）规则的实现：每个局部变量处于“已初始化 / 未初始化 / 可能已初始化”之一，
前向数据流、分支汇合取并、循环迭代到不动点。读取不是“已初始化”的变量报错；
`Drop` 和给 `var` 重新赋值前的析构，按状态变成直接析构、去掉，或者按 drop flag 析构。

`borrow.ml` 对应 Lean M3（二）和 M4（一）–（四）：后向数据流算出每个程序点之后还会用到的投影，
投影活着时检查它的源对象有没有被移动、修改、以 `inout` 传入或析构；调用时再两两检查实参的排他性。

## 翻译验证（C5）

```
sh validate.sh
```

`sinterc 文件.sn --emit=core -o 文件.lean` 把每个函数检查、展开之后的 MIR 翻译成 Lean M7（五·下）的 CFG（`emit_core.ml`），
生成的文件调用 `lean/Core/C5.lean` 的 `report`，用已证明健全的 `cfgOk` 复核；注解由 Lean 自己推断，编译器的推断不参与。

- `examples/ok/`：正常编译，每个函数都必须被 Lean 接受；
- `examples/bad/`：编译器本来会拒绝的程序，用 `--unchecked` 跳过编译器的所有权检查后导出，Lean 必须独立地拒绝。

导出范围：所有权（含调用约定和排他性）、`linear`、`errdefer` 展开后的出错出口、区域。
不导出的：投影的锁表（通过投影读取算作读源对象）、`Result` 里的引用、errdefer 的登记规则（前端规则，编译器检查）。
按 drop flag 析构导出为空操作，之后再给这个变量赋值时 Lean 会保守地拒绝。

### 三方对照（C5 后半）

```
sh diff.sh
```

`lean/Core/C5b.lean` 把 M7（六）的 22400 个程序打印成 `_batch/suite.sn`，每个函数名 `p<编号>_<y|n>` 带着 Lean（`acceptsG`）的结论。
`sinterc --batch` 逐个检查这些函数并统计：Lean 接受的 sinterc 必须接受（sinterc 会自动析构 `Box`，所以反过来不要求）；
sinterc 接受的生成 C，每个按所有长度不超过 3 的选择序列（`choose()` 依次返回的真假）各跑一遍，不能泄漏 `Box` 或区域内存。
为此加了测试用的内建函数 `choose()`，以及先不给初值的声明 `var x: T`（读之前必须赋值，由所有权检查保证）。
