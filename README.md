# sinterc：Sinter 编译器（C62，含 C28、C33、T3、T5）

版本 0.2.0-pre（预发布）。上手看 [TRY.md](TRY.md)（[English](TRY.en.md)），改动看 [CHANGELOG.md](CHANGELOG.md)。
证明在单独的仓库 [sinter-formal](https://github.com/clawbolt/sinter-formal)：克隆到本仓库旁边的 `lean` 目录后，
`sh validate.sh`（翻译验证）和 `sh diff.sh`（三方对照）才能运行。

附录 F 的里程碑：C0 解析、类型检查、生成 C；C1a 降级为 MIR 控制流图；C1b 所有权；C2 参数约定、投影与排他性；C3 linear 值与出错路径；C4 区域；C5 翻译验证；C6 枚举；C7 按借用的 match；C8 泛型；C9 trait；C10 递归类型；C11 从 Box 里移出值与拆开结构体；C12 Vec；C13 方法调用语法（UFCS）；C14 部分移动；C15 给字段赋值；C16、C17 部分移动和按 drop flag 析构接入翻译验证；C18 多层部分移动；C19 区域和泛型（泛型函数、Vec 的类型实参可以是引用）；C20 含引用的泛型结构体、枚举；C21 含引用的值接入翻译验证；C22 区域错误的反例由 Lean 独立拒绝；C23 带出区域的值在导出时明确标成已结束的区域；C24 含引用的泛型结构体、枚举的协变；C25 试用版命令行（run、build）和上手文档 TRY.md（英文版 TRY.en.md）；C26 用 print 输出文字；C27 英文错误信息（--lang=en）；C29 字符串类型 Str（Lean M17）；C30 for 循环（a..b 和 Vec）；C31 break 和 continue；C32 读取输入（read_line）和字符串的字节、子串、整数解析；C34 模块（use、pub）；C35 标准库第一版（std.math、std.option、std.result、std.vec、std.str）；C36 文件与参数（args、read_file、write_file）；C37 () 作类型实参；C28 一次报多个错误（每个函数报它的第一个，最多 20 个）；C33 for x in inout v（Lean M18：循环体里改元素本身，不能用 v）；C38 panic（Lean M19），std 的 unwrap、expect；C39 std.map（Lean S4）和 vec_swap（Lean M20）；C40 闭包（Lean M21），std.vec 的 map、filter 等；C41 闭包捕获引用（Lean M22）；C42 std.map 的二分查找（Lean S5）；C43 std.treemap（AVL 树，Lean S6），闭包体里的换行重新分隔语句；C44 对照测试加上移动、调用、闭包、泛型（42000 个程序）；C45 元组；C46 match 的 _ 分支和字面量分支（Lean M23）；T1 报错显示源码行和 ^；C47 浮点数 F64；C48 字符 Char 和 UTF-8（Lean S7），std.char，for 遍历函数返回的 Vec；C49 字符串插值；C50 排序 sort、sort_by（Lean S8）；C51 哈希表 std.hashmap（Lean S9）；C52 常用函数和 std.io；C53 下标 v[i] 和按下标赋值（Lean M18），print_str、print_byte；C54 大整数 std.bigint（Lean S10）；C55 嵌套模式（Lean M24、M24b）；C56 内建的 ToStr、默认方法、泛型 impl（Lean M25）；C57 字节串 Bytes 和二进制文件（Lean S11）；C58 std.os：环境变量、目录、子进程、exit；C59 可以存起来的闭包 Fn(…)（Lean M26）；C60 标准错误输出 eprint 和 --release；C61 字符串切片 &str in a 和静态字面量（Lean M27）；C62 直接读进 arena 的 read_file_in、read_stdin_in、read_chunk；T2 sinterc fmt；T3 编辑器的语法高亮（VS Code、Vim）；T4 CI、文档和 0.2.0-pre；T5 fmt 和编译器的随机测试。
下面是各个里程碑的约定细节；语言的完整一览（包括 `F64`、`Char`、`Str`、`Bytes`、`Vec`、元组、闭包、模块、标准库）见 [TRY.md](TRY.md)。
支持 `I64`、`Bool`、`()`、结构体、`Box<T>`、函数、`let` / `var` / 赋值、`if`（可作表达式）、`while`、`return`，
以及内置的 `print`、`print_i64`、`print_bool`、字符串的 `str_len` / `str_eq` / `str_concat` / `str_push` / `str_clone` / `str_from_i64` / `str_to_i64` / `str_byte` / `str_slice`、`read_line`、`box(e)`、解引用 `*b`、`unbox(b)`（取出 Box 里的值并释放），拆开结构体 `let Node { v, next } = n`。
参数约定 `let`（默认）/ `inout` / `sink`，`let` 绑定投影；`linear struct`、内建的 `Result<T, E>`、`Ok` / `Err`、`?`、`match`、`errdefer`、`consume(x)`。
区域：`arena |a| { … }`、`a: Arena` 参数、`alloc(a, e)`、`&T in a`。
枚举：`enum E { A(T), B }`、`.A(e)` / `.B`、`match e { .A(x) => …, .B => … }`、按借用的 `match let e { … }`。
泛型：`fn f<T, U: linear>(…)`、`struct Pair<A, B> { … }`、`enum E<T> { … }`，类型实参从实参和期望的类型推出来；内置 `Option<T>`。
trait：`trait Show { fn show(x: Self) -> I64 }`、`impl Show for I64 { … }`、约束 `T: Show + Eq + linear`，静态派发。
递归类型：经过 `Box` 的链表、树，例如 `struct Node { v: I64, next: Box<List> }`、`enum List { Cons(Node), Nil }`。
还没有 `inout` 投影、多个载荷的变体、结构体模式、关联类型、trait 对象、显式的类型实参（`f::<T>()`）、区域类型（`Expr<in a>`）、`Mut`、区域里存放引用等（按 Lean 模型逐项加入）。

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
dune test                      # 运行 test/*.t 里的端到端测试
sh fuzz.sh [轮数] [种子]         # 随机测试（T5）：fmt 的空白和随机记号行、编译器对随机改坏的程序不出内部错误
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
  - `std.option`、`std.result` 还有 `unwrap`、`expect`（C38：没有值或出错时 panic）。`std.vec` 还有接收闭包的 `map`、`filter`（`sink v`）、`any`、`all`、`count`、`for_each`、`fold`（C40）。
  - `std.map`（C39，规格 Lean S4）：`Map<K, V>`，键要实现 `math.Ord`，按键从小到大存成两个 Vec（`m.keys`、`m.vals`，只读）。
    `new`、`len`、`index_of`（取值写 `vec_get(m.vals, i)`）、`contains`、`insert`（`inout`，已有键时换值并返回旧值）、`remove`（返回去掉的值）。
    查找用二分查找（C42，Lean S5：和从头往后找的结果相同），是对数时间；插入、删除要挪动元素，是线性时间。
  - `std.treemap`（C43，规格 Lean S6）：`TreeMap<K, V>`，存成 AVL 树，查找、插入、删除都是对数时间。
    `new`、`len`、`height`、`contains`、`insert`、`remove`（和 `std.map` 一样）；值通过闭包拿：`get(m, k, |v| …)` 返回 `Option`，
    `for_each(m, |k, v| …)` 按键从小到大遍历。S6 证明了插入、删除和 S4 一致；S6b 证明了插入、删除保持 AVL 平衡，树高是对数。
- 圆括号里的换行不分隔语句，但花括号里重新分隔（C43）：`f(v, |x| { 语句 \n 语句 })` 里闭包体可以写多行。
- 元组（C45）：`(a, b)` 构造，`t.0` 取元素，`t.0.1 = e` 赋值，`let (x, y) = e` 拆开；类型写成 `(A, B)`，可以嵌套、作返回值和泛型实参（`Vec<(Str, I64)>`）。
  它是内建的泛型结构体 `Tuple#n`（字段 `0`、`1`、…，名字用户写不出来），规则和证明沿用泛型结构体（Lean M8、M13）。元素都可复制时元组可复制，否则只能移动；2 到 6 个元素。
- match 的分支（C46，Lean M23）：`_ => e` 覆盖没写到的变体（最多一个、放在最后、还有没写到的变体才能写；按移动匹配时载荷在分支结束时析构）。
  字面量分支 `0 =>`、`-1 =>`、`true =>`、`"add" =>` 展开成 if 链（字符串用 str_eq），最后必须有 `_`；不能和变体分支混用，模式不能是变量。
- 字节串 `Bytes`（C57）：任意字节，C 里和 `Str` 的表示相同，所有权也一样（需要析构、只能移动，导出给 Lean 时同样是拥有的值，不需要新的所有权证明）。
  `bytes_new`、`len`、`get`、`set`、`push`（字节是 0 到 255 的 I64，超出时 panic）、`append`、`slice`、`clone`、`eq`（`==` 不能比较字节串）；
  `str_to_bytes`（`s.to_bytes()`）、`str_from_utf8`（Lean S11：每一轮解出的字符编码回去要正好是吃掉的字节，合法时才 `.Some`）、`read_bytes`、`write_bytes`、`print_bytes`。
- std.os（C58）：内建函数 `os_env`、`os_list_dir`（不含 `.` 和 `..`，按字节排好）、`os_exists`、`os_is_dir`、`os_make_dir`、`os_remove`、`os_run`（`posix_spawnp`，不经过 shell，返回退出码，被信号结束时是 128 + 信号）、`os_output`（另外用管道收集标准输出，退出码不是 0 时是 Err）、`os_exit`；`std/os.sn` 给它们短的名字。
  和 C36 一样只读外部状态、返回新的值（Lean S3 的形状）；`os_exit` 和 `panic` 一样以 Halt 结尾、不析构（Lean M19），不需要新的证明。
- 浮点数 `F64`（C47）：字面量 `1.5`、`2e-3`；`+ - * /` 和比较要求两边类型相同（不做隐式转换，用 `to_f64`、`to_i64`，后者向零取整，超出范围或 NaN 时 panic）；`%` 只用于 I64。
- 字符 `Char`（C48，Lean S7）：一个 Unicode 标量值，可以复制；字面量 `'a'`、`'中'`、`'😀'`，转义 `'\n'`、`'\t'`、`'\\'`、`'\''`、`'\0'`；按码点比较。`Str` 仍存 UTF-8 字节：`s.chars()` 逐个解码（不合法的字节解成 U+FFFD，只吃一个字节），`s.char_count()` 是字符数，`s.push_char(c)`、`str_from_char(c)` 按 UTF-8 写入；`c.code()`、`char_from(n)`（代理区、超出范围时是 `.None`）在码点和字符间转换。`std.char` 提供 ASCII 的 `is_digit`、`is_alpha`、`to_upper`、`to_digit` 等。`for x in e` 的 `e` 不是变量或字段时（`for c in s.chars()`），先放进隐藏变量再遍历，循环结束后析构。
- 字符串插值（C49）：`"x = {x}, 和 = {a + b}"`，花括号里是任意表达式（可以嵌套字符串），`{{`、`}}` 是花括号本身。值按类型转成字符串（也可以直接写 `x.to_str()`）：I64、F64、Char、Bool、Str 是内建的，别的类型要实现内建的 trait `ToStr`：写 `fn to_str(x: T) -> Str`（按借用传入，不移走；是 `impl ToStr for T` 的简写，C56）。展开成 `{ var #s = …; str_push(#s, …); #s }`，不需要新的推理。C56 起可以给多种类型各写一个 `to_str`，泛型代码里插值要写约束 `T: ToStr`。
- 排序（C50，Lean S8）：`std.vec` 的 `sort(v)`（元素实现 `Ord`）和 `sort_by(v, |a, b| a < b)`（`less` 要是严格弱序），都是 `inout` 原地排序。堆排序，只用 `vec_swap` 交换元素，O(n log n)，不稳定（相等的元素先后可能变）。
- 哈希表 `std.hashmap`（C51，Lean S9）：`HashMap<K, V>`，键实现 `math.Eq` 和 `hashmap.Hash`（I64、Str 已实现）；`new`、`len`、`contains`、`get(m, k, |v| …)`、`insert`（已有键时返回旧值）、`remove`（返回值）、`for_each`（按插入顺序，删除会把最后一个元素换到被删的位置）。键值紧凑地存在两个 Vec 里，另有槽位表存下标；开放寻址、线性探测不绕回（槽位表长 2 倍容量，元素超过 3/4 容量时翻倍）、删除时后移回填，不需要墓碑。平均 O(1)。
- 常用函数（C52）：`std.str` 的 `to_upper`、`to_lower`（ASCII）、`replace`、`lines`（去掉行尾的 `\r`）；`std.vec` 的 `range`、`min`、`sum_f64`、`binary_search`（排好序的 Vec，返回第一个匹配的下标）、`dedup`（原地去掉相邻重复）；`std.math` 的 `gcd`、`lcm`；新模块 `std.io` 的 `read_lines`、`write_lines`、`input_lines`。字符串加了 `\r` 转义。
- 编辑器（T3）：`editor/vscode`（TextMate 语法，含字符串插值）和 `editor/vim`，安装见 `editor/README.md`。`test/t3.t` 检查两份关键字表和词法分析一致。
- 随机测试（T5，`fuzz/fuzz.ml`）：真实源文件随机改空白（不改“有没有空白”），格式化结果必须和原文件的一样；真实记号随机拼成的行必须满足 `Fmt.check`；示例程序随机删、复制、交换、替换一个记号后走完整个编译器，只能报语法、类型、所有权错误，不能抛别的异常。不一致的输入写到 `_batch/fuzz/`；CI 每次用运行编号作种子跑 20 轮。
- 格式化 `sinterc fmt`（T2）：按记号格式化，不改变记号和换行的位置，注释、字符串原样保留；按没闭合的括号重新缩进（每层 4 个空格），统一 `,` `:` `=` `==` `=>` `->` 等两边的空格和 `{ }` 里的空格，去掉行尾空白，连续空行合成一个（和 rustfmt、gofmt 一样不额外加空行）。`-w` 写回，`--check` 列出需要格式化的文件（退出码 1）。每次都确认格式化前后记号相同、再格式化一次不变，否则拒绝。`fmtcheck.sh` 检查示例和标准库都已格式化。
  按 IEEE 754，不检查溢出和除以零。`print_f64` / `str_from_f64` 打印能还原的最短写法（整数值带 `.0`），`str_to_f64` / `s.to_f64()` 解析。`std.math` 有 `sqrt floor ceil round fabs fmin fmax is_nan pi powi`。
- `vec_swap(inout v, i, j)`（也写 `v.swap(i, j)`，C39，Lean M20）：交换两个元素，越界时 panic。
- panic（C38，Lean M19）：`panic(msg)` 打印说明后立刻结束进程（退出码 101），类型是 Never。不析构任何值、不执行 `errdefer`、
  不要求线性值已被消费——线性值的保证只对正常返回和 `Err` 返回成立；需要清理的出错情况用 `Result`。
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
  C56（Lean M25）：trait 里写了函数体的方法是默认方法，impl 可以不写；泛型 impl `impl<T: ToStr> ToStr for Vec<T> { … }`，
  查找时递归地查约束。头部不能是单独的类型参数，impl 的类型参数要出现在头部里，同一个 trait 的两个 impl 不能重叠。
  内建的 trait `ToStr`（`fn to_str(x: Self) -> Str`）：I64、F64、Char、Bool、Str 已经实现，字符串插值用它。
  暂不支持：关联类型、trait 对象、结构体和枚举的类型参数写约束。
- 错误信息格式：`文件:行:列: 错误：…`，下面两行是出错的那一行和标出列的 `^`（T1；中文字符占 2 格，和整个程序有关的错误不显示源码），退出码 1。

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

`lean/Core/C5b.lean` 把 M7（六）的 22400 个程序和 C5c 的第三组 19600 个（整体移动、let / inout 调用、闭包读写捕获、泛型实例，共 42000 个）打印成 `_batch/suite.sn`，每个函数名 `p<编号>_<y|n>` 带着 Lean（`acceptsG`）的结论。
`sinterc --batch` 逐个检查这些函数并统计：Lean 接受的 sinterc 必须接受（sinterc 会自动析构 `Box`，所以反过来不要求）；
sinterc 接受的生成 C，每个按所有长度不超过 3 的选择序列（`choose()` 依次返回的真假）各跑一遍，不能泄漏 `Box` 或区域内存。
为此加了测试用的内建函数 `choose()`，以及先不给初值的声明 `var x: T`（读之前必须赋值，由所有权检查保证）。

## 闭包（C40，Lean M21）

- 参数类型写 `fn(T, inout U) -> R`（返回 `()` 时写 `fn(T)`），实参写闭包 `|x, y| 表达式`，没有参数写 `|| 表达式`：
  `vec.map(v, |x| x * k)`、`vec.for_each(v, |x| { total = total + x })`。
- 闭包只能当实参传：`fn(…)` 只能作函数参数的类型，函数参数只能调用或原样传给另一个 `fn(…)` 参数；不能存进变量、字段、Vec，也不能返回（要存起来用 C59 的 `Fn(…)`）。
- 闭包用到的外层变量是捕获：只读的按 `let`，赋值、作 `inout` 实参的按 `inout`（要求是 `var` 变量）。调用处把捕获当作额外的实参，
  和别的实参一起检查排他性，所以 `vec.for_each(v, |x| { v.push(x) })` 被拒绝。
- 实现是按闭包特化：每个闭包提升成普通函数（捕获在前），`h(a, |x| …)` 换成调用 `h` 的特化 `h[闭包]`。之后的所有权检查、排他性检查和
  Lean 翻译验证看到的都是普通函数。带 `fn(…)` 参数的函数本身只做类型检查，不生成代码。
- 闭包可以用含引用的变量和区域本身（C41，Lean M22）：提升出的函数里这些区域换成区域参数 `'c00`、`'c01`…，按闭包所在位置的嵌套顺序编号，
  只保留那里真实成立的先后关系（外层 arena 比内层的活得久；函数自己的区域参数之间没有顺序）。闭包里同样不能把内层区域的引用放进外层区域的 Vec。
- 暂不支持：闭包里再写闭包；闭包用函数参数；`fn(…)` 里有引用；trait / impl 的方法有 `fn(…)` 参数；`errdefer` 调用函数参数。

## 可以存起来的闭包 Fn(…)（C59，Lean M26）

- 类型写 `Fn(T, inout U) -> R`。闭包写在需要 `Fn(…)` 的地方（`let` / `var` 的类型标注、返回值、实参、字段、`Vec` 元素）时，
  用到的外层变量**按值**拿进来：会移动的值（`Str`、`Vec`、`Box`、`Fn`…）被移走，可复制的值复制一份，存在堆上的环境里。
- `Fn` 值和 `Box` 一样：只能移动，作用域结束时析构（连同环境）；可以存进变量、字段、`Vec`，也可以返回。
- 调用写 `f(x)`（`f` 是变量或参数）或 `f.call(x)`（字段、`Vec` 元素），闭包按 `inout`：闭包可以改自己环境里的变量（计数器），
  所以 `f` 要是 `var` 变量或 `inout` / `sink` 参数。闭包体访问的环境和实参不重叠（M26 的 `env_disjoint`），
  所以闭包照 C40 提升成普通函数检查就够了；`w.g.call(w)` 这种闭包所在的位置又当实参的调用照常被排他性检查拒绝。
- `Fn` 变量可以传给 `fn(…)` 参数（例如 `vec.map(v, f)`）：当作闭包 `|x| f(x)`。
- 环境里不能有引用、区域和 `linear` 值（M26 的 `ok`：存起来的值不指向任何栈帧，返回之后也不会悬空）。
- 实现：C 里是 `sn_fn`（环境指针、转发函数、析构函数）；每个闭包一个环境结构体，每种 `Fn` 类型一个 `call#…`。
  导出给 Lean 时 `Fn` 是需要析构的值，和 `Str` 一样，`validate.sh` 照常检查。

## 标准错误和 --release（C60）

- `eprint(s)` 写到标准错误并换行，`eprint_str(s)` 不换行。写之前先刷新标准输出，所以两者合到一处（`2>&1`）时顺序和程序里一样。
- `sinterc build --release`、`sinterc run --release`：用 `-O2` 编译，并定义 `SN_RELEASE`——`sn_alloc` / `sn_free` 不再计数，
  退出时也就不做泄漏检查。默认仍是 `-O1` 加泄漏检查（有泄漏时退出码 102），测试和对照都用默认的方式。
  只生成 C 时不接受 `--release`，自己编译时加 `-DSN_RELEASE`。
- 不涉及新的推理，没有新的 Lean 证明：语言的语义不变，只是去掉了一个运行时的自检。

## 字符串切片 &str in a 和静态字面量（C61，Lean M27）

- **静态字面量**：`Str` 多了一个标记 `lit`。字面量指向程序里的字符串常量，不分配；复制字面量还是字面量；析构什么都不做；
  修改（`push`、`push_char`、`Bytes` 的 `set`、`push`）前 `sn_str_own` 先复制到堆上（M27 的三：分配计数总等于拥有的堆字符串个数）。
  命令行参数、环境变量、目录项不归程序所有，仍然复制成堆上的 `Str`。
- **切片**：类型 `&str in a`，在编译器里就是区域 `a` 里的一种引用（`TRef (TView, a)`），所以“切片活不过缓冲区”
  直接由已有的区域检查保证，导出给 Lean 时也按引用复核（`validate.sh` 检查 `c61_view.sn`）。
  不引入“借用某个变量”：缓冲区由 `text(a, s)` 交给区域 `a`（字面量直接指向静态存储，不复制），之后不再改动，区域结束时整块释放。
- 切片的方法：`len`、`byte(i)`、`sub(i, j)`（越界时 panic，结果在同一块缓冲区里，M27 的一）、`find(p, from)`、`eq(s)`、`eq_view(w)`、
  `print_str()`，都不分配；`to_str()` 复制成 `Str`。`std.view`：`split`、`lines`、`trim`、`starts_with`、`ends_with`、`to_i64`。
- C 里切片是 `sn_view { p, len }`，按值传。字符串的 `lit` 和 `len` 共用 8 字节（位域，`len` 63 位），`sn_str` 仍是 16 字节；最初加成单独的 `bool` 时是 24 字节，全量的 `Vec<Vec<Str>>` 因此多用了约 30 MB，已改回。
- 暂不支持：对切片做字符串插值、`match` 切片。（直接读进 arena 见 C62。）

## 直接读进 arena（C62）

- `read_file_in(a, path) -> Result<&str in a, Str>`：普通文件按 `fstat` 的大小在区域里分配一块缓冲区，大小没变时只读一次、不复制。
- `read_stdin_in(a) -> &str in a`：读完标准输入，任意字节（包括 0）。
- `read_chunk(a, n) -> &str in a`：从标准输入最多读 `n` 个字节（`n ≤ 0` 时 panic），到头时是空的切片。
  每次都在区域里新分配一块，区域结束时一起释放；循环里读大文件时，把每一块的处理放进内层的 `arena` 块，内存就不会一直涨。
- 都只是在区域后面追加新的缓冲区，就是 Lean M27 的二已经证明的情况（`fresh_valid`、`grow_read`），没有新的推理。
- 同时 `std.str` 的 `find`、`split`、`replace`、`starts_with`、`ends_with` 改为逐字节比较，不再为每个位置复制一个子串。
- `std.view` 的 `line_end(v, pos)`、`trim_cr(v)`、`field(v, sep, i)`：逐行、逐字段访问，不建 `Vec`。
  `c62.t` 检查它们和 `lines`、`split` 的结果相同（空行、`\r\n`、空字段、末尾没有换行）。
- `benchmark/` 的 `sh run.sh csv`：100 万行的 CSV 对一列求和，`Str` 版 0.13s、68.4 MB，切片收集成 `Vec` 0.04s、37.6 MB，`line_end` + `field` 不建 `Vec` 0.02s、20.2 MB（结果相同）。


## 许可证 / License

MIT 或 Apache-2.0，任选其一（[LICENSE-MIT](LICENSE-MIT)、[LICENSE-APACHE](LICENSE-APACHE)）。
除非另有说明，你提交的贡献按同样的双许可发布。

Licensed under either of MIT or Apache-2.0 at your option. Unless you state otherwise, any contribution you
submit is dual licensed in the same way.
