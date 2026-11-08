# 试用 Sinter（0.1.0）

Sinter 是一个简化的 Rust：有所有权、移动、借用和区域（arena），但没有生命周期标注，
编译器检查的核心规则在 Lean 4 里证明过。`sinterc` 把 `.sn` 源文件编译成 C，再交给系统的 C 编译器。

## 安装

需要：OCaml 5.1 以上（用 opam 安装）、dune 3、menhir，以及 GCC 或 Clang。

```sh
opam install dune menhir
cd compiler
dune build
dune install        # 把 sinterc 装进当前 opam 环境；不想安装的话用 dune exec sinterc -- …
sinterc --version   # sinterc 0.1.0
```

## 第一个程序

`examples/tour/tour.sn` 是一个覆盖主要特性的小程序：

```sh
sinterc run examples/tour/tour.sn      # 编译并运行
sinterc build examples/tour/tour.sn    # 生成可执行文件 examples/tour/tour
sinterc examples/tour/tour.sn -o t.c   # 只生成 C
```

C 编译器用环境变量 `CC` 指定，默认是 `cc`。

## 语言一览

- 类型：`I64`、`Bool`、`()`、结构体、枚举、`Box<T>`、`Vec<T>`、`Result<T, E>`、`Option<T>`、引用 `&T in a`。
- 变量：`let` 不可重新赋值，`var` 可以。
- 参数约定：默认 `let`（只读借用，调用后实参还能用）；`inout`（修改调用方的变量或字段）；`sink`（拿走所有权）。
- 所有权：`Box`、`Vec` 和含有它们的结构体会被移动，作用域结束时自动析构；移走之后再用是编译错误。
  可以只移走结构体的某个字段（`var a = q.p.a`），之后再放回（`q.p.a = box(7)`）。
- `linear struct`：必须显式 `consume`，忘了处理就是编译错误；配合 `Result`、`?` 和 `errdefer` 写出错路径。
- 枚举和 `match`：`.Some(x)`、`.None`。
- 泛型和 trait：`fn same<T: Eq>(a: T, b: T) -> Bool`，单态化、静态派发。方法调用语法 `v.push(x)` 等同于 `push(v, x)`。
- 引用只能来自 arena：`arena |a| { let r = alloc(a, 1) … }`，引用不能活过它的 arena。
  函数可以带区域参数：`fn get(o: Option<&I64 in x>) -> I64`。
- 输出：`print("文字")`（字符串字面量，转义有 `\n`、`\t`、`\\`、`\"`）、`print_i64`、`print_bool`。

各个特性更完整的例子在 `test/c*.t`：每个文件开头写了这一步加了什么，后面是能运行的程序和应该被拒绝的程序。

## 已知限制

- 只有整数和布尔值；字符串只能作为 `print` 的实参，还不是一种类型。没有浮点数、闭包、模块；一个程序就是一个文件。
- 错误信息只有中文，一次只报第一个错误。
- `Box`、`Vec` 里的类型参数在实例之间算不变，比 Rust 严：有些 Rust 能写的程序这里会被拒绝。
- 运行时错误（例如下标越界）打印 `panic: …`，退出码 101。
- 生成的 C 需要 GCC 或 Clang（用到了 GNU C 扩展）。

## 反馈

最有用的反馈：
- 你觉得应该通过、却被拒绝的程序（附上源文件和错误信息）；
- 你觉得应该被拒绝、却通过了或者运行时崩溃的程序——这是最严重的问题，请一定告诉我们；
- 看不懂的错误信息。
