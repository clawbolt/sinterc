(* 错误信息的语言。编译器各处照常用中文报错；--lang=en（或环境变量 SINTER_LANG=en）时，
   在输出之前按下面的表把整条信息换成英文。表里的中文和代码里的格式串一样，%s、%d 是填进去的部分，
   英文里用 {1}、{2}… 按顺序引用它们。表里没有的信息原样输出中文。
   填进去的部分如果是固定的中文短语（例如“作用域结束时”“临时值”），也换成英文（见 words）。 *)

type lang = Zh | En

let lang = ref Zh

let pick zh en = match !lang with Zh -> zh | En -> en

(* 按信息的写法排列：更具体的写在前面（例如“在某些路径上”要排在一般的“还没有被消费”之前）。 *)
let table =
  [
    ("inout 实参必须是变量、字段或解引用这样的位置", "an inout argument must be a place: a variable, field, or dereference");
    ("%s 不可修改，不能作为 inout 实参（需要 var 变量或 inout / sink 参数）",
     "{1} is not mutable and cannot be passed as inout (it needs to be a var variable or an inout / sink parameter)");
    ("%s的两个分支类型不同：%s 和 %s", "the two branches of {1} have different types: {2} and {3}");
    ("类型不匹配：需要 %s，实际是 %s", "type mismatch: expected {1}, found {2}");
    ("未知类型 %s", "unknown type {1}");
    ("Box 里不能放 ()", "a Box cannot hold ()");
    ("Box 里不能放 linear 值（还不能从 Box 里移出）", "a Box cannot hold a linear value");
    ("Arena 只能作为函数参数的类型", "Arena can only be the type of a function parameter");
    ("未知区域 %s", "unknown region {1}");
    ("还不支持指向引用或区域的引用", "references to references or regions are not supported yet");
    ("不能引用 ()", "cannot take a reference to ()");
    ("类型实参不能是 ()", "a type argument cannot be ()");
    ("类型实参不能是 Arena", "a type argument cannot be Arena");
    ("泛型结构体、枚举的类型实参暂不能是 linear 值（%s）",
     "type arguments of generic structs and enums cannot be linear yet ({1})");
    ("Vec 要写元素类型：Vec<…>", "Vec needs an element type: Vec<…>");
    ("%s 是泛型类型，要写类型实参：%s<…>", "{1} is generic and needs type arguments: {2}<…>");
    ("Vec 需要 1 个类型实参，实际给了 %d 个", "Vec takes 1 type argument, {1} given");
    ("%s 不是泛型类型，不能写类型实参", "{1} is not generic and takes no type arguments");
    ("%s 需要 %d 个类型实参，实际给了 %d 个", "{1} takes {2} type arguments, {3} given");
    ("泛型类型的实例超过 1000 个（可能是类型实参越来越大的递归类型，例如 W<T> 里有 Box<W<Box<T>>>）",
     "more than 1000 instances of generic types (perhaps a recursive type whose type arguments keep growing, e.g. W<T> containing Box<W<Box<T>>>)");
    ("这里需要 %s，%s 是枚举的变体", "expected {1} here, but {2} is an enum variant");
    ("%s 没有变体 %s", "{1} has no variant {2}");
    ("%s 带一个 %s 类型的载荷，要写 %s(…)", "{1} carries a payload of type {2}; write {3}(…)");
    ("%s 没有载荷，不能写 %s(…)", "{1} has no payload; cannot write {2}(…)");
    ("这里需要 %s，vec_new() 是 Vec", "expected {1} here, but vec_new() is a Vec");
    ("未定义的变量 %s", "undefined variable {1}");
    ("字符串用 a.eq(b) 比较", "compare strings with a.eq(b)");
    ("for 只能遍历变量或字段里的 Vec，或者写成 a..b", "for can only iterate over a Vec in a variable or field, or a range a..b");
    ("%s 只能用在循环里", "{1} can only be used inside a loop");
    ("只能比较 I64、F64 和 Bool", "only I64, F64 and Bool values can be compared");
    ("consume 只能用于 linear 结构体，%s 不是", "consume only works on linear structs; {1} is not one");
    ("unbox 的参数必须是 Box，%s 不是", "unbox needs a Box; {1} is not one");
    ("%s 需要 1 个参数", "{1} takes 1 argument");
    ("区域里不能放 ()", "cannot allocate () in a region");
    ("区域里不能放需要析构或 linear 的值 %s：区域结束时不运行析构（D4）",
     "cannot allocate {1} in a region: it needs dropping or is linear, and regions do not run destructors");
    ("还不支持在区域里存放引用", "storing references in a region is not supported yet");
    ("alloc 的第一个参数必须是区域，%s 不是", "the first argument of alloc must be a region; {1} is not");
    ("alloc 需要 2 个参数：区域和值", "alloc takes 2 arguments: a region and a value");
    ("无法确定 vec_new() 的元素类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方",
     "cannot infer the element type of vec_new(): use it where the type is known (a let annotation, return value, argument, or field)");
    ("%s 需要 %d 个参数，实际给了 %d 个", "{1} takes {2} arguments, {3} given");
    ("%s 需要 %d 个参数", "{1} takes {2} arguments");
    ("%s 的第一个参数必须是 Vec，%s 不是", "the first argument of {1} must be a Vec; {2} is not");
    ("%s 的参数个数不对", "wrong number of arguments to {1}");
    ("%s 不是 Box 或引用，不能解引用", "{1} is not a Box or reference and cannot be dereferenced");
    ("%s 是枚举，不能取字段 %s（用 match）", "{1} is an enum; cannot access field {2} (use match)");
    ("%s 没有字段 %s", "{1} has no field {2}");
    ("%s 不是结构体，不能取字段 %s", "{1} is not a struct; cannot access field {2}");
    ("无法确定 %s 的类型：请写在 let 标注、返回值、实参或字段这些知道类型的地方",
     "cannot infer the type of {1}: use it where the type is known (a let annotation, return value, argument, or field)");
    ("? 传出的错误类型是 %s，但函数返回的错误类型是 %s",
     "? propagates error type {1}, but the function returns error type {2}");
    ("? 只能用在返回 Result 的函数里（这个函数返回 %s）",
     "? can only be used in a function that returns Result (this one returns {1})");
    ("%s 不是 Result，不能用 ?", "{1} is not a Result; cannot use ?");
    ("未定义的函数 %s", "undefined function {1}");
    ("无法推断 %s 的类型参数 %s（可以给返回值或实参写类型标注）",
     "cannot infer type parameter {2} of {1} (add a type annotation on the result or an argument)");
    ("%s 的类型参数 %s 没有标 linear，不能是 linear 类型 %s（要写 %s: linear）",
     "type parameter {2} of {1} is not marked linear, so it cannot be the linear type {3} (write {4}: linear)");
    ("%s 没有写约束 %s，不能调用 %s（要写 %s: %s）", "{1} has no bound {2}, so {3} cannot be called (write {4}: {5})");
    ("%s 没有实现 %s，不能调用 %s", "{1} does not implement {2}; cannot call {3}");
    ("%s 要求 %s: %s，但 %s 没有写约束 %s", "{1} requires {2}: {3}, but {4} has no bound {5}");
    ("%s 没有实现 %s（%s 要求 %s: %s）", "{1} does not implement {2} ({3} requires {4}: {5})");
    ("%s 的类型参数 %s 只出现在返回类型里，不能是含引用的类型 %s（区域要从实参得到）",
     "type parameter {2} of {1} appears only in the return type, so it cannot be {3}, which contains references (regions must come from arguments)");
    ("inout 实参的类型要和参数类型完全相同：需要 %s，实际是 %s（区域不能放宽）",
     "an inout argument must have exactly the parameter's type: expected {1}, found {2} (regions cannot be widened)");
    ("未知结构体 %s", "unknown struct {1}");
    ("无法推断 %s 的类型参数 %s（可以写类型标注，例如 let x: %s<…> = …）",
     "cannot infer type parameter {2} of {1} (add a type annotation, e.g. let x: {3}<…> = …)");
    ("%s 缺少字段 %s", "{1} is missing field {2}");
    ("字段 %s 重复", "field {1} is given twice");
    ("区域 %s 已经存在（内外层区域不能同名）", "region {1} already exists (nested regions need different names)");
    ("块的值的类型 %s 含有区域 %s，不能带出这个区域（R3）",
     "the value of this block has type {1}, which mentions region {2}, so it cannot leave that region");
    ("不能 match 一个不会返回的值", "cannot match on a value that never returns");
    ("%s 不是 Result 或枚举，不能 match", "{1} is not a Result or enum; cannot match on it");
    ("match let 后面必须是变量、字段或解引用（要借用的位置）",
     "match let needs a place to borrow: a variable, field, or dereference");
    ("%s 带载荷，分支要写 %s(x) 或 %s(_)", "{1} carries a payload; write {2}(x) or {3}(_)");
    ("%s 的值是 ()，分支要写 %s(_)", "the payload of {1} is (); write {2}(_)");
    ("%s 没有载荷，分支要写 %s => …", "{1} has no payload; write {2} => …");
    ("match 没有覆盖 %s（枚举的 match 必须穷尽）", "match does not cover {1} (a match on an enum must be exhaustive)");
    ("match 有两个 %s 分支", "match has two {1} arms");
    ("_ 分支只能放在最后", "the _ arm must be the last one");
    ("所有变体都已经写到了，_ 分支是多余的", "all variants are already covered; the _ arm is redundant");
    ("分支 %s 写了两次", "the arm {1} appears twice");
    ("字面量分支和变体分支不能写在同一个 match 里", "literal arms and variant arms cannot be mixed in one match");
    ("按字面量 match 时最后要有 _ 分支（字面量写不全）", "a match on literals needs a final _ arm (literals cannot be exhaustive)");
    ("分支的模式不能是变量 %s（要匹配所有其余情况时写 _）", "an arm pattern cannot be the variable {1} (write _ to match everything else)");
    ("空枚举不能 match", "cannot match on an empty enum");
    ("区域不能存进变量", "a region cannot be stored in a variable");
    ("%s 的初值不会返回，无法推断类型", "the initializer of {1} never returns, so its type cannot be inferred");
    ("%s 是用 let 声明的变量或参数，不能重新赋值（需要的话改用 var / inout）",
     "{1} is a let variable or parameter and cannot be reassigned (use var / inout if needed)");
    ("不能通过引用修改它指向的值", "cannot modify a value through a reference");
    ("只能给变量（或它里面的位置）的字段赋值", "can only assign to a field of a variable (or of a place inside one)");
    ("%s 是枚举，不能给字段 %s 赋值", "{1} is an enum; cannot assign to field {2}");
    ("%s 不是结构体，不能给字段 %s 赋值", "{1} is not a struct; cannot assign to field {2}");
    ("不能给一个不会返回的值的字段赋值", "cannot assign to a field of a value that never returns");
    ("%s 不可修改，不能给它的字段赋值（需要 var 变量或 inout / sink 参数）",
     "{1} is not mutable; cannot assign to its fields (it needs to be a var variable or an inout / sink parameter)");
    ("字段 %s 是 linear 值，不能直接赋值（旧值没有被消费）",
     "field {1} is linear and cannot be assigned directly (the old value would not be consumed)");
    ("这里要拆开的是结构体 %s，实际是 %s", "expected struct {1} to destructure, found {2}");
    ("这里要拆开的是 %s 个元素的元组，实际是 %s", "expected a tuple of {1} elements to destructure, found {2}");
    ("元组最多 %d 个元素（这里有 %d 个）", "a tuple has at most {1} elements (found {2})");
    ("不能拆开一个不会返回的值", "cannot destructure a value that never returns");
    ("linear 结构体 %s 不能拆开（要用 consume）", "linear struct {1} cannot be destructured (use consume)");
    ("拆开 %s 时缺少字段 %s（每个字段都要写）", "destructuring {1} is missing field {2} (every field must be listed)");
    ("%s 已经有一个生效的 errdefer，不能再登记", "{1} already has an active errdefer");
    ("errdefer 的实参只能是变量、字段或常量", "errdefer arguments can only be variables, fields, or constants");
    ("errdefer 的调用必须以 sink 方式传入恰好一个 linear 变量（出错时由它消费）",
     "an errdefer call must pass exactly one linear variable as a sink argument (it consumes the value on error)");
    ("errdefer 后面必须是一个函数调用", "errdefer must be followed by a function call");
    ("类型参数 %s 重复", "type parameter {1} is declared twice");
    ("%s的类型参数 %s 不能标 linear（只有函数的类型参数可以）",
     "type parameter {2} of a {1} cannot be marked linear (only function type parameters can)");
    ("类型 %s 重复定义", "type {1} is defined twice");
    ("枚举 %s 至少要有一个变体", "enum {1} needs at least one variant");
    ("枚举 %s 的变体 %s 重复", "enum {1} has variant {2} twice");
    ("函数 %s 重复定义", "function {1} is defined twice");
    ("trait %s 重复定义", "trait {1} is defined twice");
    ("trait %s 的方法 %s 重复", "trait {1} declares method {2} twice");
    ("类型 %s 不经过 Box 包含自己，大小是无限的（递归要经过 Box，例如 next: Box<List>）",
     "type {1} contains itself without a Box, so its size is infinite (recursion must go through Box, e.g. next: Box<List>)");
    ("类型参数 %s 和已有的类型同名", "type parameter {1} has the same name as an existing type");
    ("Self 不能用作类型参数的名字", "Self cannot be used as a type parameter name");
    ("未知 trait %s", "unknown trait {1}");
    ("泛型函数的签名里暂不能直接写区域或引用（类型实参可以是引用）",
     "a generic function's signature cannot mention regions or references directly yet (type arguments can be references)");
    ("trait 的方法 %s 至少要有一个参数（接收者）", "the trait method {1} needs at least one parameter (the receiver)");
    ("%s 的方法 %s 不确定：trait %s 和 %s 都有这个方法，%s 两个都满足",
     "the method {2} of {1} is ambiguous: traits {3} and {4} both have it, and {5} satisfies both");
    ("%s 是 trait %s 的方法，要写成方法调用 x.%s(…)", "{1} is a method of trait {2}; call it as a method: x.{3}(…)");
    ("trait 的方法暂不支持区域参数和引用", "trait methods cannot take regions or references yet");
    ("暂不支持为含引用的类型 %s 实现 trait", "implementing a trait for {1}, which contains references, is not supported yet");
    ("不能为 Arena 实现 trait", "cannot implement a trait for Arena");
    ("%s 已经实现过 %s", "{1} already implements {2}");
    ("impl 里的方法暂不能有类型参数", "methods in an impl cannot have type parameters yet");
    ("结构体字段 %s 不能含引用或区域（区域类型还没有实现）",
     "struct field {1} cannot contain references or regions directly (make the struct generic and pass the reference type as a type argument)");
    ("%s 的字段 %s 是 linear 的，%s 也必须标 linear", "field {2} of {1} is linear, so {3} must be marked linear too");
    ("%s 的变体 %s 的载荷不能含引用或区域（区域类型还没有实现）",
     "the payload of variant {2} of {1} cannot contain references or regions directly (make the enum generic instead)");
    ("%s 的变体 %s：没有载荷请直接写 %s", "variant {2} of {1}: for no payload just write {3}");
    ("%s 的变体 %s：载荷暂不能是 linear 值", "variant {2} of {1}: the payload cannot be linear yet");
    ("%s 里的方法 %s 重复", "{1} defines method {2} twice");
    ("trait %s 没有方法 %s", "trait {1} has no method {2}");
    ("%s 缺少方法 %s", "{1} is missing method {2}");
    ("%s 的方法 %s 有 %d 个参数，trait 里声明的是 %d 个",
     "method {2} in {1} has {3} parameters, but the trait declares {4}");
    ("%s 的方法 %s 的第 %d 个参数约定是 %s，trait 里声明的是 %s",
     "parameter {3} of method {2} in {1} is passed as {4}, but the trait declares {5}");
    ("%s 的方法 %s 的第 %d 个参数类型是 %s，应为 %s", "parameter {3} of method {2} in {1} has type {4}, expected {5}");
    ("%s 的方法 %s 返回 %s，应为 %s", "method {2} in {1} returns {3}, expected {4}");
    ("区域参数 %s 只能按 let 传入", "region parameter {1} can only be passed as let");
    ("main 必须是 fn main()，没有参数、没有返回值、没有类型参数",
     "main must be fn main(), with no parameters, no return value, and no type parameters");
    ("缺少 fn main()", "missing fn main()");
    ("泛型函数的实例超过 1000 个（可能是多态递归）",
     "more than 1000 instances of generic functions (perhaps polymorphic recursion)");
    ("%s 不拥有值（是 let / inout 参数或投影），不能移动",
     "{1} does not own its value (it is a let / inout parameter or a projection), so it cannot be moved");
    ("%s 不拥有值（是 let / inout 参数或投影），不能移走它的字段",
     "{1} does not own its value (it is a let / inout parameter or a projection), so its fields cannot be moved out");
    ("linear 结构体 %s 不能部分移动（要用 consume）", "linear struct {1} cannot be partially moved (use consume)");
    ("不能从 Box 或 Vec 里移出值（Box 用 unbox，Vec 的元素用 v.pop()）",
     "cannot move a value out of a Box or Vec (use unbox for a Box, v.pop() for Vec elements)");
    ("%s 在这里出错返回之前已经被消费，errdefer 会重复消费它",
     "{1} was already consumed before this error return, so the errdefer would consume it again");
    ("errdefer 登记时 %s 必须是还没有被消费的 linear 值",
     "when the errdefer is registered, {1} must be a linear value that has not been consumed");
    ("%s 已被移动，不能再使用", "{1} has been moved and cannot be used again");
    ("%s 可能已被移动（某条路径上或上一轮循环里），不能再使用",
     "{1} may have been moved (on some path or in a previous loop iteration) and cannot be used again");
    ("%s 已被移走，不能整体使用 %s", "{1} has been moved out, so {2} cannot be used as a whole");
    ("%s 已被移走，不能再使用", "{1} has been moved out and cannot be used again");
    ("%s 可能已被移走（某条路径上或上一轮循环里），不能整体使用 %s",
     "{1} may have been moved out (on some path or in a previous loop iteration), so {2} cannot be used as a whole");
    ("%s 可能已被移走（某条路径上或上一轮循环里），不能再使用",
     "{1} may have been moved out (on some path or in a previous loop iteration) and cannot be used again");
    ("%s 是 linear 值，%s在某些路径上还没有被消费", "{1} is linear and has not been consumed on some paths {2}");
    ("%s 是 linear 值，%s还没有被消费%s", "{1} is linear and has not been consumed {2}{3}");
    ("%s 已被移动，不能给它的字段赋值", "{1} has been moved, so its fields cannot be assigned");
    ("%s 可能已被移动（某条路径上或上一轮循环里），不能给它的字段赋值",
     "{1} may have been moved (on some path or in a previous loop iteration), so its fields cannot be assigned");
    ("%s 已被移走或可能已被移走，不能给它里面的位置赋值",
     "{1} has been (or may have been) moved out, so a place inside it cannot be assigned");
    ("%s 是 linear 值，重新赋值前必须先消费旧值", "{1} is linear; its old value must be consumed before it is reassigned");
    ("不能给 linear 的 inout 参数 %s 整体赋值（旧值没有被消费）",
     "cannot assign to the whole linear inout parameter {1} (the old value would not be consumed)");
    ("%s 在同一次调用里出现两次，其中有移动或 inout，只能都是只读的",
     "{1} appears twice in the same call and one use moves it or passes it as inout; both uses must be read-only");
    ("%s 正被投影 %s 借用（之后还会用到 %s），不能%s",
     "{1} is borrowed by projection {2} (which is used later), so it cannot be {4}");
    ("整数超出 I64 范围：%s", "integer literal out of I64 range: {1}");
    ("浮点数超出 F64 范围：%s", "float literal out of F64 range: {1}");
    ("字符字面量的引号里要正好是一个字符", "a character literal must contain exactly one character between the quotes");
    ("字符字面量的引号里要正好是一个字符（转义只有 \\n、\\t、\\\\、\\'、\\0）",
     "a character literal must contain exactly one character between the quotes (escapes: \\n, \\t, \\\\, \\', \\0)");
    ("无法识别的字符 '%s'", "unrecognized character '{1}'");
    ("字符串里只能用 \\n、\\r、\\t、\\\\、\\\" 这几种转义", "only the escapes \\n, \\r, \\t, \\\\ and \\\" are allowed in strings");
    ("字符串没有结束（缺少右边的 \"，字符串不能跨行）", "unterminated string (missing the closing \"; strings cannot span lines)");
    ("字符串里的 } 要写成 }}", "a } in a string must be written as }}");
    ("字符串里的 { 没有对应的 }（要写 { 本身请用 {{）", "the { in this string has no matching } (write {{ for a literal {)");
    ("字符串里的 {} 中间要写一个表达式", "the {} in a string must contain an expression");
    ("语法错误（在 '%s' 附近）", "syntax error near '{1}'");
    ("结构体和枚举的类型参数暂不能写 trait 约束", "type parameters of structs and enums cannot have trait bounds yet");
    (* 模块 *)
    ("%s 定义在模块 %s 里，只能在那里 consume（可以让模块 %s 提供一个 pub 的函数来消费它）",
     "{1} is defined in module {2} and can only be consumed there (module {3} can provide a pub function that consumes it)");
    ("模块 %s 里没有 %s", "module {1} has no item named {2}");
    ("%s.%s 不是 pub 的，不能在模块 %s 之外使用", "{1}.{2} is not pub and cannot be used outside module {3}");
    ("引入的 %s 和本文件里的定义重名", "the imported name {1} clashes with a definition in this file");
    ("没有引入模块 %s（在文件开头写 use %s）", "module {1} is not imported (add use {2} at the top of the file)");
    ("变量 %s 和引入的模块同名，换一个名字", "variable {1} has the same name as an imported module; rename it");
    ("impl 不用写 pub（实现跟着 trait 和类型，到处可见）", "impl does not take pub (an impl is visible wherever its trait and type are)");
    ("模块不能引入自己", "a module cannot import itself");
    ("找不到模块 %s（应当是文件 %s）", "cannot find module {1} (expected the file {2})");
    ("模块 %s 既是 %s 又是 %s，同一个程序里只能用其中一个",
     "module {1} is both {2} and {3}; a program can use only one of them");
    ("只有标准库的模块写成 std.m；自己的模块直接写 use %s",
     "only standard library modules are written std.m; for your own module write use {1}");
    ("标准库里没有模块 %s（有：%s）", "the standard library has no module {1} (available: {2})");
    ("panic 需要 1 个参数（说明文字）", "panic takes 1 argument (the message)");
    ("os_exit 需要 1 个参数（退出码）", "os_exit takes 1 argument (the exit code)");
    ("for %s in inout %s 的循环体里不能用 %s（%s 整个借给了循环，不能 push、pop、读或再借）",
     "the body of for {1} in inout {2} cannot use {3} ({4} is lent to the loop: no push, pop, read or reborrow)");
    ("%s 不可修改，不能给它的元素赋值（需要 var 变量或 inout / sink 参数）",
     "{1} is not mutable, cannot assign to its element (needs a var variable or an inout / sink parameter)");
    (* 下标 *)
    ("只能给变量、字段或 Vec 的元素（v[i]）赋值",
     "only a variable, a field or an element of a Vec (v[i]) can be assigned to");
    (* 字节串 *)
    ("字节串用 a.eq(b) 比较", "compare byte strings with a.eq(b)");
    (* trait：ToStr、默认方法、泛型 impl *)
    ("%s 没有实现 ToStr，不能在字符串里插值或调用 to_str（先写 fn to_str(x: %s) -> Str）",
     "{1} does not implement ToStr, so it cannot be interpolated or passed to to_str (write fn to_str(x: {2}) -> Str first)");
    ("%s 没有写约束 %s，而 %s 的 %s 要用到它（要写 %s: %s）",
     "{1} has no bound {2}, which the {4} of {3} needs (write {5}: {6})");
    ("%s 没有实现 %s，而 %s 的 %s 要用到它", "{1} does not implement {2}, which the {4} of {3} needs");
    ("to_str 是 trait ToStr 的方法，要写成 fn to_str(x: T) -> Str",
     "to_str is the method of the trait ToStr; write it as fn to_str(x: T) -> Str");
    ("不能为函数类型实现 trait", "a trait cannot be implemented for a function type");
    ("不能为类型参数 %s 本身实现 trait（impl<%s> … for %s 会和所有的 impl 重叠）",
     "a trait cannot be implemented for the type parameter {1} itself (impl<{2}> … for {3} would overlap every impl)");
    ("impl 的类型参数 %s 没有出现在 %s 里", "the impl type parameter {1} does not appear in {2}");
    ("impl %s for %s 和已有的 impl %s for %s 重叠（有的类型两个都能匹配）",
     "impl {1} for {2} overlaps the existing impl {3} for {4} (some type matches both)");
    (* 类型自己的方法 *)
    ("方法 %s 的第一个参数是接收者，类型要写成 %s（或 Self）",
     "the first parameter of method {1} is the receiver; its type must be {2} (or Self)");
    ("%s 已经有方法 %s", "{1} already has a method {2}");
    ("intrinsic.%s：内建函数只能在标准库里这样调用", "intrinsic.{1}: built-in functions can only be called this way inside the standard library");
    ("%s 是内建函数，标准库以外不能直接调用：写成 %s", "{1} is a built-in function and cannot be called directly outside the standard library: write {2}");
    ("%s 读写外部，标准库里只有 std.io、std.fs、std.env、std.process 和打印能调用它",
     "{1} does I/O; inside the standard library only std.io, std.fs, std.env, std.process and printing may call it");
    ("%s 没有方法 %s（%s 是普通函数：写成 %s(…)，或者放进 impl %s { … }）",
     "{1} has no method {2} ({3} is a plain function: write {4}(…), or put it in impl {5} { … })");
    ("%s 没有方法 %s", "{1} has no method {2}");
    ("intrinsic fn 只能写在标准库里", "intrinsic fn can only be written in the standard library");
    ("intrinsic fn 只能写在 impl 类型 { … } 里，不能实现 trait", "intrinsic fn can only appear in impl Type { … }, not in a trait impl");
    ("impl 里只能写 fn（%s 是什么？）", "only fn can appear in an impl (what is {1}?)");
    ("%s 的声明和内建函数 %s 的签名不一致", "the declaration of {1} does not match the signature of built-in function {2}");
    ("没有内建函数 %s", "there is no built-in function {1}");
    ("%s 已经去掉了：写成 %s", "{1} has been removed: write {2}");
    ("不能给 Arena 写方法", "methods cannot be defined on Arena");
    ("不能给函数类型写方法", "methods cannot be defined on a function type");
    ("不能给类型参数 %s 本身写方法（impl<%s> %s 会和所有的头部重叠）",
     "methods cannot be defined on the type parameter {1} itself (impl<{2}> {3} would overlap every head)");
    ("impl %s 和 impl %s 都有方法 %s，头部重叠（有的类型两个都能匹配）",
     "impl {1} and impl {2} both have a method {3}, and their heads overlap (some type matches both)");
    (* 嵌套模式 *)
    ("模式 %s 的类型是 %s，这里的值是 %s", "the pattern {1} has type {2}, but the value here is {3}");
    ("%s 不是枚举，不能用变体模式 %s", "{1} is not an enum; the variant pattern {2} cannot be used");
    ("%s 没有载荷，不能写 %s(…)", "{1} has no payload; {2}(…) cannot be written");
    ("%s 带一个 %s 类型的载荷，要写 %s(…)", "{1} carries a payload of type {2}; write {3}(…)");
    ("%s 不是 %d 个元素的元组，不能用元组模式", "{1} is not a tuple of {2} elements; the tuple pattern cannot be used");
    ("变量 %s 在同一个模式里出现了两次", "the variable {1} appears twice in the same pattern");
    ("match 的模式太复杂（要检查的情况超过 100000 种），请拆成几层 match",
     "the match patterns are too complex (more than 100000 cases to check); split it into several matches");
    ("match 没有覆盖所有情况，例如 %s", "the match does not cover every case, for example {1}");
    ("这个分支永远匹配不到（前面的分支已经覆盖了它能匹配的值）",
     "this arm can never match (the arms before it already cover every value it matches)");
    (* 闭包 *)
    ("函数类型 fn(…) 只能作为函数参数的类型（闭包不能存进变量、字段、Vec，也不能返回）",
     "a function type fn(…) can only be the type of a function parameter (closures cannot be stored in variables, fields or Vecs, or returned)");
    ("函数类型 fn(…) 里暂不能有引用或区域", "function types fn(…) cannot contain references or regions yet");
    ("%s 是函数参数，只能调用它（%s(…)），或者作为实参传给 fn(…) 类型的参数",
     "{1} is a function parameter: it can only be called ({2}(…)) or passed on to a parameter of type fn(…)");
    ("闭包只能直接写在调用的实参里，传给类型是 fn(…) 的参数",
     "a closure can only be written directly as a call argument, for a parameter of type fn(…)");
    (* 可以存起来的闭包 Fn(…) *)
    (* 字符串切片 &str in a *)
    ("未知类型 %s（字符串切片写 &str in 区域）", "unknown type {1} (a string slice is written &str in REGION)");
    ("text 的第一个参数必须是区域，%s 不是", "the first argument of text must be a region; {1} is not");
    ("text 需要 2 个参数：区域和字符串", "text takes 2 arguments: a region and a string");
    ("%s 的第一个参数必须是区域，%s 不是", "the first argument of {1} must be a region; {2} is not");
    ("eq_view 的参数必须是字符串切片，%s 不是（和 Str 比较用 eq）",
     "the argument of eq_view must be a string slice; {1} is not (use eq to compare with a Str)");
    ("%s 的第一个参数必须是字符串切片 &str in …，%s 不是", "the first argument of {1} must be a string slice &str in …; {2} is not");
    ("字符串切片不能解引用（要 Str 时用 to_str() 复制一份）",
     "a string slice cannot be dereferenced (use to_str() to copy it into a Str)");
    ("无法确定闭包的类型：写在调用的实参里传给 fn(…) 参数，或者写在知道类型的地方（例如 var f: Fn(I64) -> I64 = |x| …）",
     "cannot determine the type of this closure: pass it to a fn(…) parameter, or write it where the type is known (for example var f: Fn(I64) -> I64 = |x| …)");
    ("函数类型 Fn(…) 里暂不能有引用或区域", "function types Fn(…) cannot contain references or regions yet");
    ("类型 %s 后面不能跟括号（函数类型写 Fn(…) -> …）",
     "type {1} cannot be followed by parentheses (function types are written Fn(…) -> …)");
    ("Fn(…) 闭包暂不能用含引用或区域的变量 %s：闭包可以存起来，引用可能活不过它的区域",
     "an Fn(…) closure cannot use {1}, which contains references or regions: the closure may be stored, and the references might not outlive their region");
    ("Fn(…) 闭包暂不能用 linear 值 %s", "an Fn(…) closure cannot use the linear value {1} yet");
    ("%s 不可修改：调用 Fn(…) 闭包时闭包按 inout（它可以改自己的环境），要用 var 声明，或者作为 inout / sink 参数",
     "{1} cannot be modified: calling an Fn(…) closure passes it as inout (it may change its own environment), so declare it with var or take it as an inout / sink parameter");
    ("闭包里暂不能再写闭包", "closures inside closures are not supported yet");
    ("闭包有 %d 个参数，这里需要 %d 个", "the closure has {1} parameters, {2} expected here");
    ("闭包的参数 %s 重复", "closure parameter {1} is repeated");
    ("闭包里暂不能用函数参数 %s（可以把它作为实参直接传下去）",
     "a closure cannot use the function parameter {1} yet (pass it on directly as an argument instead)");
    ("闭包捕获的区域太多（最多 100 个）", "the closure captures too many regions (at most 100)");
    ("%s 的这个参数是 %s：要写闭包 |…| …，或者传入同类型的函数参数",
     "this parameter of {1} has type {2}: write a closure |…| …, or pass a function parameter of the same type");
    ("无法推断闭包参数的类型（%s 的类型参数 %s 要先从别的实参确定）",
     "cannot infer the closure's parameter types (type parameter {2} of {1} must be determined by another argument first)");
    ("errdefer 暂不能调用函数参数 %s", "errdefer cannot call the function parameter {1} yet");
    ("trait 的方法暂不能有 fn(…) 参数", "trait methods cannot have fn(…) parameters yet");
    ("impl 里的方法暂不能有 fn(…) 参数", "methods in an impl cannot have fn(…) parameters yet");
    ("函数类型的参数 %s 只能按 let 传入（不写约定）", "function-typed parameter {1} must be passed as let (write no mode)");
  ]

(* 填进信息里的固定中文短语。 *)
let words =
  [
    ("if ", "if"); ("match ", "match"); ("比较", "the comparison"); ("临时值", "a temporary value");
    ("结构体", "struct"); ("枚举", "enum");
    ("作用域结束时", "at the end of its scope"); ("从这里返回时", "when returning here");
    ("出错从这里返回时", "when returning with an error here"); ("（可以加 errdefer）", " (consider errdefer)");
    ("移动", "moved"); ("以 inout 传入", "passed as inout"); ("修改", "modified"); ("析构", "dropped");
  ]

let word s = Option.value (List.assoc_opt s words) ~default:s

(* 格式串按 %s、%d 切成字面的几段。 *)
let pieces p =
  let n = String.length p and buf = Buffer.create 32 and acc = ref [] and i = ref 0 in
  while !i < n do
    if !i + 1 < n && p.[!i] = '%' && (p.[!i + 1] = 's' || p.[!i + 1] = 'd') then begin
      acc := Buffer.contents buf :: !acc;
      Buffer.clear buf;
      i := !i + 2
    end
    else begin
      Buffer.add_char buf p.[!i];
      incr i
    end
  done;
  List.rev (Buffer.contents buf :: !acc)

let starts_at s i lit = i + String.length lit <= String.length s && String.sub s i (String.length lit) = lit

let rec find s lit i = if i + String.length lit > String.length s then None else if starts_at s i lit then Some i else find s lit (i + 1)

(* 信息 s 符合格式串 p 时，返回按顺序填进去的部分。每段字面取最早出现的位置，最后一段必须在结尾。 *)
let matches p s =
  let n = String.length s in
  let rec go i rest acc =
    match rest with
    | [] -> if i = n then Some (List.rev acc) else None
    | [ last ] ->
        let m = String.length last in
        if n - m >= i && starts_at s (n - m) last then Some (List.rev (String.sub s i (n - m - i) :: acc)) else None
    | lit :: more -> (
        match find s lit i with
        | Some j -> go (j + String.length lit) more (String.sub s i (j - i) :: acc)
        | None -> None)
  in
  match pieces p with
  | first :: rest when starts_at s 0 first -> go (String.length first) rest []
  | _ -> None

let fill t args =
  let n = String.length t and b = Buffer.create 64 and i = ref 0 in
  while !i < n do
    if t.[!i] = '{' && !i + 2 < n && t.[!i + 2] = '}' && t.[!i + 1] >= '1' && t.[!i + 1] <= '9' then begin
      Option.iter (fun a -> Buffer.add_string b (word a)) (List.nth_opt args (Char.code t.[!i + 1] - Char.code '1'));
      i := !i + 3
    end
    else begin
      Buffer.add_char b t.[!i];
      incr i
    end
  done;
  Buffer.contents b

(* 一条编译错误信息。 *)
let msg s =
  match !lang with
  | Zh -> s
  | En -> Option.value (List.find_map (fun (p, t) -> Option.map (fill t) (matches p s)) table) ~default:s

let rec replace_all s a b =
  match find s a 0 with
  | None -> s
  | Some i ->
      String.sub s 0 i ^ b ^ replace_all (String.sub s (i + String.length a) (String.length s - i - String.length a)) a b

(* 生成的 C 里运行时的信息（panic、泄漏）。 *)
let runtime text =
  match !lang with
  | Zh -> text
  | En ->
      List.fold_left
        (fun t (a, b) -> replace_all t a b)
        text
        [
          ("\"整数溢出\"", "\"integer overflow\""); ("\"除以零\"", "\"division by zero\"");
          ("\"F64 转换成 I64 时超出范围（或是 NaN）\"", "\"F64 to I64 conversion out of range (or NaN)\"");
          ("\"内存不足\"", "\"out of memory\""); ("\"下标越界\"", "\"index out of bounds\"");
          ("\"read_chunk 的大小必须大于 0\"", "\"the size given to read_chunk must be greater than 0\"");
          ("\"%s：退出码 %lld\"", "\"%s: exit code %lld\"");
          ("\"字节不在 0 到 255 之间\"", "\"byte not between 0 and 255\"");
          ("\"print_byte 的参数不在 0 到 255 之间\"", "\"the argument of print_byte is not between 0 and 255\"");
          ("个 Box 没有释放", "Box value(s) not freed");
        ]
