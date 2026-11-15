英文的信息表（src/messages.ml）按代码里的中文格式串查。表里的键都要还在代码里（改了中文要同时改表），
没有英文的报错列在下面：新加的报错如果出现在这里，要么补上英文，要么更新这份列表。
新加的、会报错的源文件要加进下面的列表。

  $ cp check_messages.exe check && chmod +x check
  $ ./check ../src/lexer.mll ../src/parser.mly ../src/modules.ml ../src/collect.ml ../src/typeck.ml ../src/typeck_builtins.ml ../src/typeck_env.ml ../src/typeck_expr.ml ../src/typeck_generics.ml ../src/typeck_pattern.ml ../src/typeck_regions.ml ../src/mir.ml ../src/own.ml ../src/borrow.ml
  英文表 262 条，键都能在代码里找到
  没有英文的报错 5 条：
    for x in inout 只能遍历变量或字段里的 Vec
    match 有两个 _ 分支
    只能给变量里的位置赋值
    不能给一个不会返回的值赋值
    元素是 linear 值，不能直接赋值（旧值没有被消费）
