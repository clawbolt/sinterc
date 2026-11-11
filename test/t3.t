T3：编辑器的语法高亮（editor/vscode 是 VS Code 的 TextMate 语法，editor/vim 是 Vim 的语法文件）。
两份关键字表都要和词法分析的关键字一致：词法分析加了关键字而高亮没加时，这里会报出来。

  $ grep -o '"[a-z]*", [A-Z]*;' ../src/lexer.mll | sed 's/"\([a-z]*\)".*/\1/' > keywords
  $ wc -l < keywords | tr -d ' '
  25
  $ for k in $(cat keywords) I64 F64 Bool Char Str Bytes Self; do
  >   grep -qw "$k" ../editor/vscode/syntaxes/sinter.tmLanguage.json || echo "vscode 缺 $k"
  >   grep -qw "$k" ../editor/vim/syntax/sinter.vim || echo "vim 缺 $k"
  > done

VS Code 的扩展把 .sn 文件交给这份语法：

  $ grep -c '"\.sn"' ../editor/vscode/package.json
  1
  $ grep -c '"scopeName": "source.sinter"' ../editor/vscode/package.json ../editor/vscode/syntaxes/sinter.tmLanguage.json
  ../editor/vscode/package.json:1
  ../editor/vscode/syntaxes/sinter.tmLanguage.json:1
