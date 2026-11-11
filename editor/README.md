# 编辑器支持（T3）

只有语法高亮（关键字、类型、字符串插值 `"{x}"` 和 `{{`、注释、数字、`.Some` 这样的变体）；还没有语言服务器，
报错仍然看 `sinterc` 的输出。两份关键字表由 `test/editor_syntax.t` 检查和词法分析一致。

## VS Code

把 `editor/vscode` 整个目录复制（或链接）到扩展目录，重启 VS Code：

```sh
ln -s "$PWD/editor/vscode" ~/.vscode/extensions/sinter
```

也可以打包成 `.vsix`：在 `editor/vscode` 里运行 `npx @vscode/vsce package`，再用“从 VSIX 安装”。

## Vim / Neovim

```sh
mkdir -p ~/.vim
cp -r editor/vim/* ~/.vim/        # Neovim：~/.config/nvim/
```

`ftdetect` 把 `.sn` 认成 sinter，`ftplugin` 设置 `//` 注释和 4 个空格的缩进（和 `sinterc fmt` 一致）。

---

# Editor support (T3)

Syntax highlighting only (keywords, types, string interpolation `"{x}"` and `{{`, comments, numbers, variants such as
`.Some`); there is no language server yet. `test/editor_syntax.t` checks that both keyword lists match the lexer.

- **VS Code**: link or copy `editor/vscode` into `~/.vscode/extensions/sinter` and restart, or build a `.vsix` with
  `npx @vscode/vsce package` inside `editor/vscode`.
- **Vim / Neovim**: copy `editor/vim/*` into `~/.vim/` (or `~/.config/nvim/`).
