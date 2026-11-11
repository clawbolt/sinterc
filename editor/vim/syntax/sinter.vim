" Vim 的 Sinter 语法高亮（T3）。和 editor/vscode 的语法用同一份关键字表，test/t3.t 检查它和词法分析一致。
if exists("b:current_syntax")
  finish
endif

syn keyword sinterControl if else while for in match return break continue errdefer
syn keyword sinterDecl fn struct enum trait impl let var use pub arena
syn keyword sinterModifier inout sink linear
syn keyword sinterBool true false
syn keyword sinterType I64 F64 Bool Char Str Bytes Self Box Vec Option Result Arena ToStr
syn keyword sinterVariant Ok Err
syn keyword sinterBuiltin print print_str print_i64 print_f64 print_bool print_char print_byte print_bytes panic box unbox consume alloc read_line read_file write_file read_bytes write_bytes args vec_new bytes_new str_from_utf8

syn match sinterUserType "\<[A-Z][A-Za-z0-9_]*\>"
syn match sinterDotVariant "\(\w\|)\|\]\)\@<!\.[A-Z][A-Za-z0-9_]*"
syn match sinterFloat "\<[0-9]\+\(\.[0-9]\+\([eE][+-]\=[0-9]\+\)\=\|[eE][+-]\=[0-9]\+\)\>"
syn match sinterNumber "\<[0-9]\+\>"
syn match sinterChar "'\(\\[nt\\'0]\|[^\\']\)'"
syn match sinterComment "//.*$" contains=@Spell

syn match sinterEscape contained "\\[nrt\\\"]"
syn match sinterBraces contained "{{\|}}"
syn region sinterInterp contained matchgroup=sinterInterpDelim start="{" end="}" contains=TOP
syn region sinterString start=+"+ skip=+\\"+ end=+"+ contains=sinterEscape,sinterBraces,sinterInterp

hi def link sinterControl Conditional
hi def link sinterDecl Keyword
hi def link sinterModifier StorageClass
hi def link sinterBool Boolean
hi def link sinterType Type
hi def link sinterUserType Type
hi def link sinterVariant Constant
hi def link sinterDotVariant Constant
hi def link sinterBuiltin Function
hi def link sinterFloat Float
hi def link sinterNumber Number
hi def link sinterChar Character
hi def link sinterComment Comment
hi def link sinterString String
hi def link sinterEscape SpecialChar
hi def link sinterBraces SpecialChar
hi def link sinterInterpDelim Delimiter

let b:current_syntax = "sinter"
