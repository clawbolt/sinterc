(* 一个函数类型检查之后的几步：降级成 MIR（mir.ml）、所有权检查与析构展开（own.ml）、投影锁定与排他性检查（borrow.ml）。
   每一步都可能报错（降级报“不拥有值，不能移动”这一类），报的是这个函数的第一个错误；
   这里把它们接起来，调用者只看结果，不用知道哪一步会抛什么。 *)

(* check 为假时（--unchecked）不报所有权的错误、不做投影检查，照样展开，把编译器会拒绝的程序也导出给 Lean。 *)
let func ?(check = true) d (fd : Tast.fn_def) : (Mir.func, Diag.t) result =
  match Mir.lower_fn d fd with
  | exception Diag.Error e -> Error e
  | f -> Result.bind (Own.elaborate ~check d f) (fun f -> if check then Borrow.check d f else Ok f)
