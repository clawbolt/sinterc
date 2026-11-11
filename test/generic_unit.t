() 作类型实参（Lean `Generics.UnitArg`）。() 是只有一个值的 copy 类型：可以作泛型函数、Vec、Option 等的类型实参，
也可以是变量、参数、字段、Box 里的值。Result<(), E> 的 Ok 有一个 () 载荷，写的时候可以省略（Ok 就是 Ok(())）。

  $ cat > unit.sn <<'EOF'
  > use std.option
  > use std.result
  > 
  > struct Pair<A, B> { a: A, b: B }
  > 
  > fn id<T>(sink x: T) -> T { x }
  > 
  > fn check(n: I64) -> Result<(), Str> {
  >     if n < 0 { return Err("负数") }
  >     Ok(())
  > }
  > 
  > fn twice(n: I64) -> Result<(), Str> {
  >     check(n)?
  >     let u = check(n + 1)?
  >     Ok(u)
  > }
  > 
  > fn main() {
  >     print_bool(result.is_ok(check(1)))
  >     print_bool(result.is_err(check(-1)))
  >     print_bool(option.is_some(result.ok(twice(3))))
  >     let u: () = id(())
  >     var v: Vec<()> = vec_new()
  >     v.push(u)
  >     v.push(())
  >     print_i64(v.len())
  >     match v.pop() {
  >         .Some(x) => print("弹出了一个 ()"),
  >         .None => print("空")
  >     }
  >     let o: Option<()> = .Some
  >     print_bool(option.is_some(o))
  >     let p = Pair { a: (), b: 7 }
  >     print_i64(p.b)
  >     let b = box(())
  >     let w = unbox(b)
  >     result.unwrap_or(twice(-5), w)
  >     match twice(-2) {
  >         Ok(z) => print("成功"),
  >         Err(e) => print(e)
  >     }
  > }
  > EOF
  $ sinterc run unit.sn
  true
  true
  true
  2
  弹出了一个 ()
  true
  7
  负数

还是不能引用 ()，区域里也不能放 ()：

  $ cat > ref.sn <<'EOF'
  > fn main() {
  >     arena |r| {
  >         let x = alloc(r, ())
  >     }
  > }
  > EOF
  $ sinterc ref.sn
  ref.sn:3:26: 错误：区域里不能放 ()
   3 |         let x = alloc(r, ())
     |                          ^
  [1]
