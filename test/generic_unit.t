() 作类型实参（Lean `Generics.UnitArg`）。() 是只有一个值的 copy 类型：可以作泛型函数、Vec、Option 等的类型实参，
也可以是变量、参数、字段、Box 里的值。Result<(), E> 的 Ok 有一个 () 载荷，写的时候可以省略（Ok 就是 Ok(())）。

  $ cat > unit.sn <<'EOF'
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
  >     println("{check(1).is_ok()}")
  >     println("{check(-1).is_err()}")
  >     println("{twice(3).ok().is_some()}")
  >     let u: () = id(())
  >     var v: Vec<()> = Vec.new()
  >     v.push(u)
  >     v.push(())
  >     println("{v.len()}")
  >     match v.pop() {
  >         .Some(x) => println("弹出了一个 ()"),
  >         .None => println("空")
  >     }
  >     let o: Option<()> = .Some
  >     println("{o.is_some()}")
  >     let p = Pair { a: (), b: 7 }
  >     println("{p.b}")
  >     let b = box(())
  >     let w = unbox(b)
  >     twice(-5).unwrap_or(w)
  >     match twice(-2) {
  >         Ok(z) => println("成功"),
  >         Err(e) => println(e)
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
