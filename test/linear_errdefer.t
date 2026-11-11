linear 值、Result、?、match、errdefer。

正常提交、出错回滚、一开始就失败三条路径；match 取出 Result 里的值：

  $ cat > ok.sn <<'EOF'
  > linear struct Tx { id: I64, log: Box<I64> }
  > 
  > fn begin(id: I64) -> Result<Tx, I64> {
  >     if id < 0 { return Err(id) }
  >     Ok(Tx { id: id, log: box(id * 10) })
  > }
  > 
  > fn exec(t: Tx, v: I64) -> Result<(), I64> {
  >     if v > 100 { return Err(v) }
  >     print_i64(*t.log + v)
  >     Ok(())
  > }
  > 
  > fn commit(sink t: Tx) {
  >     print_i64(t.id)
  >     consume(t)
  > }
  > 
  > fn rollback(sink t: Tx) {
  >     print_i64(0 - t.id)
  >     consume(t)
  > }
  > 
  > fn transfer(id: I64, v: I64) -> Result<(), I64> {
  >     let tx = begin(id)?
  >     errdefer rollback(tx)
  >     exec(tx, v)?
  >     commit(tx)
  >     Ok(())
  > }
  > 
  > fn report(r: Result<(), I64>) {
  >     match r {
  >         Ok(_) => print_bool(true)
  >         Err(e) => print_i64(e)
  >     }
  > }
  > 
  > fn main() {
  >     report(transfer(1, 5))
  >     report(transfer(2, 500))
  >     report(transfer(-3, 5))
  >     let r: Result<Box<I64>, I64> = Ok(box(7))
  >     let n = match r {
  >         Ok(b) => *b,
  >         Err(e) => e
  >     }
  >     print_i64(n)
  > }
  > EOF
  $ sinterc ok.sn -o ok.c && cc -std=gnu11 -w ok.c -o ok && ./ok
  15
  1
  true
  -2
  500
  -3
  7

下面的反例共用这 16 行，被测的函数从第 17 行开始：

  $ cat > lib.sn <<'EOF'
  > linear struct Tx { id: I64 }
  > 
  > fn begin(id: I64) -> Result<Tx, I64> {
  >     if id < 0 { return Err(id) }
  >     Ok(Tx { id: id })
  > }
  > 
  > fn check(v: I64) -> Result<(), I64> {
  >     if v > 100 { return Err(v) }
  >     Ok(())
  > }
  > 
  > fn commit(sink t: Tx) { consume(t) }
  > 
  > fn rollback(sink t: Tx) { consume(t) }
  > fn main() { }
  > EOF

忘了提交：正常路径上 tx 没有被消费（设计稿的 L01）：

  $ cat > part <<'EOF'
  > fn f(id: I64) -> Result<(), I64> {
  >     let tx = begin(id)?
  >     errdefer rollback(tx)
  >     check(id)?
  >     Ok(())
  > }
  > EOF
  $ cat lib.sn part > forget.sn && sinterc forget.sn
  forget.sn:18:5: 错误：tx 是 linear 值，作用域结束时还没有被消费
   18 |     let tx = begin(id)?
      |     ^
  [1]

没有 errdefer：出错返回的路径上 tx 没有被消费（Lean `Linearity.Errdefer` 的反例）：

  $ cat > part <<'EOF'
  > fn f(id: I64) -> Result<(), I64> {
  >     let tx = begin(id)?
  >     check(id)?
  >     commit(tx)
  >     Ok(())
  > }
  > EOF
  $ cat lib.sn part > noguard.sn && sinterc noguard.sn
  noguard.sn:19:14: 错误：tx 是 linear 值，出错从这里返回时还没有被消费（可以加 errdefer）
   19 |     check(id)?
      |              ^
  [1]

先手动消费，再出错返回：errdefer 会重复消费（Lean `Linearity.Errdefer` 的反例）：

  $ cat > part <<'EOF'
  > fn f(id: I64) -> Result<(), I64> {
  >     let tx = begin(id)?
  >     errdefer rollback(tx)
  >     commit(tx)
  >     check(id)?
  >     Ok(())
  > }
  > EOF
  $ cat lib.sn part > double.sn && sinterc double.sn
  double.sn:21:14: 错误：tx 在这里出错返回之前已经被消费，errdefer 会重复消费它
   21 |     check(id)?
      |              ^
  [1]

errdefer 登记时 tx 已经被消费。后面没有出错路径，只看出错路径的数据流查不出来，
所以在登记处检查（`FlowGraph.Lower` 的登记规则）：

  $ cat > part <<'EOF'
  > fn f(id: I64) -> Result<(), I64> {
  >     let tx = begin(id)?
  >     commit(tx)
  >     errdefer rollback(tx)
  >     Ok(())
  > }
  > EOF
  $ cat lib.sn part > late.sn && sinterc late.sn
  late.sn:20:5: 错误：errdefer 登记时 tx 必须是还没有被消费的 linear 值
   20 |     errdefer rollback(tx)
      |     ^
  [1]

同一个值登记两次 errdefer：

  $ cat > part <<'EOF'
  > fn f(id: I64) -> Result<(), I64> {
  >     let tx = begin(id)?
  >     errdefer rollback(tx)
  >     errdefer commit(tx)
  >     commit(tx)
  >     Ok(())
  > }
  > EOF
  $ cat lib.sn part > twice.sn && sinterc twice.sn
  twice.sn:20:5: 错误：tx 已经有一个生效的 errdefer，不能再登记
   20 |     errdefer commit(tx)
      |     ^
  [1]

只在一个分支里消费（Lean `Linearity.Linear`：分支两边必须一致）：

  $ cat > part <<'EOF'
  > fn f(c: Bool) {
  >     let tx = Tx { id: 1 }
  >     if c { commit(tx) }
  > }
  > EOF
  $ cat lib.sn part > branch.sn && sinterc branch.sn
  branch.sn:18:5: 错误：tx 是 linear 值，作用域结束时在某些路径上还没有被消费
   18 |     let tx = Tx { id: 1 }
      |     ^
  [1]
