预导入的核心部分 std/core/：每个程序都自动读进来，不用 use。trait（Eq、Ord、Hash、Ordering）、泛型的 min / max，
以及 I64、F64、Char、Option、Result、Str、&str in a、Bytes、Vec<T> 的方法（impl 类型 { … }，Lean `Traits.Inherent`）。
从旧模块（std.str、std.vec）迁移过来的方法，结果和原来的函数相同。

数、字符、Option、Result、比较：

  $ cat > num.sn <<'EOF'
  > fn main() {
  >     let a = -7
  >     println("{a.abs()}")
  >     println("{a.sign()}")
  >     println("{a.min(3)}")
  >     println("{a.max(3)}")
  >     let b = 15
  >     println("{b.clamp(0, 10)}")
  >     println("{b.pow(2)}")
  >     println("{b.gcd(-25)}")
  >     println("{b.lcm(4)}")
  >     println("{b.to_f64()}")
  >     let x = 2.5
  >     let y = -2.5
  >     println("{x.round()}")
  >     println("{y.round()}")
  >     println("{x.floor()}")
  >     println("{x.ceil()}")
  >     println("{y.abs()}")
  >     println("{x.powi(-2)}")
  >     println("{x.max(y)}")
  >     println("{x.to_i64()}")
  >     let q = 16.0
  >     println("{q.sqrt()}")
  >     let c = 'q'
  >     println("{c.is_alpha()}")
  >     println(c.to_upper().to_str())
  >     let d = '7'
  >     println("{d.to_digit().unwrap_or(-1)}")
  >     println("{c.to_digit().unwrap_or(-1)}")
  >     let o: Option<I64> = .Some(4)
  >     println("{o.is_some()}")
  >     let none: Option<I64> = .None
  >     println("{none.is_none()}")
  >     println("{none.unwrap_or(9)}")
  >     println("{o.map(|v| v * 10).unwrap()}")
  >     let r: Result<I64, Str> = Err("bad")
  >     println("{r.is_err()}")
  >     println("{r.unwrap_or(0)}")
  >     let r2: Result<I64, Str> = Ok(5)
  >     println("{r2.ok().unwrap()}")
  >     let p = 3
  >     println("{p.cmp(5).to_i64()}")
  >     println("{p.cmp(3).is_eq()}")
  >     println("{p.cmp(1).reverse().to_i64()}")
  >     println("{max(4, 9)}")
  >     println(min("pear", "apple"))
  >     let w = "b"
  >     println(w.cmp("a").is_gt().to_str())
  > }
  > EOF
  $ sinterc run num.sn
  7
  -1
  -7
  3
  10
  225
  5
  60
  15.0
  3.0
  -3.0
  2.0
  3.0
  2.5
  0.16
  2.5
  2
  4.0
  true
  Q
  7
  -1
  true
  true
  9
  40
  true
  0
  5
  -1
  true
  -1
  9
  apple
  true

字符串、字符串切片、字节串：

  $ cat > text.sn <<'EOF'
  > fn main() {
  >     let s = "  Hello, World  "
  >     let t = s.trim()
  >     println(t)
  >     println("{t.eq(s.trim())}")
  >     println(t.starts_with("Hell").to_str())
  >     println(t.ends_with("x").to_str())
  >     println(t.find("o").unwrap_or(-1).to_str())
  >     println(t.find_from("o", 5).unwrap_or(-1).to_str())
  >     println(t.contains("World").to_str())
  >     println(t.to_upper())
  >     println(t.to_lower())
  >     println(t.replace("l", "L"))
  >     println(t.replace("l", "L").eq(t.replace("l", "L")).to_str())
  >     let ab = "ab"
  >     println(ab.repeat(3))
  >     let csv = "a,,b"
  >     let parts = csv.split(",")
  >     println("{parts.len()}")
  >     println(parts.join("|"))
  >     println(parts.join("|").eq(csv.split(",").join("|")).to_str())
  >     let crlf = "x\r\ny\n"
  >     println("{crlf.lines().len()}")
  >     println(crlf.lines().join("+"))
  >     let num = "-42"
  >     println("{num.parse_i64().unwrap_or(0)}")
  >     let e = ""
  >     println("{e.is_empty()}")
  >     arena |a| {
  >         let line = text(a, " x, 42 ,y\n")
  >         let fs = line.trim().split(",")
  >         println("{fs.len()}")
  >         println(fs[1].trim().to_str())
  >         println("{fs[1].trim().parse_i64().unwrap_or(0) + 1}")
  >         println(line.field(",", 2).unwrap().trim().to_str())
  >         println(line.field(",", 3).is_none().to_str())
  >         println(line.trim().starts_with("x").to_str())
  >         println("{line.line_end(0)}")
  >         println(line.find_from("4", 0).unwrap_or(-1).to_str())
  >     }
  >     let hs = "hé"
  >     let raw = hs.to_bytes()
  >     println("{raw.decode().is_some()}")
  >     println("{raw.is_empty()}")
  > }
  > EOF
  $ sinterc run text.sn
  Hello, World
  true
  true
  false
  4
  8
  true
  HELLO, WORLD
  hello, world
  HeLLo, WorLd
  true
  ababab
  3
  a||b
  true
  2
  x+y
  -42
  true
  3
  42
  43
  y
  true
  true
  9
  4
  true
  false

Vec：

  $ cat > vec.sn <<'EOF'
  > fn main() {
  >     var v: Vec<I64> = Vec.new()
  >     v.push(3)
  >     v.push(1)
  >     v.push(2)
  >     v.push(3)
  >     println("{v.sum()}")
  >     println("{v.max().unwrap()}")
  >     println("{v.min().unwrap()}")
  >     println("{v.contains(2)}")
  >     println("{v.index_of(3).unwrap_or(-1)}")
  >     println("{v.count(|x| x > 1)}")
  >     println("{v.any(|x| x == 1)}")
  >     println("{v.all(|x| x > 0)}")
  >     println("{v.fold(0, |acc, x| acc * 10 + x)}")
  >     let doubled = v.map(|x| x * 2)
  >     println("{doubled.sum()}")
  >     v.sort()
  >     println("{v.fold(0, |acc, x| acc * 10 + x)}")
  >     println("{v.binary_search(3).unwrap_or(-1)}")
  >     v.dedup()
  >     println("{v.fold(0, |acc, x| acc * 10 + x)}")
  >     v.reverse()
  >     println("{v.fold(0, |acc, x| acc * 10 + x)}")
  >     let big = v.filter(|x| x >= 2)
  >     println("{big.fold(0, |acc, x| acc * 10 + x)}")
  >     var total = 0
  >     big.for_each(|x| { total = total + x })
  >     println("{total}")
  >     var f: Vec<F64> = Vec.new()
  >     f.push(1.5)
  >     f.push(-2.0)
  >     println("{f.sum()}")
  >     println("{f.max().unwrap()}")
  >     println("{f.is_empty()}")
  >     var names: Vec<Str> = Vec.new()
  >     names.push("b")
  >     names.push("a")
  >     names.sort()
  >     println(names.join(","))
  > }
  > EOF
  $ sinterc run vec.sn
  9
  3
  1
  true
  0
  3
  true
  true
  3123
  18
  1233
  2
  123
  321
  32
  5
  -0.5
  1.5
  false
  a,b

自己的定义优先于预导入的名字：

  $ cat > own.sn <<'EOF'
  > fn max(a: I64, b: I64) -> I64 { 0 }
  > fn main() {
  >     println("{max(4, 9)}")
  > }
  > EOF
  $ sinterc run own.sn
  0

没有用到的核心函数不输出到 C 里：

  $ cat > tiny.sn <<'EOF'
  > fn main() {
  >     println("{1}")
  > }
  > EOF
  $ sinterc tiny.sn -o tiny.c && grep -c "is_digit" tiny.c
  0
  [1]
  $ cat > used.sn <<'EOF'
  > fn main() {
  >     let c = '5'
  >     println("{c.is_digit()}")
  > }
  > EOF
  $ sinterc used.sn -o used.c && grep -q "is_digit" used.c && echo 输出了
  输出了
