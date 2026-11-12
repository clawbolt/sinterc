预导入的核心部分 std/core/：每个程序都自动读进来，不用 use。trait（Eq、Ord、Hash、Ordering）、泛型的 min / max，
以及 I64、F64、Char、Option、Result、Str、&str in a、Bytes、Vec<T> 的方法（impl 类型 { … }，Lean `Traits.Inherent`）。
和旧模块（std.str、std.vec）重复的方法对照旧模块的结果。

数、字符、Option、Result、比较：

  $ cat > num.sn <<'EOF'
  > fn main() {
  >     let a = -7
  >     print_i64(a.abs())
  >     print_i64(a.sign())
  >     print_i64(a.min(3))
  >     print_i64(a.max(3))
  >     let b = 15
  >     print_i64(b.clamp(0, 10))
  >     print_i64(b.pow(2))
  >     print_i64(b.gcd(-25))
  >     print_i64(b.lcm(4))
  >     print_f64(b.to_f64())
  >     let x = 2.5
  >     let y = -2.5
  >     print_f64(x.round())
  >     print_f64(y.round())
  >     print_f64(x.floor())
  >     print_f64(x.ceil())
  >     print_f64(y.abs())
  >     print_f64(x.powi(-2))
  >     print_f64(x.max(y))
  >     print_i64(x.to_i64())
  >     let q = 16.0
  >     print_f64(q.sqrt())
  >     let c = 'q'
  >     print_bool(c.is_alpha())
  >     print(str_from_char(c.to_upper()))
  >     let d = '7'
  >     print_i64(d.to_digit().unwrap_or(-1))
  >     print_i64(c.to_digit().unwrap_or(-1))
  >     let o: Option<I64> = .Some(4)
  >     print_bool(o.is_some())
  >     let none: Option<I64> = .None
  >     print_bool(none.is_none())
  >     print_i64(none.unwrap_or(9))
  >     print_i64(o.map(|v| v * 10).unwrap())
  >     let r: Result<I64, Str> = Err("bad")
  >     print_bool(r.is_err())
  >     print_i64(r.unwrap_or(0))
  >     let r2: Result<I64, Str> = Ok(5)
  >     print_i64(r2.ok().unwrap())
  >     let p = 3
  >     print_i64(p.cmp(5).to_i64())
  >     print_bool(p.cmp(3).is_eq())
  >     print_i64(p.cmp(1).reverse().to_i64())
  >     print_i64(max(4, 9))
  >     print(min("pear", "apple"))
  >     let w = "b"
  >     print_bool(w.cmp("a").is_gt())
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
  > use std.str
  > fn main() {
  >     let s = "  Hello, World  "
  >     let t = s.trim()
  >     print(t)
  >     print_bool(t.eq(str.trim(s)))
  >     print_bool(t.starts_with("Hell"))
  >     print_bool(t.ends_with("x"))
  >     print_i64(t.find("o").unwrap_or(-1))
  >     print_i64(t.find_from("o", 5).unwrap_or(-1))
  >     print_bool(t.contains("World"))
  >     print(t.to_upper())
  >     print(t.to_lower())
  >     print(t.replace("l", "L"))
  >     print_bool(t.replace("l", "L").eq(str.replace(t, "l", "L")))
  >     let ab = "ab"
  >     print(ab.repeat(3))
  >     let csv = "a,,b"
  >     let parts = csv.split(",")
  >     print_i64(parts.len())
  >     print(parts.join("|"))
  >     print_bool(parts.join("|").eq(str.join(str.split(csv, ","), "|")))
  >     let crlf = "x\r\ny\n"
  >     print_i64(crlf.lines().len())
  >     print(crlf.lines().join("+"))
  >     let num = "-42"
  >     print_i64(num.parse_i64().unwrap_or(0))
  >     let e = ""
  >     print_bool(e.is_empty())
  >     arena |a| {
  >         let line = text(a, " x, 42 ,y\n")
  >         let fs = line.trim().split(",")
  >         print_i64(fs.len())
  >         print(fs[1].trim().to_str())
  >         print_i64(fs[1].trim().parse_i64().unwrap_or(0) + 1)
  >         print(line.field(",", 2).unwrap().trim().to_str())
  >         print_bool(line.field(",", 3).is_none())
  >         print_bool(line.trim().starts_with("x"))
  >         print_i64(line.line_end(0))
  >         print_i64(line.find_from("4", 0).unwrap_or(-1))
  >     }
  >     let hs = "hé"
  >     let raw = hs.to_bytes()
  >     print_bool(raw.decode().is_some())
  >     print_bool(raw.is_empty())
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
  >     var v: Vec<I64> = vec_new()
  >     v.push(3)
  >     v.push(1)
  >     v.push(2)
  >     v.push(3)
  >     print_i64(v.sum())
  >     print_i64(v.max().unwrap())
  >     print_i64(v.min().unwrap())
  >     print_bool(v.contains(2))
  >     print_i64(v.index_of(3).unwrap_or(-1))
  >     print_i64(v.count(|x| x > 1))
  >     print_bool(v.any(|x| x == 1))
  >     print_bool(v.all(|x| x > 0))
  >     print_i64(v.fold(0, |acc, x| acc * 10 + x))
  >     let doubled = v.map(|x| x * 2)
  >     print_i64(doubled.sum())
  >     v.sort()
  >     print_i64(v.fold(0, |acc, x| acc * 10 + x))
  >     print_i64(v.binary_search(3).unwrap_or(-1))
  >     v.dedup()
  >     print_i64(v.fold(0, |acc, x| acc * 10 + x))
  >     v.reverse()
  >     print_i64(v.fold(0, |acc, x| acc * 10 + x))
  >     let big = v.filter(|x| x >= 2)
  >     print_i64(big.fold(0, |acc, x| acc * 10 + x))
  >     var total = 0
  >     big.for_each(|x| { total = total + x })
  >     print_i64(total)
  >     var f: Vec<F64> = vec_new()
  >     f.push(1.5)
  >     f.push(-2.0)
  >     print_f64(f.sum())
  >     print_f64(f.max().unwrap())
  >     print_bool(f.is_empty())
  >     var names: Vec<Str> = vec_new()
  >     names.push("b")
  >     names.push("a")
  >     names.sort()
  >     print(names.join(","))
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
  >     print_i64(max(4, 9))
  > }
  > EOF
  $ sinterc run own.sn
  0

没有用到的核心函数不输出到 C 里：

  $ cat > tiny.sn <<'EOF'
  > fn main() {
  >     print_i64(1)
  > }
  > EOF
  $ sinterc tiny.sn -o tiny.c && grep -c "is_digit" tiny.c
  0
  [1]
  $ cat > used.sn <<'EOF'
  > fn main() {
  >     let c = '5'
  >     print_bool(c.is_digit())
  > }
  > EOF
  $ sinterc used.sn -o used.c && grep -q "is_digit" used.c && echo 输出了
  输出了
