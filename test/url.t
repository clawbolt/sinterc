std.url：URL 的解析、拼写和 %XX 编码。

  $ cat > url.sn <<'EOF'
  > use std.url
  > 
  > fn show(s: Str) {
  >     match url.Url.parse(s) {
  >         Ok(u) => println("{u.scheme} | {u.user} | {u.host} | {u.port()} | {u.path} | {u.query} | {u.fragment} | {u}"),
  >         Err(e) => println("错误：{e}")
  >     }
  > }
  > 
  > fn main() {
  >     show("http://example.com")
  >     show("HTTPS://User@Example.COM:8443/a/b?x=1&y=2#top")
  >     show("http://example.com:80/index.html")
  >     show("http://[::1]:8080/p?q")
  >     show("ws://localhost/chat#")
  >     show("example.com/a")
  >     show("http://a:b:c/")
  >     show("http://h:99999/")
  >     show("http://[::1/")
  >     match url.Url.parse("http://h/a/b/c.html?x") {
  >         Ok(u) => {
  >             println("{u.request_target()} {u.authority()}")
  >             println("{u.join("d.html").unwrap()} {u.join("/e").unwrap()} {u.join("https://o/z").unwrap()}")
  >         },
  >         Err(e) => println(e)
  >     }
  > }
  > EOF
  $ sinterc run url.sn
  http |  | example.com | 80 | / |  |  | http://example.com/
  https | User | example.com | 8443 | /a/b | x=1&y=2 | top | https://User@example.com:8443/a/b?x=1&y=2#top
  http |  | example.com | 80 | /index.html |  |  | http://example.com/index.html
  http |  | ::1 | 8080 | /p | q |  | http://[::1]:8080/p?q
  ws |  | localhost | 80 | /chat |  |  | ws://localhost/chat
  错误：不是 URL（没有 ://）：example.com/a
  错误：URL 的主机名里有多个 :（IPv6 要加方括号）：http://a:b:c/
  错误：URL 的端口不在 0..65535：http://h:99999/
  错误：URL 的 IPv6 地址缺 ]：http://[::1/
  /a/b/c.html?x h
  http://h/a/b/d.html http://h/e https://o/z

编码和解码；查询的 + 是空格。

  $ cat > enc.sn <<'EOF'
  > use std.url
  > 
  > fn main() {
  >     println(url.encode("中 a/b?c=d&e~"))
  >     println(url.encode_path("/文件/a b.txt"))
  >     println(url.decode("%E4%B8%AD%20a%2Fb").unwrap())
  >     println("{url.decode("%E4%B8").is_none()} {url.decode("%zz").is_none()} {url.decode("100%").is_none()}")
  >     for p in url.parse_query("q=sinter+lang&lang=%E4%B8%AD&flag&&x=a=b") {
  >         println("[{p.0}] = [{p.1}]")
  >     }
  >     var v: Vec<(Str, Str)> = Vec.new()
  >     v.push(("名字", "张 三"))
  >     v.push(("a&b", "1=2"))
  >     println(url.encode_query(v))
  > }
  > EOF
  $ sinterc run enc.sn
  %E4%B8%AD%20a%2Fb%3Fc%3Dd%26e~
  /%E6%96%87%E4%BB%B6/a%20b.txt
  中 a/b
  true true true
  [q] = [sinter lang]
  [lang] = [中]
  [flag] = []
  [x] = [a=b]
  %E5%90%8D%E5%AD%97=%E5%BC%A0%20%E4%B8%89&a%26b=1%3D2
