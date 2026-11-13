std.http：HTTP/1.1 的服务端和客户端。服务端和客户端在同一个程序里，端口由系统选。

路由：:名字 匹配一段，* 匹配剩下的所有段。

  $ cat > route.sn <<'EOF'
  > use std.http
  > 
  > fn show(path: Str, pattern: Str) {
  >     match http.route(path, pattern) {
  >         .Some(ps) => println("{path} ~ {pattern}：{ps.len()} [{ps.join(",")}]"),
  >         .None => println("{path} ~ {pattern}：不匹配")
  >     }
  > }
  > 
  > fn main() {
  >     show("/", "/")
  >     show("/users/42/posts", "/users/:id/posts")
  >     show("/users//posts", "/users/:id/posts")
  >     show("/users/42", "/users/:id/posts")
  >     show("/static/css/a.css", "/static/*")
  >     show("/a/b", "/a")
  >     var h = http.Headers.new()
  >     h.add("Set-Cookie", "a=1")
  >     h.add("set-cookie", "b=2")
  >     h.set("CONTENT-TYPE", "text/plain")
  >     h.set("content-type", "text/html")
  >     println("{h.len()} {h.get("SET-COOKIE").unwrap()} {h.get("Content-Type").unwrap()} {h.has("x")}")
  >     println("{http.reason(404)} {http.reason(299)}")
  > }
  > EOF
  $ sinterc run route.sn
  / ~ /：0 []
  /users/42/posts ~ /users/:id/posts：1 [42]
  /users//posts ~ /users/:id/posts：不匹配
  /users/42 ~ /users/:id/posts：不匹配
  /static/css/a.css ~ /static/*：1 [css/a.css]
  /a/b ~ /a：不匹配
  3 a=1 text/html false
  Not Found Unknown

keep-alive：一个连接上连发三个请求（第二个是 HEAD：有 Content-Length，没有回复体；第三个说 Connection: close），
请求体用 chunked 发。客户端直接用 std.net 写原始的报文。

  $ cat > keepalive.sn <<'EOF'
  > use std.net
  > use std.http { Handler }
  > 
  > struct Echo { tag: Str }
  > 
  > impl Handler for Echo {
  >     fn handle(e: Echo, req: http.Request) -> http.Response {
  >         http.Response.text(200, "{req.method} {req.path} {req.body.len()}").with_header(e.tag, req.version)
  >     }
  > }
  > 
  > fn raw(port: I64) -> Result<(), Str> {
  >     let c = net.connect("127.0.0.1", port)?
  >     let w1 = c.write_str("POST /up HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n6;ext=1\r\n world\r\n0\r\nTrailer: t\r\n\r\n")
  >     let w2 = c.write_str("HEAD /h HTTP/1.1\r\nHost: x\r\n\r\n")
  >     let w3 = c.write_str("GET /last HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")
  >     let all = c.read_all()
  >     c.close()
  >     let text = all?.decode().unwrap_or("?")
  >     for line in text.split("\r\n") {
  >         if line.starts_with("HTTP/") || line.starts_with("Content-Length") || line.starts_with("Connection") || line.starts_with("POST") || line.starts_with("GET") {
  >             println(line)
  >         }
  >     }
  >     Ok(())
  > }
  > 
  > fn main() {
  >     match net.listen("127.0.0.1", 0) {
  >         Ok(l) => {
  >             let port = l.port()
  >             let h = Echo { tag: "X-Seen" }
  >             scope |s| {
  >                 s.spawn(|| http.serve(l, 1, h))
  >                 match raw(port) {
  >                     Ok(_) => {},
  >                     Err(e) => println(e)
  >                 }
  >             }
  >             l.close()
  >         },
  >         Err(e) => println(e)
  >     }
  > }
  > EOF
  $ sinterc run keepalive.sn
  HTTP/1.1 200 OK
  Content-Length: 11
  Connection: keep-alive
  POST /up 11HTTP/1.1 200 OK
  Content-Length: 9
  Connection: keep-alive
  HTTP/1.1 200 OK
  Content-Length: 11
  Connection: close
  GET /last 0

客户端：回复是 chunked 的、没有 Content-Length 读到连接关闭的、状态行不对的；https 和连不上的 Err。

  $ cat > client.sn <<'EOF'
  > use std.net
  > use std.http
  > 
  > // 依次接受三个连接，各回一种写法的回复
  > fn fake(l: net.TcpListener) {
  >     var replies: Vec<Str> = Vec.new()
  >     replies.push("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nX-A: 1\r\n\r\n3\r\nabc\r\nA\r\n0123456789\r\n0\r\n\r\n")
  >     replies.push("HTTP/1.0 201 Created\r\n\r\n直到关闭")
  >     replies.push("garbage\r\n\r\n")
  >     for r in replies {
  >         match l.accept() {
  >             Ok(c) => {
  >                 match http.read_request(c) {
  >                     Ok(.Some(req)) => {
  >                         let w = c.write_str(r)
  >                     },
  >                     Ok(.None) => {},
  >                     Err(e) => println("fake：{e}")
  >                 }
  >                 c.close()
  >             },
  >             Err(e) => println(e)
  >         }
  >     }
  > }
  > 
  > fn show(r: Result<http.Response, Str>) {
  >     match r {
  >         Ok(resp) => println("{resp.status} {resp.reason} {resp.header("x-a").unwrap_or("-")} {resp.text_body()}"),
  >         Err(e) => println("Err {e}")
  >     }
  > }
  > 
  > fn main() {
  >     match net.listen("127.0.0.1", 0) {
  >         Ok(l) => {
  >             let base = "http://127.0.0.1:{l.port()}/"
  >             scope |s| {
  >                 s.spawn(|| fake(l))
  >                 show(http.get(base))
  >                 show(http.get(base))
  >                 show(http.get(base))
  >             }
  >             l.close()
  >         },
  >         Err(e) => println(e)
  >     }
  >     show(http.get("https://example.com/"))
  >     match http.get("http://127.0.0.1:1/") {
  >         Ok(_) => println("连上了"),
  >         Err(e) => println("{e.starts_with("connect 127.0.0.1:1：")}")
  >     }
  > }
  > EOF
  $ sinterc run client.sn
  200 OK 1 abc0123456789
  201 Created - 直到关闭
  Err 回复的状态行不对：garbage
  Err std.http 还不支持 https（没有 TLS）：https://example.com/
  true

服务端收到坏的请求：回 400 并关闭连接。

  $ cat > bad.sn <<'EOF'
  > use std.net
  > use std.http { Handler }
  > 
  > struct Ok200 { body: Str }
  > 
  > impl Handler for Ok200 {
  >     fn handle(o: Ok200, req: http.Request) -> http.Response { http.Response.text(200, o.body) }
  > }
  > 
  > fn raw(port: I64, msg: Str) -> Result<Str, Str> {
  >     let c = net.connect("127.0.0.1", port)?
  >     let w = c.write_str(msg)
  >     let first = c.read_line()
  >     c.close()
  >     Ok(first?.unwrap_or("（没有回复）"))
  > }
  > 
  > fn main() {
  >     var msgs: Vec<Str> = Vec.new()
  >     msgs.push("GET /\r\n\r\n")
  >     msgs.push("GET / HTTP/2\r\n\r\n")
  >     msgs.push("GET / HTTP/1.1\r\nno colon here\r\n\r\n")
  >     msgs.push("POST / HTTP/1.1\r\nContent-Length: abc\r\n\r\n")
  >     match net.listen("127.0.0.1", 0) {
  >         Ok(l) => {
  >             let port = l.port()
  >             let n = msgs.len()
  >             let h = Ok200 { body: "ok" }
  >             scope |s| {
  >                 s.spawn(|| http.serve(l, n, h))
  >                 for m in msgs {
  >                     match raw(port, m) {
  >                         Ok(line) => println(line),
  >                         Err(e) => println(e)
  >                     }
  >                 }
  >             }
  >             l.close()
  >         },
  >         Err(e) => println(e)
  >     }
  > }
  > EOF
  $ sinterc run bad.sn
  HTTP/1.1 400 Bad Request
  HTTP/1.1 400 Bad Request
  HTTP/1.1 400 Bad Request
  HTTP/1.1 400 Bad Request
