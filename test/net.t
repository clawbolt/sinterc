std.net：TCP、UDP、地址类型和域名解析。套接字是 linear 结构体，必须显式 close。
服务端和客户端在同一个程序里，用 scope 里的任务同时运行；端口写 0，由系统选一个空闲的。

  $ cat > tcp.sn <<'EOF'
  > use std.net
  > 
  > fn serve(l: net.TcpListener) {
  >     match l.accept() {
  >         Ok(c) => {
  >             match c.read_line() {
  >                 Ok(.Some(line)) => {
  >                     let r = c.write_str("echo: {line}\n")
  >                 },
  >                 Ok(.None) => println("服务端：连接没有数据"),
  >                 Err(e) => println("服务端：{e}")
  >             }
  >             c.close()
  >         },
  >         Err(e) => println("服务端：{e}")
  >     }
  > }
  > 
  > fn client(port: I64) -> Result<(), Str> {
  >     let c = net.connect("127.0.0.1", port)?
  >     let w = c.write_str("你好\r\n")
  >     c.shutdown()
  >     let reply = c.read_all()
  >     c.close()
  >     match reply.unwrap().decode() {
  >         .Some(s) => print(s),
  >         .None => println("不是 UTF-8")
  >     }
  >     Ok(())
  > }
  > 
  > fn main() {
  >     match net.listen("127.0.0.1", 0) {
  >         Ok(l) => {
  >             let port = l.port()
  >             println("{port > 0}")
  >             scope |s| {
  >                 s.spawn(|| serve(l))
  >                 match client(port) {
  >                     Ok(_) => println("完成"),
  >                     Err(e) => println("客户端：{e}")
  >                 }
  >             }
  >             l.close()
  >         },
  >         Err(e) => println(e)
  >     }
  > }
  > EOF
  $ sinterc run tcp.sn
  true
  echo: 你好
  完成

连接没有在监听的端口：Err。

  $ cat > refused.sn <<'EOF'
  > use std.net
  > 
  > fn main() {
  >     var port = 0
  >     match net.listen("127.0.0.1", 0) {
  >         Ok(l) => {
  >             port = l.port()
  >             l.close()
  >         },
  >         Err(e) => println(e)
  >     }
  >     match net.connect("127.0.0.1", port) {
  >         Ok(c) => {
  >             println("连上了")
  >             c.close()
  >         },
  >         Err(e) => println("{e.starts_with("connect 127.0.0.1:")}")
  >     }
  > }
  > EOF
  $ sinterc run refused.sn
  true

UDP：两个套接字互发一个数据报。linear 值不能用 unwrap 取出（Result.unwrap 的 T 不是 linear），要写 match。

  $ cat > udp.sn <<'EOF'
  > use std.net
  > 
  > fn ping(a: net.UdpSocket, b: net.UdpSocket) {
  >     let to = net.SocketAddr.new("127.0.0.1", b.port()).unwrap()
  >     let r = a.send_to("ping".to_bytes(), to)
  >     match b.recv_from(100) {
  >         Ok((data, from)) => {
  >             println("{data.decode().unwrap()} {from.port == a.port()} {from.ip}")
  >         },
  >         Err(e) => println(e)
  >     }
  > }
  > 
  > fn main() {
  >     match net.bind_udp("127.0.0.1", 0) {
  >         Ok(a) => {
  >             match net.bind_udp("127.0.0.1", 0) {
  >                 Ok(b) => {
  >                     ping(a, b)
  >                     b.close()
  >                 },
  >                 Err(e) => println(e)
  >             }
  >             a.close()
  >         },
  >         Err(e) => println(e)
  >     }
  > }
  > EOF
  $ sinterc run udp.sn
  ping true 127.0.0.1

地址：解析、规范的文本形式、域名解析（IP 本身解析成它自己）。

  $ cat > addr.sn <<'EOF'
  > use std.net
  > 
  > fn show(s: Str) {
  >     match net.SocketAddr.parse(s) {
  >         .Some(a) => println("{a} {a.ip.is_v6()}"),
  >         .None => println("{s}：不是地址")
  >     }
  > }
  > 
  > fn main() {
  >     show("10.0.0.1:80")
  >     show("[::0001]:8080")
  >     show("::1:80")
  >     show("1.2.3:80")
  >     show("1.2.3.4:70000")
  >     let ips = net.lookup("127.0.0.1").unwrap()
  >     println("{ips.len()} {ips[0]}")
  >     match net.lookup("no-such-host.invalid") {
  >         Ok(v) => println("{v.len()}"),
  >         Err(e) => println("{e.starts_with("lookup no-such-host.invalid：")}")
  >     }
  > }
  > EOF
  $ sinterc run addr.sn
  10.0.0.1:80 false
  [::1]:8080 true
  ::1:80：不是地址
  1.2.3:80：不是地址
  1.2.3.4:70000：不是地址
  1 127.0.0.1
  true

忘了关闭连接：编译不过（linear）。

  $ cat > leak.sn <<'EOF'
  > use std.net
  > 
  > fn main() {
  >     let c = net.connect("127.0.0.1", 1)
  >     match c {
  >         Ok(s) => println("连上了"),
  >         Err(e) => println(e)
  >     }
  > }
  > EOF
  $ sinterc leak.sn
  leak.sn:6:18: 错误：s 是 linear 值，作用域结束时还没有被消费
   6 |         Ok(s) => println("连上了"),
     |                  ^
  [1]

很多连接同时进行：服务端每个连接一个任务，客户端也是 200 个任务同时连。任务在少量工作线程上运行，
等套接字时让出；服务端的 scope 在一个任务里（它的 scope_join 也让出）。每个客户端发一个数，收到它的两倍。

  $ cat > many.sn <<'EOF2'
  > use std.net
  > 
  > fn echo(sink c: net.TcpStream) {
  >     match c.read_line() {
  >         Ok(.Some(line)) => {
  >             let n = line.parse_i64().unwrap_or(0)
  >             let r = c.write_str("{n * 2}\n")
  >         },
  >         Ok(.None) => {},
  >         Err(e) => eprintln(e)
  >     }
  >     c.close()
  > }
  > 
  > fn serve(l: net.TcpListener, n: I64) {
  >     scope |s| {
  >         for i in 0..n {
  >             match l.accept() {
  >                 Ok(c) => s.spawn(|| echo(c)),
  >                 Err(e) => eprintln(e)
  >             }
  >         }
  >     }
  > }
  > 
  > fn ask(port: I64, k: I64) -> Result<I64, Str> {
  >     let c = net.connect("127.0.0.1", port)?
  >     let w = c.write_str("{k}\n")
  >     let r = c.read_line()
  >     c.close()
  >     w?
  >     match r? {
  >         .Some(line) => Ok(line.parse_i64().unwrap_or(-1)),
  >         .None => Err("没有回复")
  >     }
  > }
  > 
  > fn client(port: I64, k: I64) {
  >     match ask(port, k) {
  >         Ok(v) => { if v != 2 * k { println("{k} 收到 {v}") } },
  >         Err(e) => println("{k}：{e}")
  >     }
  > }
  > 
  > fn main() {
  >     let n = 200
  >     match net.listen("127.0.0.1", 0) {
  >         Ok(l) => {
  >             let port = l.port()
  >             scope |s| {
  >                 s.spawn(|| serve(l, n))
  >                 for k in 0..n {
  >                     s.spawn(|| client(port, k))
  >                 }
  >             }
  >             l.close()
  >             println("完成")
  >         },
  >         Err(e) => println(e)
  >     }
  > }
  > EOF2
  $ sinterc run many.sn
  完成
