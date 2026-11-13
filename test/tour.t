文档（TRY.md）里的两个一览程序都要能运行。

  $ sinterc run ../examples/tour/tour.sn > /dev/null

  $ printf 'the cat and the dog\nThe 猫 and the 狗, the end\n' | sinterc run ../examples/tour/words.sn
  共 12 个词，7 个不同的词
  the: 5
  and: 2
  cat: 1
  dog: 1
  end: 1

HTTP 服务器的示例（std.net、scope）：selftest 在同一个程序里起服务端，客户端依次发四个请求
（第三个和 curl 一样把中文写成 %XX，服务端解码）。

  $ sinterc run ../examples/tour/http_server.sn selftest
  HTTP/1.1 200 OK | Sinter 的 HTTP 示例
  HTTP/1.1 200 OK | 你好，世界
  HTTP/1.1 200 OK | 你好，世界
  HTTP/1.1 404 Not Found | 没有 /missing
