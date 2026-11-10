文档（TRY.md）里的两个一览程序都要能运行。

  $ sinterc run ../examples/tour/tour.sn > /dev/null

  $ printf 'the cat and the dog\nThe 猫 and the 狗, the end\n' | sinterc run ../examples/tour/words.sn
  共 12 个词，7 个不同的词
  the: 5
  and: 2
  cat: 1
  dog: 1
  end: 1
