// 使用foldLeft方法计算一个列表中所有字符串的总长度

object Hello {
    def main(args: Array[String]): Unit = {
        val words = List("hello", "a", "hello", "scala")
        println(words.foldLeft(0)((sum, value) => sum + value.length))
    }
}
