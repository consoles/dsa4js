// 默认情况下集合是不可改变的，每个集合操作都会新建一个新的集合而不是修改旧的集合，
object HelloWorld {
    def main(args: Array[String]): Unit = {
        val animals = Set("lions", "tiger", "bears")
        animals.foreach(println)
        println
        (animals + "cat").foreach(println)
        println
        (animals - "tiger").foreach(println)
        println

        println("并集")
        (animals ++ Set("sea", "air")).foreach(println)
        println("差集")
        (animals -- Set("lions", "air")).foreach(println)
        println("交集")
        (animals & Set("lions", "air")).foreach(println)

        println("映射")
        val map = Map(1 -> "one", 2 -> "two", 3 -> "three")
        println(map(1))
        
        val map2 = map + (4 -> "four")
        println(map2(4))

        val words = List("hello", "a", "hello", "scala")
        println(words.count(word => word.size > 2))
        val list = List(1, 2, 3, 4)
        val sum = (0 /: list) { (sum, i) => sum + i }
        println(sum)
        println(list.foldLeft(0)((sum, value) => sum + value))
    }
}

