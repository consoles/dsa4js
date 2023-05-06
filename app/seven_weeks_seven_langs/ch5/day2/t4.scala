// 编写一个Censor trait，包含一个可将Pucky和Beans替换为Shoot和Darn的方法
// 使用映射存储脏话和它们的替代品

trait Censor {
  val replacements = Map(
    "Pucky" -> "Shoot",
    "Beans" -> "Darn"
  )

  def replaceWords(text: String): String = {
    replacements.foldLeft(text) {
      case (str, (dirty, clean)) => str.replaceAllLiterally(dirty, clean)
    }
  }
}

class MyText extends Censor {
  val text = "I don't like Pucky and Beans!"
  val cleanText = replaceWords(text)
  println(cleanText) // 输出 "I don't like Shoot and Darn!"
}

object HelloWorld {
  def main(args: Array[String]): Unit = {
    val text = new MyText
  }
}
