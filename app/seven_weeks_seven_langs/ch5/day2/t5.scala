// 从一个文件中加载脏话或它们的替代品

import scala.io.Source

trait Censor {
  val replacements = loadReplacements()

  def loadReplacements(): Map[String, String] = {
    val source = Source.fromFile("replacements.txt")
    val lines = source.getLines().filterNot(_.startsWith("#")).toSeq
    source.close()
    lines.map(_.split("->").map(_.trim)).collect { case Array(dirty, clean) => dirty -> clean }.toMap
  }

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

