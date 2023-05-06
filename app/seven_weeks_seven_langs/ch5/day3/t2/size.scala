import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

object Sizer {
    def main(args: Array[String]): Unit = {
        val urls = List(
            "https://www.google.com",
            "https://www.yahoo.com",
            "https://www.msn.com",
            "https://www.bing.com",
            "https://www.cnn.com"
        )

        var startTime = System.nanoTime
        val futures = for {
            url <- urls
        } yield Future {
            (url, Source.fromURL(url).mkString.length)
        }
        val results = Await.result(Future.sequence(futures), Duration.Inf)
        results.foreach {
            case (url, length) => println(s"concurrent: Length of $url: $length")
        }
        var endTime = System.nanoTime
        println(s"concurrent Total time: ${(endTime - startTime) / 1000000} ms")

        startTime = System.nanoTime
        for(url <- urls) {
            println(s"sequential: Length of $url: ${Source.fromURL(url).mkString.length}")
        }
        endTime = System.nanoTime
        println(s"sequential Total time: ${(endTime - startTime) / 1000000} ms")
    }
}

// scala sizer.scala
// concurrent: Length of https://www.google.com: 16032
// concurrent: Length of https://www.yahoo.com: 3125
// concurrent: Length of https://www.msn.com: 49246
// concurrent: Length of https://www.bing.com: 103677
// concurrent: Length of https://www.cnn.com: 1984453
// concurrent Total time: 3797 ms
// sequential: Length of https://www.google.com: 16052
// sequential: Length of https://www.yahoo.com: 3125
// sequential: Length of https://www.msn.com: 49498
// sequential: Length of https://www.bing.com: 103677
// sequential: Length of https://www.cnn.com: 1984453
// sequential Total time: 8625 ms
