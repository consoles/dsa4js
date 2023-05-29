import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConverters._
import scala.io.Source
import org.jsoup.Jsoup
import java.net.URL

// 加分题：让 sizer 跟踪给定页面上的所有链接并加载它们。
// 例如，对给定页面“google.com”，sizer会计算出 Google 主页及主页所链接的页面的数量总和

object Sizer {

    var v = Set[String]() 

    def visit(urls: Set[String], depth: Int): Future[List[(String, Int, Int, Int)]] = {
        val futures = for {
            url <- urls
        } yield Future {
            if (v.contains(url)) {
                (url, 0, 0, 0)
            } else {
                try {
                    v += url
                    val doc = Jsoup.connect(url).get() // 加上try catch
                    val links = doc.select("a[href]").asScala.map(href => {
                        val url = href.attr("abs:href")
                        // new URL(url).getProtocol + "://" + new URL(url).getHost
                        url
                    }).filter(_.startsWith("http")).toSet
                    println(s"$url => $links depth: $depth")
                    if (depth > 1 || urls.size == 0) {
                        (url, 0, 0, 0)
                    } else {
                        val r = visit(links, depth + 1)
                        val subLinkResults = Await.result(r, Duration.Inf)
                        var subLinkTotalSize = 0
                        subLinkResults.foreach {
                            case (url, length, linkSize, subLinkSize) => subLinkTotalSize += linkSize
                        }
                        (url, doc.body().text().length, links.size, subLinkTotalSize)
                    }
                } catch {
                    case e: Exception =>
                        println(s"Error loading $url: ${e.getMessage}")
                        (url, 0, 0, 0)
                }
            }    
        }
        Future.sequence(futures.toList)
    }

    def main(args: Array[String]): Unit = {
        val urls = Set(
            "https://www.google.com",
            "https://jestjs.io",
            "https://www.bohaishibei.com",
            "https://docs.kalix.io",
            "https://www.github.com"
        )

        var startTime = System.nanoTime
        val results = Await.result(visit(urls, 0), Duration.Inf)
        results.foreach {
            case (url, length, linkSize, subLinkSize) => println(s"concurrent: Length of $url: $length, linkSize: $linkSize, subLinkSize: $subLinkSize")
        }
        var endTime = System.nanoTime
        println(s"concurrent Total time: ${(endTime - startTime) / 1000000} ms")
    }
}

// sbt run
// concurrent: Length of https://www.google.com: 159, linkSize: 18, subLinkSize: 290
// concurrent: Length of https://docs.kalix.io: 4297, linkSize: 133, subLinkSize: 1241
// concurrent: Length of https://jestjs.io: 3324, linkSize: 149, subLinkSize: 1412
// concurrent: Length of https://www.bohaishibei.com: 7150, linkSize: 76, subLinkSize: 1133
// concurrent: Length of https://www.github.com: 7423, linkSize: 84, subLinkSize: 1392
// concurrent Total time: 1627841 ms
// [success] Total time: 1631 s (27:11), completed 2023骞?鏈?鏃?18:08:37
