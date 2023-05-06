import akka.actor._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

// 2 个单例对象简单作为消息使用
case object Poke
case object Feed

// Kid 是一个 Actor，这意味着它将在线程池上的某个线程运行，并从一个队列中读取消息。它将一个接一个处理消息
class Kid extends Actor {
  def receive = {
    case Poke => {
      println("Ow...")
      println("Quit it...")
    }
    case Feed => {
      println("Gurgle...")
      println("Burp...")
    }
    case _ => println("What?")
  }
}

// 引入了外部包 akka，需要使用 sbt run 运行
object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Ready to poke and feed...")
    val system = ActorSystem("HelloSystem")
    val bart = system.actorOf(Props[Kid], name = "bart")
    val lisa = system.actorOf(Props[Kid], name = "lisa")
    bart ! Poke
    lisa ! Poke
    bart ! Feed
    lisa ! Feed
    system.terminate()
  }
}

// 以上程序是并发的，下面 2 次的输出顺序不一样

// Ready to poke and feed...
// Ow...
// Quit it...
// Ow...
// Quit it...
// Gurgle...
// Gurgle...
// Burp...
// Burp...

// Ow...
// Quit it...
// Ow...
// Quit it...
// Gurgle...
// Burp...
// Gurgle...
// Burp...
