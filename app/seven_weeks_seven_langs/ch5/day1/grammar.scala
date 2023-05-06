// 只有属性但是没有方法和构造器的简单类
class Person(firstName: String, lastName: String)

// 一个完整的面向对象的类
class Compass {
    val directions = List("N", "E", "S", "W")
    var bearing = 0
    print("Initial bearing: ")
    println(directions)

    def direction() = directions(bearing)
    def inform(turnDirection: String) = {
        println("Turing " + turnDirection + ". Now bearing " + direction())
    }
    def turnRight() = {
        bearing = (bearing + 1) % directions.size
        inform("right")
    }
    def turnLeft() = {
        bearing = (bearing - 1 + directions.size) % directions.size
        inform("left")
    }
}

// 辅助构造器
class Person2(firstName: String) {
    println("Outer constructor")
    def this(firstName: String, lastName: String) = {
        this(firstName)
        println("Inner constructor")
    }
    def talk() = println("Hi")
}

// 当一个类只有一个实例的时候可以使用 `object` 而不是 `class` 关键字来定义这个类。
// 单例
object TrueRing {
    def rule = println("To Rule them all")
}

class Person3(val name : String) {
    def talk(message: String) = println(name + " says " + message)
    def id(): String = name
}

class Employee(override val name: String, val number: Int) extends Person3(name) {
    override def talk(message: String) = {
        println(name + " with number " + number + " says " + message)
    }
    override def id(): String = number.toString
}

// 部分类
class Person4(val name: String)
trait Nice {
    def greet() = println("Howdily doorily.")
}
class Character(override val name: String) extends Person4(name) with Nice 

object HelloWorld {
    def main(args: Array[String]): Unit = {
        println("Hello World")
        // range
        println("==============range===========")
        val range = 0 until 10
        println(range.start)
        println(range.end)
        println(range.step)

        val range2 = (0 to 10) by 5
        println(range2)

        val range3 = (0 until 10 by 5)
        println(range3)

        val range4 = (10 until 0 by -1)
        println(range4.start)
        println(range4.end)
        println(range4.step)
        println(range4)

        val range5 = (10 until 0)
        println(range5) // empty Range 10 until 0, 方向无法被推断
        val range6 = (0 until 10)
        println(range6)

        val range7 = 'a' to 'e'
        println(range7) // NumericRange a to e, 隐式类型转换，实际上指定了一个 for 语句也就是指定了一个 range

        // 元组
        println("==============元组===========")
        val person = ("Elvis", "Presley")
        println(person._1)
        println(person._2)
        val (x, y) = (1, 2)
        println(x + y)

        // 类
        println("==============类===========")
        val gump = new Person("Scott", "Jack")
        println("简单类:" + gump)
        println("一个完整的面向对象的类：")
        val compass = new Compass
        compass.turnRight()
        compass.turnRight()
        compass.turnLeft()
        compass.turnLeft()
        compass.turnLeft()

        val bob = new Person2("Bob")
        val bobTate = new Person2("Bob", "Tate")

        // 拓展类
        println("==============拓展类===========")
        TrueRing.rule
        val employee = new Employee("Yoda", 4)
        employee.talk("Extend or extend not. There is no try.")
        val flanders = new Character("Ned")
        flanders.greet()
    }
}
