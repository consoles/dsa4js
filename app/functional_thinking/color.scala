class Color(val red:Int, val green:Int, val blue:Int)

case class Red(r:Int) extends Color(r,0,0)
case class Green(g:Int) extends Color(0,g,0)
case class Blue(b:Int) extends Color(0,0,b)

def printColor(c:Color) = c match {
    case Red(r) => println("Red")
    case Green(g) => println("Green")
    case Blue(b) => println("Blue")
    case color:Color => {
        print("R: " + color.red + ", ")
        print("G: " + color.green + ", ")
        println("B: " + color.blue)
    }
    case null => println("Unknown color")
}

// R: 1, G: 2, B: 3
// Red
// Green
// Unknown color
object HelloWorld {
    def main(args: Array[String]): Unit = {
        printColor(new Color(1,2,3))
        printColor(Red(111))
        printColor(Green(111))
        printColor(null)
    }
}
