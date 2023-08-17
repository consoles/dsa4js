
def isPalindrome(s: String) = s == s.reverse
def findPalindrome(s: Seq[String]) = s find isPalindrome

object HelloWorld {
    def main(args: Array[String]): Unit = {
        val words = "Bob went to Harrah and gambled with Otto and Steve" split " " map(_.toLowerCase)
        println(findPalindrome(words))
    }
}
