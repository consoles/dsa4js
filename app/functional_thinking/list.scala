val numbers = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)

// 遍历打印列表
def iterateList(list: List[Int]): Unit = {
    for (i <- list) {
        println(i)
    }
}

// 以递归的方式进行列表遍历
def iterateListRecursively(list: List[Int]): Unit = {
    list match {
        case Nil => return
        case head :: tail => {
            println(head)
            iterateListRecursively(tail)
        }
    }
}

// 命令式的从列表中过滤元素
def filterList(list: List[Int], predicate: Int => Boolean): List[Int] = {
    // list.filter(predicate)
    var result = List[Int]()
    for (i <- list) {
        if (predicate(i)) {
            result = i :: result
        }
    }
    result.reverse
}

// 递归的方式编写筛选函数
def filterListRecursively(list: List[Int], predicate: Int => Boolean): List[Int] = {
    list match {
        case Nil => return list
        case head :: tail => {
            if (predicate(head)) {
                head :: filterListRecursively(tail, predicate)
            } else {
                filterListRecursively(tail, predicate)
            }
        }
    }
}

object HelloWorld {
    def main(args: Array[String]): Unit = {
        iterateList(numbers)
        println("==============")
        iterateListRecursively(numbers)
        println("==============")
        println(filterList(numbers, _ % 2 == 0))
        println("==============")
        println(filterListRecursively(numbers, _ % 2 == 0))
    }
}
