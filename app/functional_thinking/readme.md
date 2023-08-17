[函数式编程思维](https://m.douban.com/book/subject/26587213/)

https://github.com/oreillymedia/functional_thinking

> 语法不过是一些小细节罢了，真正考验人的是用另一种方式去思考。
> 很多时候，再优秀的想法也得等待技术基础慢慢成熟。

函数式编程语言提供了2个方法 `foldLeft` 和 `foldRight`。由于加法满足交换律，`foldLeft` 和 `foldRight` 将得到相同的结果，但是减法和除法并不是这样。左折叠不支持无限序列，而右折叠支持无限序列。

# 列表

在类C语言中，列表通常会被塑造成一个带索引的集合，从这个角度我们可以很容易实现迭代，甚至代码中不需要明确用到索引。

```scala
val numbers = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)

// 遍历打印列表
def iterateList(list: List[Int]): Unit = {
    for (i <- list) {
        println(i)
    }
}

object HelloWorld {
    def main(args: Array[String]): Unit = {
        iterateList(numbers)
    }
}
```

`iterateList` 方法虽然没有直接用到索引，但是我们的思维中还是把集合想象成了一排带编号的格子。对于函数式编程语言来说，他们看到的不是带有索引的格子，而是看成列表由第一个元素(头部)和列表的其余元素（尾部）两部分组成。

```scala
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

object HelloWorld {
    def main(args: Array[String]): Unit = {
        iterateList(numbers)
        println("==============")
        iterateListRecursively(numbers)
    }
}
```

筛选函数：

```scala
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
```

# 函数缓存

```groovy
package test

class ClassifierMemoized {
    def static dividesBy = { a, b -> a % b == 0 }
    def static isFactor = dividesBy.memoize()

    def static factorsOf(number) {
        (1..number).findAll { i -> isFactor.call(number, i) }
    }

    def static sumFactors = { number -> 
        factorsOf(number).inject(0) { sum, i -> sum + i }
    }
    // 可以指定最大缓存数量，让语言本身替我们管理缓存
    def static sumOfFactors = sumFactors.memoizeAtMost(1000)

    def static isPrefect(number) {
        def success = sumFactors(number) == 2 * number
        // println "number: $number, success: $success"
        return success
    }

    def static isPrefectWithMemoize(number) {
        sumOfFactors(number) == 2 * number
    }
}

class Timer {
    long startTime
    String label

    def start(label) {
        this.label = label
        this.startTime = System.currentTimeMillis()
    }

    def stop() {
        def endTime = System.currentTimeMillis()
        println "$label: ${endTime - this.startTime} ms"
    }
}

range = 1..1000

timer = new Timer()
timer.start("寻找1-1000之间的完美数(不使用缓存)#1")
range.each { ClassifierMemoized.isPrefect(it) }
timer.stop()
timer.start("寻找1-1000之间的完美数(不使用缓存)#2")
range.each { ClassifierMemoized.isPrefect(it) }
timer.stop()

timer.start("寻找1-1000之间的完美数(使用缓存)#1")
range.each { ClassifierMemoized.isPrefectWithMemoize(it) }
timer.stop()
timer.start("寻找1-1000之间的完美数(使用缓存)#2")
range.each { ClassifierMemoized.isPrefectWithMemoize(it) }
timer.stop()

// 寻找1-1000之间的完美数(不使用缓存)#1: 867 ms
// 寻找1-1000之间的完美数(不使用缓存)#2: 535 ms
// 寻找1-1000之间的完美数(使用缓存)#1: 292 ms
// 寻找1-1000之间的完美数(使用缓存)#2: 4 ms
```

在命令式思路下，开发者是代码的主人（以及一切责任的承担者）。而函数式语言的思路是，为了操纵一些标准的构造，我们来制作一种通用的组件，有时候还在组件上设置若干调节旋钮（也就是函数的不同变体和参数的不同组合）。函数式语言的基本元素，因此在函数层面上的优化会附带产生功能的提升。实际上我们手动创建的缓存不可能比语言设计者更高效，因为语言设计者可以无视他们给语言定义的规矩，可以做更底层的优化。我们将缓存等问题交给语言，不仅仅是因为他们效率更高，更是因为我们可以站在更高的抽象层次上看待问题。

> 语言设计者实现出来的机制总是比开发者自己做的效率更高，因为他们可以不受语言本身的限制。

手工建立缓存的工作不算复杂，但它给代码增加了状态的影响和额外的复杂性。而借助函数式语言的特性，例如记忆，我们可以在函数的级别上完成缓存工作，只需要微不足道的改动，就能取得比命令式做法更好的效果。在函数式编程消除了不确定因素之后，我们得以专注解决真正的问题。

# 缓求值

```clojure
(defn palindrome? [s]
    (let [sl (.toLowerCase s)]
    (= sl (apply str (reverse sl)))))

(defn find-palindromes [s]
    (filter palindrome? (clojure.string/split s #" ")))

;; (anna)
(println (find-palindromes "The quick brown fox jumped over anna the dog"))
;; (Bob Harrah Otto)
(println (find-palindromes "Bob went to Harrah and gambled with Otto and Steve"))
;; 缓求值序列，只从序列中取出 1 个元素
;; (Bob)
(println (take 1 (find-palindromes "Bob went to Harrah and gambled with Otto and Steve")))
```

Clojure 的数据结构默认都是缓求值的。Scala 实现缓求值特性的手法略有不同。它没有把一切都默认安排成缓求值的，而是在集合之上另外提供了一层缓求值的视图。

Scala 只要在 val 声明前面加上`lazy` 字样，就可以令字段从严格求值变成按需要求值：

```scala
lazy val x = timeConsumingAndOrSizableComputation()
// 实际上是下面方法的语法糖
var _x = None
def x = if (_x.isDefined) _x.get else {
    _x = Some(timeConsumingAndOrSizableComputation())
    _x.get
}
```

Groovy 也提供了效果差不多的便利语法，不过它是通过一种高级语言特性抽象语法树变换来实现的。这种特性允许我们操作编译器产生的内部结构抽象语法树（AST），在很基础的层次上实施变换。Groovy 预定义了若干变换，其中就有 `@Lazy` 标注

# Clojure 多重方法

Clojure 承载多态语义的多重方法（multimethod）特性允许开发者使用任意特征（及其组合）来触发分发。Clojure 习惯用 struct 来放置数据，struct 有点像一个只有数据部分的类。如下所示：

```clojure
(defstruct color :red :green :blue)

(defn red [v]
    (struct color v 0 0))

(defn green [v]
    (struct color 0 v 0))

(defn blue [v]
    (struct color 0 0 v))

;;  定义一个多重方法
;; 首先定义了 basic-colors-in 分发函数，它用一个 vector 结构返回所有非零的颜色分量
(defn basic-clolor-in [color]
    (for [[k v] color :when (not= v 0)] k))

(defmulti color-string basic-clolor-in)

;; 针对单色的几种特殊处理
(defmethod color-string [:red] [color]
    (str "Red: " (:red color)))

(defmethod color-string [:green] [color]
    (str "Green: " (:green color)))

(defmethod color-string [:blue] [color]
    (str "Blue: " (:blue color)))

(defmethod color-string :default [color]
    (str "Red: " (:red color) ", Green: " (:green color) ", Blue: " (:blue color)))


;; 如果调用时传入的参数是单色，该多重方法将执行对应的单色版本。
;; 如果我们传入复合的颜色，则会触发默认方法，由它返回所有的颜色分量值。 

;; 测试 3 种纯色
(println (color-string (struct color 5 0 0))) ;; Red: 5
(println (color-string (struct color 0 12 0))) ;; Green: 12
(println (color-string (struct color 0 0 66))) ;; Blue: 66

;; 测试复合颜色
(println (color-string (struct color 5 10 2))) ;; Red: 5, Green: 10, Blue: 2
```

Clojure 的多重方法是一种特别的方法定义形式，它的参数是一个返回判断条件的分发函数。后续定义的一系列同名方法分别对应到不同的分发条件。切断多态和继承之间的耦合关系，催生了一种强大的、灵活周全的分发机制。这样的分发机制能够处理相当复杂的情况，例如不同图像文件格式的分发问题，每种格式类型都是由各不相同的一组特征来定义的。多重方法赋予了 Clojure 构造强大分发机制的能力，其适应性不输于 Java 的多态，而且限制更少。

# Either 类

不同于 java 中的 try-catch 的异常传播机制，函数式风格中错误作为返回值是更加友好的做法。Either 分为左值和右值，左值封装异常，右值封装结果。
