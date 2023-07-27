# 七周七语言(理解多种编程泛型)

https://e.jd.com/30154717.html

ruby repl 命令：`irb`

## ch2：ruby

## ch3：Io

强类型

[Io](https://iolanguage.org) 拥有大量可定制语法和函数，以及强有力的并发模型。协程，异步套接字，SIMD（单指令多数据流，用一条指令同时操作多条数据，从而实现并行化的技术），相对于传统的线程（每个套接字并发或者非SIMD向量运算时产生）编写的程序快得多，即便程序是 C 编写的。领域：卫星上的路由配置语言，电子游戏的脚本语言，皮克斯动画公司。

占用空间少，嵌入式领域。Io 虚拟机也容易移植到不同的 OS 上

## ch4：Prolog

Programming in logic逻辑编程语言，在自然语言处理领域非常受欢迎。主要有 3 个基本构成单元：

- 事实
- 规则
- 查询

小写字母开头的是原子(atom), 一个类似 Ruby 中 Symbol 的固定值，大写字母或者下划线开头的是变量。

### 环境搭建

[Prolog 语言入门教程](https://ruanyifeng.com/blog/2019/01/prolog.html)

[GNU Prolog](http://gprolog.org/#download), 参考 ch4/day1/friends.pl 编写文件, 点击菜单 File -> Change Dir 到 `D:/dsa4js/app/seven_weeks_seven_langs/ch4`

![运行 Prolog](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/gnu_prolog.png)

上图中的 `['day1/friends.pl'].`, `likes(wallace, sheep).`, `likes(grommit, cheese).` 是我们的输入， yes 和 no 是返回值。

在 friends.pl 中，前 3 行是事实，最后 1 行是规则。事实和规则构成了知识库。事实是我们对这个世界直观观察的结果，规则是关于现实世界的逻辑推论。

```
| ?- friend(wallace, wallace).

no
```

规则 `friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).` 的解释。

`:-` 右边第一部分，被称为子目标(subgoal), `\+` 表示逻辑反。 `\+(X = Y)` 意思是 X 不等于 Y

再做一个查询：

```
| ?- friend(grommit, wallace).

yes
| ?- friend(wallace, grommit).

yes
```

在英语中，如果可以证明 X 喜欢某个 Z 并且 Y 喜欢某个 Z， 那么 X 就是 Y 的朋友。 上面的用例中 wallace 和 grommit 都喜欢 cheese， 所以以上查询都返回 yes。

深入分析一下代码， X 不等于 Y， 满足第一个子目标，查询将使用第二个和第三个子目标 likes(X, Z) 和 likes(Y, Z)。grommit和wallace都喜欢cheese，所以又满足了第二个和第三个子目标。

```
| ?- friend(wendolene, grommit).

no
```

在这个例子中，Prolog会尝试几组可能的X、Y和Z值:
- wendolene、grommit和cheese
- wendolene、grommit和sheep

两种组合都无法同时满足两个目标，即wendolene喜欢Z并且grommit也喜欢Z。因为不存在这样的组合，所以逻辑引擎报告no，即它们不是朋友。

```
friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).
```

上述代码是一个具有三个变量X、Y和Z的Prolog规则。我们把这个规则称作friend/2，即有两个参数的friend规则的缩写。这个规则拥有三个用逗号分隔的子目标。当所有子目标都为真时，这个规则才为真。所以我们这个规则的含义是：如果X与Y不等同且X和Y都喜欢同一个Z，那么X是Y的朋友。

VS code 插件 VSC-Prolog 可以配置好 `prolog.executablePath` 可以对 prolog 语法比较好的支持和 lint 等，能自动识别 `.pro` 文件后缀。

### 缺点

prolog 使用了一种深度优先搜索的决策树，它使用所有可能的组合与规则集合相匹配，并且其编译器对这个过程做了很好的优化。不过，这个策略需要进行大量计算，特别是当数据集规模非常大的时候。这也迫使Prolog用户必须理解语言的工作原理以保持数据集的规模在可控范围内。

## ch5 Scala

基于 Scala 3.1.3 (19.0.2, Java Java HotSpot(TM) 64-Bit Server VM)

`Nil` 的类型 `scala.collection.immutable.Nil.type = List()`

当一个类只有一个实例的时候可以使用 `object` 而不是 `class` 关键字来定义这个类, 这就是单例。

`Nothing` 类时所有类的子类，`Any` 是所有类的父类。`Null` 是一个 trait， null 则是 Null 的一个实例。一个空集合是 Nil，而 Nothing 是一个 trait，是所有类的子类。Nothing 类没有实例，所以不能像 Null 那样对其解引用。例如抛出异常的方法的返回值是 Nothing，意思是根本没有返回值。

柯里化(currying):将一个带有多个参数的函数转换为多个拥有独自参数列表的函数

scala 并发主要包括 actor 和消息传递。actor 拥有线程池和队列池。当发送一条消息给 actor 的时候（使用操作符 !），是将一个对象放到该 actor 队列中。actor 读取消息并采取行动，通常情况下，actor通过模式匹配器去检测消息并执行相应的消息处理。

## ch6 Erlang

Erlang 是并发和容错的代名词

需要将 erlang 的安装路径加到 PATH 环境变量中，可以使用 `erl` 打开 repl

产品：CouchDB

Erlang是一门函数式语言，可靠性方面的特性很多，可用于开发可靠性要求极高的系统。Erlang 在替换模块时不必停止运行，这样就能边运行边维护电话交换机等设备。有些使用Erlang的系统已持续运行多年，从未因维护而中止过。可话说回来，要说到Erlang最关键的功能，那还得是并发。

Erlang 是轻量级进程，简化了应用程序中多进程创建、管理和通信的过程。分布式消息传递称为基本的语言结构，因此锁机制也不再必要，并发性能得到提升。和 Scala 一样，Erlang 也将 actor 用在了并发当中，Scala 的 actor 代表一个对象，由线程池创建和维护，而 Erlang 的 actor 代表一个轻量级进程，从队列中读取外部进入的消息，并用模式匹配决定其处理方式。

Erlang 虽然也有常规错误检测手段，但在容错应用中，需要处理的错误加起来远比传统应用要多，这是常规手段无法解决的。Erlang解决这一问题的秘诀是“就让它崩溃”。由于Erlang能轻易监测到崩溃进程，因此终止相关进程并启动新进程也就不在话下了。

变量以大写字母开头，原子以小写字母开头，原子不能被赋值，变量也只能赋值 1 次。

等号不是简单的赋值，而是模式匹配。erlang 是基于 prolog 的语法规则。

[OTP（Open Telecom Platform）](https://erlang.org/doc/apps/otp/lib/otp-24.0/doc/html/index.html)是 Erlang 的一个库和框架集合，它提供了一系列常用的工具、库和应用程序，用于构建高可用性、高并发性、分布式和容错性系统。

OTP 库提供了一些重要的功能，如进程管理、错误处理、日志记录、消息传递、持久性、并发控制、代码升级等。使用 OTP 库可以大大简化 Erlang 开发人员处理常见问题的工作，并帮助他们编写更可靠、可维护和可扩展的 Erlang 应用程序。
OTP 库是 Erlang 的一个核心特性，Erlang 开发人员通常使用它来构建各种类型的系统，包括电信、金融、医疗和互联网应用程序等。

erlang 3 种并发原语：

- 用 `!` 发送消息
- 用 `spawn` 产生进程
- 用 `receive` 接收消息

## ch7 Clojure

JVM 上基于Lisp 的实现。Lisp 是一种列表语言，有以下特性：

- Lisp 是一种列表语言。函数调用时，取列表第一个元素作函数，列表其余元素作参数。
- Lisp 使用自有数据结构表示代码。称之为数据即代码（data as code）。

结合这两种特性，所得到是一种特别适合元编程的语言。可以把代码组织成像类里的方法一样。再用这些对象构成树，就能得到一个基本的对象模型。也可以构造一个基于原型的代码组织结构，其中为数据和行为预留下扩展槽。还可以构造出一个纯函数式实现。正是这种灵活性让 Lisp 几乎能支持任何编程范型。

Clojure 使用 STM(Software Transactional Memory) 并发。如果想要修改一个引用的状态，必须在事务内进行操作。

同步情况下，原子和引用都是处理可变状态既简单又安全的方法。和 atom 一样，代理也是封装起来的一份数据。和 Io 中的 future 相似，解引用后的代理会一直阻塞到直到有值可用。使用者可以用函数异步修改数据，而更新会在另一个线程中发生。每次只有一个函数能修改代理的状态。

从引用、代理、或者原子中读取值永远都不会锁定也永远不会阻塞。Clojure 涉及使用快照，其值是瞬时的，并可能立即过期，这正是版本数据库进行快速并发控制的方式。

[leiningen](https://codeberg.org/leiningen/leiningen) 是一个管理 Clojure 程序和 java 配置的工具集。`lein repl` 可以进入 REPL 环境。

宏读取器：

- `;` 表示注释
- `'` 表示引用
- `#` 表示匿名函数

Clojure 中有一个基本数据类型叫做 ratio，它支持延迟计算以避免精度损失。

```clojure
user=> (+ 1 1)
2
user=> (- 1)
-1
user=> (/ 1 6)
1/6
user=> (/ 2 12)
1/6
user=> (/ 2.0 12)
0.16666666666666666
user=> (class (/ 2.0 12))
java.lang.Double
user=> (class (/ 2 12))
clojure.lang.Ratio
user=> (mod 5 4)
1
user=> (mod 4 4)
0
user=> (mod 4 5)
4
```

前缀表示法可以实现下面的效果：

```clojure
user=> (< 1 2 3)
true
user=> (< 1 3 2 4)
false
user=>
```

对于任意数量参数组成的列表，只需要一个简单的操作符就能知道它是否已经排序。

在 Clojure 中习惯将 List 用作代码，将 Vector 作为数据。
以冒号开头的词是关键字，类似ruby中的符号或者erlang中的原子。

### List

```clojure
user=> (1 2 3)
Execution error (ClassCastException) at user/eval2118 (REPL:1).
class java.lang.Long cannot be cast to class clojure.lang.IFn (java.lang.Long is in module java.base of loader 'bootstrap'; clojure.lang.IFn is in unnamed module of loader 'app')

user=> (list 1 2 3)
(1 2 3)
user=> '(1 2 3)
(1 2 3)
user=>

user=> (first '(:r2d2 :c3po))
:r2d2
user=> (last '(:r2d2 :c3po))
:c3po
user=> (rest '(:r2d2 :c3po))
(:c3po)
user=> (cons :battle-droid '(:r2d2 :c3po))
(:battle-droid :r2d2 :c3po)
user=>
```

### Vector

向量和列表一样，是元素的有序集合，向量是为随机访问优化的，用方括号表示。

```clojure
user=> [:hutt :wookie :ewok]
[:hutt :wookie :ewok]
user=> (first [:hutt :wookie :ewok])
:hutt
user=> (nth [:hutt :wookie :ewok] 0)
:hutt
user=> (nth [:hutt :wookie :ewok] 2)
:ewok
user=> (last [:hutt :wookie :ewok])
:ewok
user=> ([:hutt :wookie :ewok] 2)
:ewok
user=>  (rest [:hutt :wookie :ewok])
(:wookie :ewok)
user=>
```

向量也是函数，取下标作为参数，可以合并 2 个向量：

```clojure
user=> (concat [:apple] [:orange])
(:apple :orange)
```

### Set

Set 使用 `#{}` 包括起来

```clojure
user=>  #{:a :b :c}
#{:c :b :a}
user=> (def data #{:a :b :c})
#'user/data
user=> data
#{:c :b :a}
user=> (count data)
3
user=> (sort data)
(:a :b :c)
user=> (sorted-set 2 3 1 4)
#{1 2 3 4}
user=>

user=> (clojure.set/union #{:a} #{:b})
#{:b :a}
user=> (clojure.set/difference #{:a :b :c} #{:b})
#{:c :a}
user=>
```

set 也是函数, `#{:a :b :c} :a` 用于判断集合 `{:a, :b, :c}` 是否包含 `:a`

```clojure
user=> (#{:a :b :c} :a)
:a
user=> (#{:a :b :c} :d)
nil
user=>
user=>
```

### Map

```clojure
user=> {:a :aa :b :bb :c :cc}
{:a :aa, :b :bb, :c :cc}
user=>
```

缺点是很难看清楚，如果少写了值，也不容易看出来，导致报错：

```clojure
user=> {:a :aa :b :bb :c}
Syntax error reading source at (REPL:1:19).
Map literal must contain an even number of forms

user=>
```

可以使用逗号作为空白符来分隔：

```clojure
user=> {:a :aa, :b :bb, :c :cc}
{:a :aa, :b :bb, :c :cc}
user=>
```

映射本身也是函数，

```clojure
user=> (def person {:name "张三", :age 10})
#'user/person
user=> (person :name)
"张三"
user=> (:name person)
"张三"
user=> (merge {:name "张三", :age 10} {:sex "男"})
{:name "张三", :age 10, :sex "男"}
user=> (sorted-map 1 :one, 3 :three, 2 :two)
{1 :one, 2 :two, 3 :three}
user=>
```

### 函数

定义函数：

```clojure
user=> (defn hello [name] (str "Hello, " name))
#'user/hello
user=> (hello "张三")
"Hello, 张三"
user=>
```

可以使用 `doc` 命令查看函数的文档：

```clojure
user=> (doc str)
-------------------------
clojure.core/str
([] [x] [x & ys])
  With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args.
nil
user=>
```

#### 参数的绑定与解构

```clojure
user=> (def board [[:x :o :x] [:o :x :o] [:o :x :o]])
#'user/board
user=> (defn center [[_ [_ c _] _]] c)
#'user/center
user=> (center board)
:x
user=>
```

#### 匿名函数

```clojure
user=> (def people ["Lea", "Jack Scott"])
#'user/people
user=> (count "Jack Scott")
10
user=> (map count people)
(3 10)
user=> (map (fn [w] (* 2 (count w))) people)
(6 20)
user=> (map #(* 2 (count %)) people)
(6 20)
user=>
```

计算 2 倍名字的长度 `(map (fn [w] (* 2 (count w))) people)` 就是匿名函数，这种方式还能简写：`(map #(* 2 (count %)) people)`，使用 `#` 定义匿名函数，而 `%` 被绑定到序列的每个元素上，`#` 被称作*宏读取器*（reader macro）。

```clojure
user=> (def v [3 1 2])
#'user/v
user=> (apply + v)
6
user=> (apply max v)
3
user=> (filter odd? v)
(3 1)
user=> (filter #(< % 3) v)
(1 2)
```

## ch8 Haskell

Haskell在进行正确性证明上比命令式语言容易得多。

编译器 [GHC](https://www.haskell.org/ghc/), 使用 `ghci` 可以打开 REPL，也可以用 `runhaskell *.hs` 命令直接运行源码。

```
ghci> :set +t
ghci> 5
5
it :: Num a => a
ghci> 5.0
5.0
it :: Fractional a => a
ghci> "Hello"
"Hello"
it :: String
ghci> 5 == (2+3)
True
it :: Bool
ghci> :t 5
5 :: Num a => a
ghci>
```

函数：

```
PS D:\dsa4js\app\seven_weeks_seven_langs\ch7\day3> ghci
GHCi, version 9.2.8: https://www.haskell.org/ghc/  :? for help
ghci> let x = 10
ghci> double x = x * 2
ghci> double 2
4
```
