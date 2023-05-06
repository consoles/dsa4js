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
