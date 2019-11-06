# dsa4js

> 常见算法的javascript实现

## Usage

```bash
$ git clone https://github.com/consoles/dsa4js.git
$ cd dsa4js && npm install
$ mocha
```

## 排序

[各种排序算法](http://wwwlgis.informatik.uni-kl.de/archiv/wwwdvs.informatik.uni-kl.de/courses/DBSREAL/SS2005/Vorlesungsunterlagen/Implementing_Sorting.pdf)

参见sort.js

### 选择排序

找到数组中的最小的那个元素，其次将它和数组的第一个元素进行交换（如果第一个元素本身就是最小的元素，那么它将和自己进行交换）。再次，在剩下的元素中找到最小的元素，将它和数组的第二个元素交换位置。如此反复，直到整个数组有序。核心思想：

> 不断在剩余元素中找到最小者。当前的索引将数组分成了2部分，左边是有序数组，右边是待排序。

大约需要N^2/2次比较和N次交换

总体来说：

- 它是一种*输入无关*的算法。为了找出最小元素而扫描一遍数组并不能为下一遍扫描提供什么信息。也就是说：一个完全有序的数组和顺序混乱的数组没有任何区别！其他排序算法更加善于利用输入的初始状态。
- *交换的次数最少*。交换的次数和数组的规模线性相关。

```js
// 比较的次数为(n-1-0) + (n-1-1) + (n-1-2) + ... + (n-1-(n-1))
// n(n-1) - n(n-1) / 2 = n(n-1) / 2
let selectionSort = arr => {
	for (let i = 0; i < arr.length; i++) {
		let minIndex = i
		// 比较的次数为n-1-i次
		for (let j = i + 1; j < arr.length; j++)
			if (arr[j] < arr[minIndex])
				minIndex = j
		// swap函数调用了n次（0~n-1）		
		swap(arr, i, minIndex)
	}
	return arr
}
```

选择排序是不稳定的，原因如下：

有数组1,5,5,2。则第一个5会和最后的一个2交换，原先的第一个5变成了"第二个5"，5的相对位置发生了变化。因为选择排序的元素的交换发生在*不相邻*的元素之间，而插入排序之所以有序是因为*交换发生在相邻元素间*。

### 插入排序

> 生活中一个形象的例子是整理桥牌：一张一张地来将每一张牌插入到其他已经有序的牌中的适当位置。在计算机的实现中为了给更小的元素腾出空间，我们需要将其余所有的元素都向右移动一位。

和选择排序类似，当前索引将数组分成了有序和无序两部分，但是有序数组的最终位置还不确定，为了给更小的元素腾出空间，它们可能被移动。当索引到达数组的最右边的时候，整个数组就有序了。核心思想：

> 不断将元素插入到已经有序的数组的适当位置。

- 效率严重取决于*数组的初始值*。一个很大的有序（或接近有序）数组的效率将比随机的数组或者逆序数组快得多
- 插入排序非常适合于处理数组中只有几个元素的位置不正确的情况。*当倒置的元素很小的时候可能是最好的算法*。

```js
// 对于1~N-1之间的每一个i，将a[i]和a[0]~a[i-1]中比它小的元素依次有序交换
// 在索引i从左向右的过程中，它左侧的元素总是有序的，所以当i到达最右端的时候排序自然完成了
let insertSort = arr => {
	for (let i = 1; i < arr.length; i++)
		// insert a[i] into arr[i-1],arr[i-2],arr[i-3],...,arr[0]
		for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--)
			swap(arr, j, j - 1)
	return arr
}
```

将内循环中较大元素都向右移动而不是交换两个元素可以将数组访问的次数减半从而提升速度。

对于随机排列的数组，平均需要N^2/4次比较N^2/4次交换。最坏情况下需要N^2/2次比较和N^2/2次交换，最好情况下需要N-1次比较和0次交换。（平均情况下每个元素需要地洞半个数组的长度）

逆序对的数量可以衡量数组的有序程度，如果逆序对的数量小于数组大小的某个倍数，我们就称为这个数组是部分有序的。参见reverse-pair.js。典型的部分有序数组：

1. 数组中每个元素距离它最终的位置都不远
2. 一个有序的大数组接一个小数组
3. 数组中只有几个元素的位置不正确

插入排序实际上是依次减少逆序对的过程。

插入排序对上面的数组非常高效。

> 如果数组逆序，则选择排序的性能高于插入排序，因为插入排序比较和交换的次数都是N(N-1) / 2，选择排序的比较次数为N(N-1) / 2，交换次数为N。

#### 无需交换的插入排序

```js
// https://algs4.cs.princeton.edu/21elementary/InsertionX.java.html
const insertSortNoSwap = arr => {
    const n = arr.length;

    // 从后向前扫描，参考2.1.24中的哨兵，依次将最小的元素放在最开始的位置
    let exchanges = 0;
    for (let i = n - 1; i > 0; i--) {
        if (arr[i] < arr[i - 1]) {
            swap(arr, i, i - 1);
            exchanges++;
        }
    }
    // 经过上面的操作后，最小的元素就放在了开头（哨兵）
    if (exchanges === 0) return;

    for (let i = 2; i < n; i++) {
        const v = arr[i];
        let j = i;
        while (v < arr[j - 1]) {
            arr[j] = arr[j - 1];
            j--;
        }
        if (j !== i) {
            arr[j] = v;
        }
    }
};

const insertSortNoSwap2 = arr => {
    const n = arr.length;
    for (let i = 1; i < n; i++) {
        let v = arr[i];
        let j = i - 1;
        while (j >= 0 && v < arr[j]) {
            arr[j + 1] = arr[j];
            j--;
        }
        if (j !== i - 1) {
            arr[j + 1] = v;
        }
    }
};
```

#### 插入排序中的哨兵

2.1.24 插入排序的哨兵。在插入排序的实现中先找出最小的元素并将其置于数组的最左边，这样就能去掉内循环的判断条件 j > 0,这是一种常见的规避边界测试的方法，能够省略判断条件的元素通常称为哨兵。

```js
const insertSort = arr => {
    const n = arr.length;

    // 找到数组最小元素放在最左边
    let minIndex = 0;
    for (let i = 1; i < n; i++) {
        if (arr[i] < arr[minIndex]) {
            minIndex = i;
        }
    }
    if (minIndex !== 0) {
        swap(arr, 0, minIndex);
    }

    for (let i = 1; i < n; i++) {
        // 规避了判断条件j > 0
        for (let j = i; arr[j] < arr[j - 1]; j--) {
            swap(arr, j, j - 1);
        }
    }
};
```

java中的`Arrays.sort`针对原始类型使用快速排序，针对引用类型使用归并排序。

### 希尔排序

希尔排序本质上是插入排序的改进。是一种*缩小增量排序*。考虑一种极端情形：最小的元素位于数组的尽头，那么它挪动到正确的位置需要进行n-1次移动。它交换不相邻的元素以对数组的局部进行排序，最终利用插入排序将局部有序的数组排序。

> 先将整个待排元素序列分割成若干个子序列（由相隔某个“增量”的元素组成的）分别进行直接插入排序，然后依次缩减增量再进行排序，待整个序列中的元素基本有序（增量足够小）时，再对全体元素进行一次直接插入排序。因为*直接插入排序在元素基本有序的情况下（接近最好情况），效率是很高的*，因此希尔排序在时间效率上比前两种方法有较大提高,*数组越大，优势越大*。

希尔排序更高效的原因是权衡了子数组的规模和有序性。排序之初：各个数组都很短，排序之后子数组都是部分有序的，这两种情况都非常适合插入排序。

[希尔排序](http://blog.csdn.net/morewindows/article/details/6668714)

### 归并排序

参见[归并排序](merge_sort.md)

### 快速排序

参见[快速排序](quick_sort.md)

### 出列排序

问题来源：算法练习2.1.14。说说你会如何将一副扑克牌排序，限制条件是只能查看最上面的两张牌，交换最上面的两张牌，或是将最上面的一张牌放到这摞牌的最下面。

```js
const sort = arr => {
    const cards = arr.slice();
    const ret = [];
    while (true) {
        // 经过n-1次操作后最小的牌放在了最前面，将此牌出列，不断找到最小的牌
        for (let i = 0; i < cards.length - 1; i++) {
            // 将较小的一个元素向后挪一个位置，保证不会被换走
            if (cards[0] < cards[1]) {
                [cards[0], cards[1]] = [cards[1], cards[0]];
            }
            cards.push(cards.shift());
            console.log(cards);
        }
        console.log('after change', cards);
        // 最大的牌被放在了最前面(出列)
        const min = cards.shift();
        ret.push(min);
        if (cards.length === 0) {
            break;
        }
    }
    return ret;
}

const arr = [4, 3, 1, 2, 5];
const ret = sort(arr);
console.log(ret);
```

## 背包

一种不支持从中删除元素的集合数据类型——它的目的就是帮助用例收集元素并迭代遍历所有收集到的元素（用例也可以检查背包是否为空或者背包中元素的数量）。迭代的顺序不确定且和用例无关。

## 队列

FIFO是公平性的体现。

> 南方有嘉木，谁与望天堂.

### 环形链表实现的队列

```js
class Queue {
    constructor() {
        this.last = null;
    }
    enqueue(item) {
        const node = new Node(item);
        if (this.last == null) {
            this.last = node;
            node.next = node;
        } else {
            node.next = this.last.next;
            this.last.next = node;
            this.last = node;
        }
    }
    dequeue() {
        if (this.isEmpty()) return null;
        const item = this.last.next.value;
        if (this.last.next == this.last) {
            this.last = null;
        } else {
            this.last.next = this.last.next.next;
        }
        return item;
    }
    isEmpty() {
        return this.last === null;
    }
}
```

## 下压栈

栈的基本思想是LIFO。新邮件到来的时候，你总是可以看到它们在栈的最上面。这种思想的好处是我们可以及时看到最新的内容。当我们点击一个超链接的时候浏览器会进入一个新的页面（并将其压入一个栈），我们可以不断点击超链接访问新页面，但是总可以通过点击“后退”按钮重新访问以前的页面（从栈中弹出）。

### 算术表达式求值

算术表达式可能是一个数，或者由一个左括号、一个算术表达式、一个运算符、另一个算术表达式和右括号组成的表达式。这里定义的是未省略括号的算术表达式，例如：1 + 2 * 3最该定义中应该表示为(1 + (2 * 3))。简单起见我们定义算术运算符包含二元运算加减乘除和一元运算sqrt。难点在于如何*解析*由括号、运算符和数字组成的字符串，并按照正确的顺序进行初级算术运算。这个就是Dijkstra的双栈法求值（符号栈和数字栈）。表达式由括号、运算符和操作数组成，我们按照以下的规则从左到右将其送入栈处理：

- 将操作数压入数栈
- 将操作符压入符栈
- 忽略左括号
- 遇到右括号时，弹出一个运算符，弹出所需数量的操作数，并将运算符和操作数的运算结果压入操作数栈

在处理完最后一个右括号时，操作数栈上只有一个值，它就是整个表达式的值。这个算法的原理是：每当算法遇到一个被括号包围并且由一个运算符和两个操作数组成的子表达式时，它都将运算符和操作数的计算结果压入操作数栈。这样的结果就好像在输入中用这个值代替了该子表达式，因此用这个值代替子表达式得到的结果和原来的表达式是相同的。我们可以反复应用这个规律并得到一个最终值。参见`Evaluation.js`。

### 括号匹配问题

匹配括号是否成对出现。`[()]{}{[()()]()}`为true，而`[(])`为false。与双栈法求值算法类似：遇到左括号(`(`,`[`或者`{`)的时候入栈，遇到右括号的时候出栈判断匹配问题。

### 括号补全问题

从标准输入得到一个缺少左括号的表达式并打印出补全括号之后的中序表达式。`1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) )` => `((1 + 2) * ((3 - 4) * (5 - 6)))`

- 使用两个栈分别保存数值和操作符，分别为opStack和valueStack。顺序处理输入字符串的字符：
- 如果为操作符，压入opStack。
- 否则，如果为右括号，从valueStack取出两个操作数，从opStack取出1个操作符，添加括号组合后压入valueStack。
- 否则，为数字，压入valueStack。
以上处理办法需要输出满足以下条件，也就是有如下的假设：输入表达式是合法的。

### 中缀转后缀

与算术表达式求值使用的算法一样，值栈和符号栈。扫描字符
- 忽略左括号
- 遇到数字压入valueStack
- 遇到符号压入opStack
- 遇到右括号从valueStack中弹出2个操作数，从opStack中弹出一个操作符，计算后缀表达式压入valueStack
- 最后valueStack中的值就是后缀表达式

### 后缀表达式求值

- 思路类似双栈法，但是扫描字符串的时候遇到操作符的时候就需要计算结果了，可以省掉opStack
- 遇到数字压入valueStack
- 遇到操作符从valueStack中弹出2个元素，并进行相关操作后压入valueStack
- 后缀表达式的值就是栈顶的值

## 优先队列

参见[优先队列](priority_queue.md)

## 符号表

参见[符号表](symbol_table.md)

## 二分搜索树

参见[二分搜索树](bst.md)

## 哈希表

## 参考资料

- [《算法》](https://github.com/aistrate/AlgorithmsSedgewick)
- [练习题java实现](https://github.com/YangXiaoHei/Algorithms)
- [练习题C#实现](https://github.com/ikesnowy/Algorithms-4th-Edition-in-Csharp)
- [算法4的数据文件](http://git-hexo-blog.oss-cn-beijing.aliyuncs.com/algs4-data.zip)
- [Fighter's Blog](https://fighterhit.github.io/)

## 难点

- 1.3.49
- 2.2.25
- 2.3.22
- 2.3.24
- 2.4.16
- 2.4.23
- 2.4.24
- 2.4.29
- 2.4.35
- 2.4.42
- 2.5.19
- 2.5.20
- 2.5.32
- 3.2.5
- 3.2.16
- 3.2.20
- 3.2.24
- 3.2.35
- 3.2.41
- 3.4.6
- 3.4.7
- 3.4.18
- 3.4.20
- 3.4.21
- 3.4.23
- 3.4.27
- 3.4.28
- 3.4.29
- 3.4.32
- 3.5.15
- 3.5.22
- 3.5.27

## webstorm快捷键

- 上下移动代码：cmd+shift+上/下
- 在当前目录新建文件：control + option + N
- 选取光标左右:cmd + shift + 左/右
- 打开最近文件：cmd + E
- 前进、后退:`cmd+[,cmd+]`
- 替换:cmd + R
- 搜索：shift + cmd + F
- 打开剪切板：ctrl + shift + v:ctrl+c多次的内容可以存在剪切板中，然后批量粘贴 
- 查看类中的方法：cmd+F12，直接输入方法名可以过滤
- 向前删除：Fn+delete
- PageUp/PageDown:Fn+上/下
- 显示方法参数：cmd+P
- 显示文档信息：F1
- 重写父类方法:control+o
- 包围代码:cmd+option+T(if,else,try,for,括号)
- 优化导入:control+option+O
- 从缓冲区中粘贴：cmd+shift+V
- 将代码合并成一行：control+shift+J
- 大小写转换：cmd+shift+U
- 查找方法调用处：option+F7
- 跳转到方法实现处:cmd+option+B
- 显示类的层次：control+H
- 显示方法层次：cmd+shift+H
- 跳到下一个/上一个警告的位置:F2/shift+F2
- 打开/关闭对应的侧边窗口:cmd+1/2/3/4/5
 
