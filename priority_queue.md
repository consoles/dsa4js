# 优先队列

堆排序基于优先队列。基于堆实现的优先队列常用于解决topK的问题。求最大的K个元素，只需要创建容量为K的最小堆，如果队列满的时候删除元素（堆顶的最小元素），当输入完毕之后留在队列中的K个元素都是最大的K个元素。

二叉堆如果用链式存储来表示二叉树，则每个节点需要维护3个指针（左右孩子节点和父节点），但是如果我们使用二叉树的话用数组就可以表示了：空出数组的第一个位置，按照层序顺序放入数组中。

索引为k(数组索引k从1开始)的节点的父节点为parseInt(k / 2),左孩子2k，右孩子2k+1。

```javascript
const swap = require('../../swap');

class MaxPQ {
  /**
   * @param arr
   * @param bottomUp 是否自底向上建堆
   */
  constructor(arr, bottomUp = false) {
    this.arr = [-1].concat(arr);
    if (bottomUp)
      this.initBottomUp();
    else
      this.initTopDown();
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this.arr[k] > this.arr[parentIndex]) {
        swap(this.arr, k, parentIndex);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _sink(k) {
    const maxIndex = this.arr.length - 1;
    while (k < maxIndex) {
      let j = 2 * k;
      if (j > maxIndex) break;
      if (j + 1 <= maxIndex && this.arr[j] < this.arr[j + 1]) {
        j++;
      }
      if (this.arr[j] <= this.arr[k]) {
        break;
      }
      swap(this.arr, j, k);
      k = j;
    }
  }

  initBottomUp() {
    // 叶子节点（没有子节点）自然而然是堆
    // 从第一个非叶子节点逐层向上，直到根节点执行sink
    for (let i = parseInt(this.arr.length / 2); i >= 1; i--) {
      this._sink(i);
    }
  }

  initTopDown() {
    // 当前元素不断swim直到根节点（就像向优先队列中插入元素一样）
    for (let i = 2; i < this.arr.length; i++) {
      this._swim(i);
    }
  }
}
```

构建堆有两种方式，从顶向下(swim)和自底向上(sink)。

## 优先队列的链表实现

由于没有了数组索引那么方便，如何定义下一个节点的位置就比较难办。实现起来比数组实现难了不少

```javascript
class Node {
  constructor(value) {
    this.value = value;
    this.left = null;
    this.right = null;
    this.parent = null;
  }
}

class MaxPQ {
  constructor() {
    this.sz = 0;
    this.root = null;
    this.last = null;
  }

  static less(node1, node2) {
    return node1.value < node2.value;
  }

  /**
   * 交换2个节点的值
   * @param node1
   * @param node2
   */
  static swap(node1, node2) {
    [node1.value, node2.value] = [node2.value, node1.value];
  }

  static _swim(node) {
    while (node.parent) {
      if (MaxPQ.less(node.parent, node)) {
        MaxPQ.swap(node.parent, node);
        node = node.parent;
      } else {
        break;
      }
    }
  }

  insert(value) {
    const node = new Node(value);
    this.sz++;
    // 堆为空
    if (!this.last) {
      this.last = this.root = node;
      return;
    }
    // 只有一个节点
    if (this.last === this.root) {
      node.parent = this.root;
      this.root.left = node;
      this.last = node;
      MaxPQ._swim(node);
      return;
    }
    // 定位到最后一个节点的父节点
    let parent = this.last.parent;
    // 右子节点为空，插入到右子节点
    if (!parent.right) {
      node.parent = parent;
      parent.right = node;
    } else {
      // 当前子树已满，需要向上回溯
      // 找到下一个子树（回溯的时候从左子树回溯上去）
      while (parent !== this.root) {
        if (parent !== parent.parent.right) {
          break;
        }
        parent = parent.parent;
      }
      // 已经是满二叉树
      if (parent === this.root) {
        // 一路向左，进入下一层
        while (parent.left) {
          parent = parent.left;
        }
        node.parent = parent;
        parent.left = node;
      } else {
        // 不是满二叉树
        // 向左子树移动，再一路向左
        parent = parent.parent.right;
        while (parent.left) {
          parent = parent.left;
        }
        node.parent = parent;
        parent.left = node;
      }
    }
    this.last = node;
    MaxPQ._swim(node);
  }

  static _sink(node) {
    while (node.left) {
      let toExch = node.left;
      if (node.right && MaxPQ.less(node.left, node.right)) toExch = node.right;
      if (MaxPQ.less(node, toExch)) {
        MaxPQ.swap(node, toExch);
        node = toExch;
      } else {
        break;
      }
    }
  }

  delMax() {
    const ret = this.root.value;
    MaxPQ.swap(this.root, this.last);

    if (this.sz === 2) {
      this.root.left = null;
      this.last = this.root;
      this.sz--;
      return ret;
    }
    if (this.sz === 1) {
      this.last = null;
      this.root = null;
      this.sz--;
      return ret;
    }
    let newLast = this.last;
    if (newLast === this.last.parent.right) {
      newLast = this.last.parent.left;
    } else {
      // 找到上一颗子树
      while (newLast !== this.root) {
        if (newLast !== newLast.parent.left) {
          break;
        }
        newLast = newLast.parent;
      }
      // 已经是满二叉树
      if (newLast === this.root) {
        // 一路向右，回到上一层
        while (newLast.right) {
          newLast = newLast.right;
        }
      } else {
        // 不是满二叉树
        // 向左子树移动，再一路向右
        newLast = newLast.parent.left;
        while (newLast.right) {
          newLast = newLast.right;
        }
      }
    }

    // 删除最后一个节点
    if (this.last.parent.left === this.last) {
      this.last.parent.left = null;
    } else {
      this.last.parent.right = null;
    }

    MaxPQ._sink(this.root);
    // 指向新的最后一个节点
    this.last = newLast;
    this.sz--;
    return ret;
  }

  isEmpty() {
    return this.sz === 0;
  }
}
```

## 无需交换的堆

在swim的时候判断当前节点和父节点，如果当前节点的值比父节点的值大就交换当前值和父节点的值，其实我们只需要将正确的值移动到正确的位置。下面的代码不需要两两交换，直接赋值即可：

```javascript
_sink(k) {
  const key = this.data[k];
  while (2 * k <= this.sz) {
    let j = 2 * k;
    if (j + 1 <= this.sz && this.data[j + 1] > this.data[j]) {
      j++;
    }
    if (this.data[j] <= key) {
      break;
    }
    this.data[k] = this.data[j];
    k = j;
  }
  this.data[k] = key;
}

_swim(k) {
  const key = this.data[k];
  while (k > 1 && this.data[parseInt(k / 2)] < key) {
    this.data[k] = this.data[parseInt(k / 2)];
    k = parseInt(k / 2);
  }
  this.data[k] = key;
}
```

# 堆排序

堆排序可以分为2个阶段：

构造阶段：将原始数组重新组织安排进1个堆中
下沉阶段：从堆中按照递减顺序取出所有元素并得到排序结果

我们可以将需要排序的数组本身作为堆，因此堆排序无需额外的空间。

## 堆的构造

我们可以在NlgN的时间完成这项任务，只需要从左到右遍历数组，用swim保证扫描指针左侧的所有元素已经是一颗堆有序的完全树即可，就像连续向优先队列中插入元素一样。

一个更聪明高效的方法是从右向左用sink函数构建子堆。数组的每个位置都已经是一个子堆的根节点了，sink对于这些子堆也适用。如果一个节点的2个子节点已经是堆了，那么在该节点上调用sink可以将它们变成一个堆。这个过程会递归地建立起堆的秩序。开始的时候我们只需要扫描数组中一半的元素，因为我们可以跳过大小为1的子堆。最后我们在位置1上调用sink方法，扫描结束。在排序的第一阶段堆的构造方法和我们想象中有所不同，因为我们的目标是构造一个堆有序的数组并使最大元素位于数组的开头（次大的元素在附近）而非构造函数的末尾。

![堆排序](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/heap-sort.png)

## 下沉排序

堆排序的主要工作都是在第二阶段完成的。在这里我们将堆中的最大元素删除，然后放入堆缩小后数组空出的位置。这个过程和选择排序有些相似（按照降序而非升序取出所有元素），但是所需要的比较次数少得多，因为堆提供了一种从未排序部分找到最大元素的有效方法。

```javascript
class HeapSort {
  sort(arr) {
    let n = arr.length;
    // 建堆
    for (let k = parseInt(n / 2); k >= 1; k--) {
      this._sink(arr, k, n);
    }
    while (n > 1) {
      this._swap(arr, 1, n--);
      this._sink(arr, 1, n);
    }
  }

  _sink(arr, k, n) {
    while (2 * k <= n) {
      let j = 2 * k;
      if (j < n && this._less(arr, j, j + 1)) {
        j++;
      }
      if (this._less(arr, k, j)) {
        this._swap(arr, j, k);
        k = j;
      } else {
        break;
      }
    }
  }

  _less(arr, i, j) {
    return arr[i - 1] < arr[j - 1];
  }

  _swap(arr, i, j) {
    [arr[i - 1], arr[j - 1]] = [arr[j - 1], arr[i - 1]];
  }
}
```

堆排序在排序复杂性的研究中有着重要的地位——它是我们所知的唯一能够最优利用空间和时间的方法：在最坏的情况下也能保证使用2NlgN次比较和额定的额外空间。当空间十分紧张的时候这种算法非常流行，而在现代OS中很少使用它，因为数组元素很少与相邻的元素进行比较，缓存未命中的次数远远高于大多数都在相邻元素间进行的算法。

由于堆排序分为2个阶段：构建阶段和下沉阶段（最大的元素放在最后的位置，然后右边界向左移动），因此构造堆的时候应该构建最大堆。

## 堆排序针对比较次数的优化方法Floyd

大多数在下沉排序期间重新插入堆的元素会被直接加入到堆底。Floyd观察发现：我们正好可以通过免去检查元素是否到达正确的位置来节省时间。在下沉中总是直接提升较大的子节点直至到达堆底，然后再使元素上浮到正确的位置。这个想法几乎能将*比较次数减少一半* ———— 接近了归并排序所需的比较次数（随机数组）。这种方法需要额外的空间，因此在实际应用中只有当比较操作代价较高时才采用.

```javascript
class HeapSortFloyd extends HeapSort {
  sort(arr) {
    let n = arr.length;
    // 建堆
    for (let k = parseInt(n / 2); k >= 1; k--) {
      this._sink(arr, k, n);
    }
    // 排序
    while (n > 1) {
      this._swap(arr, 1, n--);
      this._sinkThenSwim(arr, 1, n);
    }
  }

  _swim(arr, k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this._less(arr,parentIndex,k)) {
        this._swap(arr,k,parentIndex);
        k = parentIndex;
      } else{
        break;
      }
    }
  }

  _sinkThenSwim(arr, k, n) {
    while (2 * k <= n) {
      let j = 2 * k;
      if (j < n && this._less(arr, j, j + 1)) {
        j++;
      }
      // 去掉下沉的条件this._less(arr, k, j)
      this._swap(arr, k, j);
      k = j;
    }
    // sink循环结束之后调用swim
    this._swim(arr, k);
  }
}
```

# 求数组前k大元素

维护一个容量为k的优先队列（最小堆）。当队列未满的时候入队，队列满的时候入队然后删除最小元素，数组扫描完成后留在队列中的就是最大的k个元素。

如果一个用例使用insert插入了一个比队列中所有元素都大的新元素，随后立即调用delMax。假设没有重复元素，此时的堆和进行这些操作之前的堆完全相同么？进行两次insert然后进行两次delMax呢？

首先看第一种情况，一次 insert() 接一次 delMax()。由于插入的数比堆中的所有元素都大，这个元素会一路上升到根结点。记上升路径上的点为 a1,a2,a3,…,ak，其中 ak 是插入的结点，a1 是根结点。插入完成后路径上点的次序变为 ak,a1,a2,…,ak−1 。随后进行一次 delMax()，先做交换，次序变为 ak−1,a1,…,ak−2,ak 。由于 a1 是堆中原来的最大值，下沉时一定会和它交换。根据定义，二叉堆是父结点总是优于子结点的完全二叉树，因此以后续结点作为根结点的子树也都是堆。故同理 ak−1 会和 a2,a3,…,ak−2 交换，即沿原路径返回。因此这种情况下前后堆不发生改变。

然后看第二种情况，操作顺序为 insert() insert() delMax() delMax()。根据之前的结论，插入最大结点之后立即删除最大元素不会使堆发生变化，中间的两个操作抵消。序列变为：insert() delMax()。同理再次利用刚才的结论，操作抵消，堆不发生变化。故第二种情况也不会使堆发生改变。

# 使用优先队列实现栈、队列和随机队列

给元素标上序号，按照序号(key)进行排序：

```javascript
class Node {
  constructor(key, value) {
    this.key = key;
    this.value = value;
  }
}
```

栈：LIFO，基于最大堆的PQ，保证最近插入的元素是最大的。入栈即插入元素，保证插入元素的时候key是最大的，出栈就是delMax操作，因为最近的元素序号最大，所以符合栈的定义
队列：LIFO,基于最小堆的PQ,每次插入的时候序号递增，这样最先插入的元素因为序号比较小会在堆顶
随机队列：插入元素的时候随机一个序号即可

```javascript
class MaxPQ {
  constructor() {
    this.arr = [-1];
    this.num = 0;
    this.sz = 0;
  }

  less(i, j) {
    return this.arr[i].key < this.arr[j].key;
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (!this.less(parentIndex, k)) break;
      swap(this.arr, k, parentIndex);
      k = parentIndex;
    }
  }

  _sink(k) {
    const sz = this.sz;
    while (k < sz) {
      let j = 2 * k;
      if (j > sz) break;
      if (j + 1 < sz && this.less(j, j + 1)) j++;
      if(!this.less(k,j)) break;
      swap(this.arr, j, k);
      k = j;
    }
  }

  getKey(){
    return this.num++;
  }

  insert(value) {
    const key = this.getKey();
    const node = new Node(key, value);
    this.arr.push(node);
    this.sz++;
    this._swim(this.sz);
  }

  delMax() {
    const value = this.arr[1].value;
    swap(this.arr, 1, this.sz--);
    this._sink(1);
    return value;
  }

  isEmpty(){
    return this.sz === 0;
  }
}

class MinPQ extends MaxPQ {
  less(i,j) {
    return this.arr[j].value < this.arr[i].value;
  }
}

class RandomPQ extends MaxPQ {
  getKey() {
    return Math.random();
  }
}

class Stack {
  constructor() {
    this.q = new MaxPQ();
  }
  isEmpty(){
    return this.q.isEmpty();
  }
  push(value) {
    this.q.insert(value);
  }
  pop() {
    return this.q.delMax();
  }
}

class Queue{
  constructor(){
    this.pq = new MinPQ();
  }
  isEmpty(){
    return this.pq.isEmpty();
  }
  enqueue(value){
    this.pq.insert(value);
  }
  dequeue(){
    return this.pq.delMax();
  }
}

class RandomQueue extends Queue{
  constructor(){
    super();
    this.pq = new RandomPQ();
  }
}
```

# 动态中位数查找

设计一种DS，支持在对数时间内插入元素、常数时间内找到中位数并在对数时间内删除中位数。

单独用一个变量存放中位数，然后前半部分放在一个最大堆中，后半部分放在一个最小堆中

![动态中位数查找](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/median_queue.png)

上图中median和两个堆并没有直接连接，这里只是为了方便理解元素的顺序

- 最大堆中存放的值都不大于median，最小堆中存放的值都不小于median，这样有相对顺序 `最大堆堆顶 <= median <= 最小堆堆顶`
- 只要两个堆含有的元素之差不超过1，那么median变量中存放的就是整个数组的中位数。
- 如果元素差大于1就需要进行调整：把median变量插入到元素较小的堆中，再从元素较多的堆中取出元素放入到median变量，直到元素差不大于1
- 插入元素的时候，根据插入元素的大小插入到另一个堆中去，再做一次调整
- 删除中位数大的时候，去掉中位数，从元素较多的一侧堆中取出元素补位，再进行一次调整
- 注意处理堆中只有一个元素的情况


```javascript
class MedianPQ {
  constructor() {
    this.maxPQ = new MaxPQ(); // 最大堆，保存前半段元素
    this.minPQ = new MinPQ(); // 最小堆，保存后半段元素
    this.median = null; // 中位数
    this.sz = 0; // 堆大小
  }

  insert(value) {
    if (this.sz++ === 0) {
      this.median = value;
      return;
    }
    if (value < this.median) {
      this.maxPQ.insert(value);
    } else {
      this.minPQ.insert(value);
    }
    this.updateMedian();
  }

  /**
   * 删除并返回中位数
   */
  delMedian() {
    const median = this.median;
    if (--this.sz === 0) {
      this.median = null;
      return median;
    }
    // 从较大的一侧堆中取出元素作为新的中位数
    if (this.minPQ.sz > this.maxPQ.sz) {
      this.median = this.minPQ.delMin();
    } else {
      this.median = this.maxPQ.delMax();
    }
    return median;
  }

  isEmpty() {
    return this.sz === 0;
  }

  /**
   * 根据两个堆的大小调整中位数
   */
  updateMedian() {
    while (this.maxPQ.sz - this.minPQ.sz > 1) {
      this.minPQ.insert(this.median);
      this.median = this.maxPQ.delMax();
    }
    while (this.minPQ.sz - this.maxPQ.sz > 1) {
      this.maxPQ.insert(this.median);
      this.median = this.minPQ.delMin();
    }
  }
}
```

# 最大-最小优先队列

对数时间插入元素、删除最大元素；常数级别找到最大元素和最小元素。算法思想比较简单：用一个最大堆和最小堆，每个堆中保存了全部的数组元素，且相同的元素之间有指针相连。

![最大最小优先队列](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/min_max_pq.png)

插入元素的时候需要构建两个相同的元素分别插入到2个堆中。找到最小元素和找到最大元素只需要分别返回最大堆和最小堆的堆顶元素即可。以删除最小元素为例，先对最小堆进行 DelMin() 操作，再通过指针找到对应最大堆的元素并删除。

```js
class MinMaxNode {
  constructor(value, index) {
    this.value = value;
    this.index = index;
    this.pair = null;
  }

  /**
   * 工厂方法，建立两个孪生节点
   * @param value
   * @param index
   * @returns {{minNode: MinMaxNode, maxNode: MinMaxNode}}
   */
  static getNodes(value, index) {
    const minNode = new MinMaxNode(value, index);
    const maxNode = new MinMaxNode(value, index);
    minNode.pair = maxNode;
    maxNode.pair = minNode;
    return {minNode, maxNode};
  }
}

class MaxPQ {
  constructor() {
    this.data = [-1];
    this.sz = 0;
  }

  less(i, j) {
    return this.data[i].value < this.data[j].value;
  }

  swap(i, j) {
    this.data[i].pair.pair = this.data[j];
    this.data[j].pair.pair = this.data[i];

    const swapNode = this.data[i].pair;
    const swapValue = this.data[i].value;

    this.data[i].value = this.data[j].value;
    this.data[i].pair = this.data[j].pair;

    this.data[j].value = swapValue;
    this.data[j].pair = swapNode;
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this.less(parentIndex, k)) {
        this.swap(k, parentIndex);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this.less(j, j + 1)) {
        j++;
      }
      if (this.less(j, k)) {
        break;
      } else {
        this.swap(j, k);
        k = j;
      }
    }
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  delMax() {
    const value = this.data[1];
    const sz = this.sz--;
    this.swap(1, sz);
    this.data[sz] = null;
    this._sink(1);
    return value;
  }

  /**
   * 删除指定索引的元素
   */
  remove(k) {
    const sz = this.sz--;
    if (k === sz) {
      this.data[sz] = null;
      return;
    }
    if (this.sz <= 2) {
      this.swap(1, k);
      this.data[sz] = null;
      return;
    }
    this.swap(k, sz);
    this.data[sz] = null;
    this._swim(k);
    this._sink(k);
  }

  max() {
    return this.data[1];
  }

  isEmpty() {
    return this.sz === 0;
  }
}

class MinPQ extends MaxPQ {
  less(i, j) {
    return this.data[j].value < this.data[i].value;
  }

  min() {
    return super.max();
  }

  delMin() {
    return super.delMax();
  }

  max() {
    throw new Error('没有这个方法');
  }

  delMax() {
    throw new Error('没有这个方法');
  }
}

class MinMaxPQ {
  constructor() {
    this.minPQ = new MinPQ();
    this.maxPQ = new MaxPQ();
    this.sz = 0;
  }

  delMax() {
    this.minPQ.remove(this.maxPQ.max().pair.index);
    const value = this.maxPQ.max().value;
    this.maxPQ.delMax();
    this.sz--;
    return value;
  }

  delMin() {
    this.maxPQ.remove(this.minPQ.min().pair.index);
    const value = this.minPQ.min().value;
    this.minPQ.delMin();
    this.sz--;
    return value;
  }

  insert(value) {
    const {minNode, maxNode} = MinMaxNode.getNodes(value, ++this.sz);
    this.maxPQ.insert(maxNode);
    this.minPQ.insert(minNode);
  }

  isEmpty() {
    return this.sz === 0;
  }

  max() {
    return this.maxPQ.max().value;
  }

  min() {
    return this.minPQ.min().value;
  }
}
```

# 索引优先队列

在很多应用中，允许用例引用已经进入优先队列中的元素是非常有必要的。做到这一点最简单的方法是给每个元素一个索引。另外一种常见的情况是用例已经有了总量为N的多个元素，并且还同时使用了多个（平行）数组来存储这些元素的信息。此时，其他无关的用例代码已经在使用一个整数索引来引用这些元素了。

```javascript
class IndexMinPQ {
  constructor() {
    this.sz = 0;
    this.pq = [-1]; // 索引二叉堆，从1开始
    this.qp = [-1]; // 逆序:qp[pq[i]] = pq[qp[i]] = i;
    this.values = [-1]; // 有优先级之分的元素
  }

  _less(i, j) {
    return this.values[this.pq[i]] < this.values[this.pq[j]];
  }

  _swap(i, j) {
    const tmp = this.pq[i];
    this.pq[i] = this.pq[j];
    this.pq[j] = tmp;
    this.qp[this.pq[i]] = i;
    this.qp[this.pq[j]] = j;
  }

  isEmpty() {
    return this.sz === 0;
  }

  contains(k) {
    return Number.isInteger(this.qp[k]) && this.qp[k] !== -1;
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this._less(k, parentIndex)) {
        this._swap(parentIndex, k);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  insert(k, value) {
    const sz = ++this.sz;
    this.qp[k] = sz;
    this.pq[sz] = k;
    this.values[k] = value;
    this._swim(sz);
  }

  min() {
    return this.values[this.pq[1]];
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this._less(j + 1, j)) {
        j++;
      }
      if (this._less(j, k)) {
        this._swap(k, j);
        k = j;
      } else {
        break;
      }
    }
  }

  delMin() {
    const index = this.pq[1];
    const sz = this.sz--;
    this._swap(1, sz);
    this._sink(1);
    const value = this.values[this.pq[sz]];
    this.values[this.pq[sz]] = null;
    this.qp[this.pq[sz + 1]] = -1;
    return {index, value};
  }

  minIndex() {
    return this.pq[1];
  }

  change(k, value) {
    this.values[k] = value;
    // 顺序无关紧要
    this._swim(this.qp[k]);
    this._sink(this.qp[k]);
  }

  delete(k) {
    const index = this.qp[k];
    this._swap(index,this.sz--);
    this._swim(index);
    this._sink(index);
    this.values[k] = null;
    this.qp[k] = -1;
  }
}
```

## 使用索引优先队列实现多路归并

```js
class Stream {
  constructor(data) {
    this.readIndex = 0;
    this.data = data;
  }

  next() {
    return this.data[this.readIndex++];
  }

  hasNext() {
    return this.readIndex < this.data.length;
  }
}
const streams = [
  [1, 4, 9, 16, 25, 36],
  [-3, -1, 1, 3, 5, 7, 9],
  [0, 2, 4, 6, 8, 10],
  [1, 1, 2, 3, 5, 7, 11, 13]
].map(data => new Stream(data));

function merge(streams) {
  const n = streams.length;
  const pq = new IndexMinPQ();
  for (let i = 0; i < n; i++) {
    const stream = streams[i];
    if (stream.hasNext()) {
      pq.insert(i, stream.next());
    }
  }
  while (!pq.isEmpty()) {
    const {index, value} = pq.delMin();
    console.log(value);
    const stream = streams[index];
    if (stream.hasNext()) {
      pq.insert(index, stream.next());
    }
  }
}

merge(streams);
// -3 -1 0 1 1 1 1 2 2 3 3 4 4 5 5 6 7 7 8 9 9 10 11 13 16 25 36
```

上面的代码使用索引优先队列实现了多路归并问题：将多个*有序*输入流归并成一个有序的输出流。许多应用中都会有这个问题。输入可能来自于多种科学仪器的输出（按照时间排序），或者多个音乐或者电影网站的信息列表（按名称或者艺术家名字排序），或是商品交易（按账号或者时间排序），或者其他。如果有足够的空间，可以简单读入一个数组并排序，但是使用了优先队列，*无论输入有多长*你都可以将其全部读入并排序。

# 多叉堆

多叉堆和二叉堆的实现很相似，因为也是完全二叉树，所以可以很方便地用数组表示，只不过sink的时候需要比较的子节点变成了*最大d个*，swim时候的父节点的下标也变了。对于d叉堆，第k个节点的子节点的范围是`[d(i−1)+2,di+1]`,父节点的下标为`Math.floor((i-2) /d) + 1`。

```js
class DWayMaxHeap {
  constructor(d) {
    this.d = d;
    this.sz = 0;
    this.data = [-1];
  }

  _sink(k) {
    const sz = this.sz;
    const d = this.d;
    while (k <= sz) {
      const start = d * (k - 1) + 2;
      // 注意边界判断
      if (start > sz) {
        break;
      }
      const end = Math.min(start + d - 1, sz);
      let j = start; // 最大值对应的索引
      for (let i = start; i <= end; i++) {
        if (this._less(j, i)) {
          j = i;
        }
      }
      if (this._less(k, j)) {
        this._swap(j, k);
        k = j;
      } else {
        break;
      }
    }
  }

  _swim(k) {
    const d = this.d;
    while (k > 1) {
      const parentIndex = Math.floor((k - 2) / d) + 1;
      if (this._less(parentIndex, k)) {
        this._swap(k, parentIndex);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  _less(i, j) {
    return this.data[i] < this.data[j];
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  delMax() {
    const value = this.data[1];
    const sz = this.sz--;
    this._swap(1, sz);
    this._sink(1);
    return value;
  }

  isEmpty() {
    return this.sz === 0;
  }
}
```

# 优先队列与任务调度和负载均衡

从标准输入读取任务和运行时间，按照最短时间优先的原则准备调度计划，将所有任务分配给M个处理器并使得所有任务完成所需要的时间最少。

```js
class MinPQ {
  constructor() {
    this._data = [-1];
    this.sz = 0;
  }

  insert(value) {
    const sz = ++this.sz;
    this._data[sz] = value;
    this._swim(sz);
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = k >> 1;
      if (this._less(k, parentIndex)) {
        this._swap(parentIndex, k);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _less(i, j) {
    return this._data[i].compareTo(this._data[j]) < 0;
  }

  _swap(i, j) {
    [this._data[i], this._data[j]] = [this._data[j], this._data[i]];
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this._less(j + 1, j)) {
        j++;
      }
      if (this._less(j, k)) {
        this._swap(k, j);
        k = j;
      } else {
        break;
      }
    }
  }

  delMin() {
    const item = this._data[1];
    const sz = this.sz--;
    this._swap(sz, 1);
    this._sink(1);
    return item;
  }

  isEmpty() {
    return this.sz === 0;
  }
}

class Job {
  constructor(name, time) {
    this.name = name;
    this.time = time;
  }

  compareTo(other) {
    return this.time - other.time;
  }
}

class Processor {
  constructor(name) {
    this.name = name;
    this.jobs = [];
    this.busyTime = 0;
  }

  addJob(job) {
    this.jobs.push(job);
    this.busyTime += job.time;
  }

  compareTo(other) {
    return this.busyTime - other.busyTime;
  }
}

class LPT {
  constructor(numOfProcessor) {
    const pq = new MinPQ();
    for (let i = 0;i < numOfProcessor;i++) {
      pq.insert(new Processor('processor-' + i));
    }
    this.pq = pq;
  }

  dispatchJob(job) {
    const processor = this.pq.delMin();
    processor.addJob(job);
    this.pq.insert(processor);
    console.log(processor.name, 'get the job', job.name, 'job.time = ', job.time, 'processor busy time = ', processor.busyTime);
  }
}

const jobs = [1,2,9,5,7,8,4,3,6].map((value,index) => new Job('job' + index,value)).sort((a,b) => a.compareTo(b));
const lpt = new LPT(3);
for (const job of jobs) {
  lpt.dispatchJob(job);
}

// processor-0 get the job job0 job.time =  1 processor busy time =  1
// processor-2 get the job job1 job.time =  2 processor busy time =  2
// processor-1 get the job job7 job.time =  3 processor busy time =  3
// processor-0 get the job job6 job.time =  4 processor busy time =  5
// processor-2 get the job job3 job.time =  5 processor busy time =  7
// processor-1 get the job job8 job.time =  6 processor busy time =  9
// processor-0 get the job job4 job.time =  7 processor busy time =  12
// processor-2 get the job job5 job.time =  8 processor busy time =  15
// processor-1 get the job job2 job.time =  9 processor busy time =  18
```

# 稳定的优先队列

针对重复的元素，认为后添加的元素比较大。

思路：在插入元素的时候同时记录插入的顺序，比较的时候将插入顺序也纳入比较。对于值一样的元素插入顺序在前面的元素比较小，交换的时候需要注意同时交换插入的顺序。

```js
class MaxPQ {
  constructor() {
    this.data = [-1];
    this.seq = [-1]; // 元素的插入顺序
    this.sz = 0;
  }

  _swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
    [this.seq[i], this.seq[j]] = [this.seq[j], this.seq[i]];
  }

  _less(i, j) {
    const diff = this.data[i].compareTo(this.data[j]);
    return diff === 0 ? this.seq[i] < this.seq[j] : diff < 0;
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = k >> 1;
      if (this._less(parentIndex, k)) {
        this._swap(parentIndex, k);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  insert(item) {
    const sz = ++this.sz;
    this.data[sz] = item;
    this.seq[sz] = sz;
    this._swim(sz);
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this._less(j, j + 1)) {
        j++;
      }
      if (this._less(k, j)) {
        this._swap(k, j);
        k = j;
      } else {
        break;
      }
    }
  }

  delMax() {
    const item = this.data[1];
    const sz = this.sz--;
    this._swap(1, sz);
    this._sink(1);
    this.data[sz] = null;
    this.seq[sz] = 0;
    return item;
  }

  isEmpty(){
    return this.sz === 0;
  }
}

class Item {
  constructor(index, value) {
    this.index = index;
    this.value = value;
  }

  compareTo(other) {
    return this.value - other.value;
  }
}

const items = [2, 1, 2, 1, 3, 4, 3, 5, 7, 8, 5, 2, 1].map((value, index) => new Item(index, value));
const q = new MaxPQ();
for (const item of items) {
  q.insert(item);
}

while (!q.isEmpty()) {
  console.log(q.delMax());
}
```

输出结果：

```bash
Item { index: 9, value: 8 }
Item { index: 8, value: 7 }
Item { index: 10, value: 5 }
Item { index: 7, value: 5 }
Item { index: 5, value: 4 }
Item { index: 6, value: 3 }
Item { index: 4, value: 3 }
Item { index: 11, value: 2 }
Item { index: 2, value: 2 }
Item { index: 0, value: 2 }
Item { index: 12, value: 1 }
Item { index: 3, value: 1 }
Item { index: 1, value: 1 }
```
