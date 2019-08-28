# 二分搜索树

BST是一颗二叉树，且每个节点都大于左子树中的*任意*节点，小于右子树的*任意*节点。

中序遍历是升序就是BST，充要条件。

二分搜索树结合了链表插入的灵活性和有序数组查找的高效性。

## 二分搜索树中节点的表示

- 键
- 值
- 左孩子：指向所有小于该节点的键组成的BST
- 右孩子：指向所有大于该节点的键组成的BST
- 节点计数器：以该节点为根BST的节点总数

`size(x) = size(left) + size(right) + 1`

维护node对象中的节点计数器似乎要很多代码，但是这样做是值得的。因为rank和select方法需要知道每个节点所代表的子树中的节点数目。要保证所有节点中的计数器的正确性的确容易出错，但是这个值在调试的时候很有用。我们也可以用递归来实现用例中的size函数，但是这样统计所有节点的运行时间可能是*线性*的。这十分危险，因为如果不知道这么一个简单的操作会如此耗时，用例的性能可能会变得很差。

## BST的实现

[官方实现](https://algs4.cs.princeton.edu/32bst/BST.java.html)

```js
const assert = require('assert');

// 参见：https://algs4.cs.princeton.edu/32bst/BST.java.html
class Node {
  constructor(key, value, count) {
    this.key = key;
    this.value = value;
    this.left = null;
    this.right = null;
    this.count = count;
    this.height = 0;
  }
}

class BST {

  constructor() {
    this.root = null;
    this.cache = null;
  }

  static _size(node) {
    return node ? node.count : 0;
  }

  static _height(node) {
    return node ? node.height : 0;
  }

  _get(node, key) {
    if (!node) return null;
    const nodeKey = node.key;
    if (nodeKey === key) {
      // this.cache = node;
      return node.value;
    }
    return key > nodeKey ? this._get(node.right, key) : this._get(node.left, key);
  }

  get(key) {
    // if (this.cache && this.cache.key === key) return this.cache.value;
    return this._get(this.root, key);
  }

  /**
   * 如果key存在于以node为根节点的子树中则更新它的值；否则将以key和value为键值对的新节点插入到该子树中
   * 递归调用前：沿着树向下走：将给定的键和每个节点相比较并根据结果向左或者向右移动到下一个节点
   * 递归调用后：沿着树向上爬：重置搜索路径上每个父节点指向子节点的链接，并更新路径上每个节点计数器的值
   * 返回插入新节点后BST的根
   *
   * 这个过程有点像向链表中添加元素的递归实现
   */
  _put(node, key, value) {
    // console.log('put', '在以', node && node.key, '为根节点的BST中插入', key);
    if (!node) return new Node(key, value, 1);
    const nodeKey = node.key;
    if (nodeKey === key) {
      node.value = value;
    } else if (key < nodeKey) {
      // 这里存在重复赋值，如果node.left == left也会进行赋值，其实是不必要的，但是这样做也没错，是幂等的
      const left = this._put(node.left, key, value);
      // console.log('link-change', node.key, '的左孩子', node.left && node.left.key, ' -> ', left && left.key);
      node.left = left;
    } else {
      const right = this._put(node.right, key, value);
      // console.log('link-change', node.key, '的右孩子', node.right && node.right.key, ' -> ', right && right.key);
      node.right = right;
    }
    const leftCount = BST._size(node.left);
    const rightCount = BST._size(node.right);
    // console.log('count-change', node.key, '节点的计数器为', node.count, ' -> ', `left:${leftCount},right:${rightCount},total = ${leftCount + rightCount + 1}`);
    node.count = leftCount + rightCount + 1;
    BST._updateNodeHeight(node);
    // this.cache = node;
    return node; // 注意返回！！！
  }

  /**
   * 比较烂的写法：很多判断
   */
  _put2(node, key, value) {
    if (node.key === key) {
      node.value = value;
      return;
    }
    if (key < node.key) {
      // 没有左孩子？
      if (!node.left) {
        node.left = new Node(key, value, 1);
        node.count++;
        return;
      }
      this._put2(node.left, key, value);
    } else {
      if (!node.right) {
        node.right = new Node(key, value, 1);
        node.count++;
        return;
      }
      this._put2(node.right, key, value);
    }
    // 更新节点计数器
    BST._updateNodeCount(node);
    BST._updateNodeHeight(node);
  }

  put2(key, value) {
    this.root ? this._put2(this.root, key, value) : this.root = new Node(key, value, 1);
  }

  /**
   * 查找key，找到就更新它的值，否则为它创建一个新的节点
   */
  put(key, value) {
    if (this.cache && this.cache.key === key) {
      this.cache.value = value; // 浅拷贝，会直接修改原来的对象
      return;
    }
    this.root = this._put(this.root, key, value);
  }

  _max(node) {
    if (!node) return null;
    return !node.right ? node : this._max(node.right);
  }

  get max() {
    assert(!this.isEmpty(), 'calls max() with empty bst');
    return this._max(this.root).key;
  }

  _min(node) {
    if (!node) return null;
    return !node.left ? node : this._min(node.left);
  }

  get min() {
    assert(!this.isEmpty(), 'calls min() with empty bst');
    return this._min(this.root).key;
  }

  get size() {
    return BST._size(this.root);
  }

  contains(key) {
    return this.get(key) !== null;
  }

  isEmpty() {
    return this.size === 0;
  }

  rangeSize(lo, hi) {
    assert(lo && hi, '必须指定左右边界');
    if (lo > hi) return 0;
    const size = this.rank(hi) - this.rank(lo);
    return this.contains(hi) ? size + 1 : size;
  }

  static _updateNodeCount(node) {
    node.count = BST._size(node.left) + BST._size(node.right) + 1;
  }

  static _updateNodeHeight(node) {
    node.height = Math.max(BST._height(node.left), BST._height(node.right)) + 1;
  }

  _floor(node, key) {
    if (!node) return null;
    if (key === node.key) return node;
    if (key < node.key) return this._floor(node.left, key);
    // 右子树中找到的值不需要和根节点比较大小：因为对于BST来说，右子树中的所有键都大于根节点
    const t = this._floor(node.right, key);
    // console.log('从', node.key, '的右子树', node.right && node.right.key, '的结果', t && t.key);
    return t ? t : node;
  }

  floor(key) {
    const x = this._floor(this.root, key);
    return x ? x.key : null;
  }

  _floor2(node, key, best) {
    if (!node) return best;
    if (key === node.key) return node.key;
    return key < node.key ? this._floor2(node.left, key, best) : this._floor2(node.right, key, node.key);
  }

  floor2(key) {
    return this._floor2(this.root, key, null);
  }

  _ceil(node, key) {
    if (!node) return null;
    if (key === node.key) return node;
    if (key > node.key) return this._ceil(node.right, key);
    const t = this._ceil(node.left, key);
    return t ? t : node;
  }

  ceil(key) {
    const x = this._ceil(this.root, key);
    return x ? x.key : null;
  }

  _select(node, k) {
    if (!node) return null;
    const t = BST._size(node.left);
    if (k < t) return this._select(node.left, k);
    if (k > t) return this._select(node.right, k - t - 1);
    return node;
  }

  select(k) {
    const x = this._select(this.root, k);
    return x ? x.key : x;
  }

  _rank(node, key) {
    if (!node) return 0;
    if (key < node.key) return this._rank(node.left, key);
    if (key > node.key) return 1 + BST._size(node.left) + this._rank(node.right, key);
    return BST._size(node.left);
  }

  rank(key) {
    return this._rank(this.root, key);
  }

  /**
   * 大量删除一个随机键然后插入这个随机键，其内部路径长度会增大(随机选择前驱节点和后继节点可以解决这个问题)
   * 参见练习题42
   */
  _delete(node, key) {
    if (!node) return null;
    if (key < node.key) {
      node.left = this._delete(node.left, key);
    } else if (key > node.key) {
      node.right = this._delete(node.right, key);
    } else {
      if (!node.right) return node.left;
      if (!node.left) return node.right;
      const t = node;
      // 用被删除节点的后继节点取代被删除的节点
      node = this._min(node.right);
      // 注意这里先赋值右子树，再赋值左子树，否则node节点(t)的左子树被污染了
      node.right = this._deleteMin(t.right);
      node.left = t.left;
    }
    BST._updateNodeCount(node);
    BST._updateNodeHeight(node);
    return node;
  }

  delete(key) {
    // if (this.cache && this.cache.key === key) {
    //   this.cache = null;
    // }
    this.root = this._delete(this.root, key);
  }

  _deleteMax(node) {
    if (!node.right) {
      // if (this.cache === node) {
      //   this.cache = null;
      // }
      return node.left;
    }
    node.right = this._deleteMax(node.right);
    BST._updateNodeCount(node);
    return node;
  }

  deleteMax() {
    this.cache = null;
    this.root = this._deleteMax(this.root);
  }

  _deleteMin(node) {
    // 不断检索左子树，直到遇到空的左链接
    // 返回该节点的右链接，原先的左链接因为没有节点指向会成为孤儿对象被gc
    if (!node.left) {
      // if (this.cache === node) {
      //   this.cache = null;
      // }
      return node.right;
    }
    // 更新左子树为被删除节点的左子树
    node.left = this._deleteMin(node.left);
    // 更新节点计数器
    BST._updateNodeCount(node);
    return node;
  }

  deleteMin() {
    this.root = this._deleteMin(this.root);
  }

  // 根-左-右
  _preOrder(node) {
    // 标准的递归写法，先写递归终止条件

    if (!node) return;

    console.log(node.key);
    this._preOrder(node.left);
    this._preOrder(node.right);

    // if (node) {
    //   console.log(node.key);
    //   this._preOrder(node.left);
    //   this._preOrder(node.right);
    // }
  }

  preOrder() {
    this._preOrder(this.root);
  }

  preOrder2() {
    // 前序遍历的非递归实现(模拟系统栈的调用过程)
    const stack = [];
    stack.push(this.root);
    while (stack.length > 0) {
      const cur = stack.pop();
      console.log(cur.key);
      // 先放入右子树，这样才能先处理左子树
      if (cur.right) {
        stack.push(cur.right);
      }
      if (cur.left) {
        stack.push(cur.left);
      }
    }
  }

  // 左-根-右
  _inOrder(node) {
    if (node) {
      this._inOrder(node.left);
      console.log(node.key);
      this._inOrder(node.right);
    }
  }

  inOrder() {
    this._inOrder(this.root);
  }

  inOrder2() {
    const stack = [];
    let cur = this.root;
    while (cur || stack.length > 0) {
      if (cur) {
        stack.push(cur);
        cur = cur.left;
      } else {
        cur = stack.pop();
        console.log(cur.key);
        cur = cur.right;
      }
    }
  }

  // 左-右-根
  _postOrder(node) {
    if (node) {
      this._postOrder(node.left);
      this._postOrder(node.right);
      console.log(node.key);
    }
  }

  postOrder() {
    this._postOrder(this.root);
  }

  // 层序遍历（广度优先遍历）
  levelOrder() {
    const queue = [];
    queue.push(this.root);
    while (queue.length > 0) {
      const cur = queue.shift();
      console.log(cur.key);
      if (cur.left) {
        queue.push(cur.left);
      }
      if (cur.right) {
        queue.push(cur.right);
      }
    }
  }

  _toString(node, depth, strArr) {
    if (!node) {
      strArr.push('--'.repeat(depth) + 'null\n');
      return;
    }
    strArr.push('--'.repeat(depth) + node.key + '\n');
    this._toString(node.left, depth + 1, strArr);
    this._toString(node.right, depth + 1, strArr);
  }

  toString() {
    const strArr = [];
    this._toString(this.root, 0, strArr);
    return strArr.join('');
  }

  * _keys(node, lo, hi) {
    if (!node) return;

    const key = node.key;

    if (lo < key) {
      // 左子树中有更小的？
      yield* this._keys(node.left, lo, hi);
    }
    // 范围内的
    if (key >= lo && key <= hi) {
      yield key;
    }
    if (hi > key) {
      // 右子树中有更大的？
      yield* this._keys(node.right, lo, hi);
    }
  }

  /**
   * 参考了中序遍历的非递归实现
   */
  * _keys2(node, lo, hi) {
    if (!node) return;
    const stack = [];
    let cur = this.root;
    while (cur || stack.length > 0) {
      if (cur) {
        stack.push(cur);
        if (cur.key < lo) {
          cur = null; // 不需要扫描左子树了，手动命中else(尝试向右子树查找)
        } else {
          cur = cur.left;
        }
      } else {
        cur = stack.pop();
        const key = cur.key;
        // 已经大于右边界了
        if (key > hi) {
          break;
        }
        if (key >= lo) {
          yield key;
        }
        cur = cur.right;
      }
    }
  }

  * keys() {
    if (this.isEmpty()) return;
    return yield* this._keys(this.root, this.min, this.max);
  }

  * rangeKeys(lo, hi) {
    if (this.isEmpty() || hi < lo) return;
    return yield* this._keys(this.root, lo, hi);
  }

  // 注意根结点的深度为 0，树的高度等于整棵树中最大的深度值
  _height(node) {
    // 当 x 等于 null 时，说明它是叶子结点的左/右子树，应该返回 0-1=-1。
    return node ? Math.max(this._height(node.left), this._height(node.right)) + 1 : -1;
  }

  get height() {
    // 所需空间为线性级别，查询耗时为常数
    return BST._height(this.root);
    // 所需空间和树高成正比，查询耗时为线性级别
    // return this._height(this.root);
  }

  randomKey() {
    // const k = Math.floor(Math.random() * this.size);
    // return this.select(k);
    assert(!this.isEmpty(), 'random key bst is empty');
    const h = parseInt(Math.log2(this.size));
    let cur = this.root;
    let steps = Math.floor(Math.random() * (h + 1));
    while (steps--) {
      if (Math.random() > 0.5) {
        if (!cur.left) {
          return cur.key;
        }
        cur = cur.left;
      } else {
        if (!cur.right) {
          return cur.key;
        }
        cur = cur.right;
      }
    }
    return cur.key;
  }

  _maxLevel(node) {
    return !node ? 0 : Math.max(this._maxLevel(node.left), this._maxLevel(node.right)) + 1;
  }

  maxLevel() {
    return this._maxLevel(this.root);
  }

  isAllElementsNull(nodes) {
    return nodes.every(x => !x);
  }

  printWhiteSpace(count) {
    if (count > 0) {
      this.print(' '.repeat(count));
    }
  }

  print(str) {
    process.stdout.write(str + '');
  }

  _draw(nodes, level, maxLevel) {
    if (this.isEmpty() || this.isAllElementsNull(nodes)) {
      return;
    }
    const floor = maxLevel - level; // 层数越深，前面的空格越少
    const endgeLines = parseInt(2 ** (Math.max(0, floor - 1)));
    const firstSpaces = 2 ** floor - 1;
    const betweenSpaces = 2 ** (floor + 1) - 1;

    // 打印行开头的空格字符
    this.printWhiteSpace(firstSpaces);

    // 打印这一层级的所有元素,并构造下一层的完全二叉树
    const newNodes = [];
    for (const node of nodes) {
      if (node) {
        this.print(node.key);
        newNodes.push(node.left);
        newNodes.push(node.right);
      } else {
        this.printWhiteSpace(1);
        newNodes.push(null);
        newNodes.push(null);
      }
      this.printWhiteSpace(betweenSpaces); // 同一层2个节点之间的空格
    }
    this.print('\n');

    // 打印2层之间的空格,斜杠和反斜杠
    for (let i = 1; i <= endgeLines; i++) {
      for (let j = 0; j < nodes.length; j++) {
        this.printWhiteSpace(firstSpaces - i); // 每一行，前面的空格数减少1

        const node = nodes[j];
        // 完全二叉树对应的这个位置的节点不存在，打印"合适"的空格
        if (!node) {
          this.printWhiteSpace(2 * endgeLines + i + 1);
          continue;
        }
        // 有左孩子，打印 /
        if (node.left) {
          this.print('/');
        } else {
          this.printWhiteSpace(1);
        }

        this.printWhiteSpace(2 * i - 1); // 1,3,5,7...

        if (node.right) {
          this.print('\\'); // 注意转义
        } else {
          this.printWhiteSpace(1);
        }
        this.printWhiteSpace(2 * endgeLines - i);
      }
      this.print('\n');
    }

    this._draw(newNodes, level + 1, maxLevel);
  }

  // https://stackoverflow.com/questions/4965335/how-to-print-binary-tree-diagram
  draw() {
    const maxLevel = this.maxLevel();
    this._draw([this.root], 1, maxLevel);
  }

  // 任意一个节点距离根节点的路径长度
  pathSumToRoot(node){
    let cur = this.root;
    let key = node.key;
    let sum = 0;
    while (cur) {
      const curKey = cur.key;
      if (key === curKey) {
        return sum + 1;
      }
      sum++;
      if (key < curKey) {
        cur = cur.left;
      } else {
        cur = cur.right;
      }
    }
    return 0; // 没找到
  }

  get pathSum(){
    // 广度优先，针对每个节点，计算pathSum
    const queue = [this.root];
    let sum = 0;
    while (queue.length > 0) {
      const cur = queue.shift();
      sum += this.pathSumToRoot(cur);
      if (cur.left) {
        queue.push(cur.left);
      }
      if (cur.right) {
        queue.push(cur.right);
      }
    }
    return sum;
  }

  static _isBST(node, min, max) {
    if (!node) return true;
    if (node.key <= min || node.key >= max) return false;
    return BST._isBST(node.left, min, node.key) && BST._isBST(node.right, node.key, max);
  }

  static isBST(node) {

    return BST._isBST(node, Number.MIN_SAFE_INTEGER, Number.MAX_SAFE_INTEGER);

    // 以下是错误的实现

//       3
//     /   \
//    2     5
//   /  \
//  1    4

// 左子树中不应该有节点4，下面的算法只考虑了局部

    // if (!node) return true;
    // if (node.left && node.left.key >= node.key) return false;
    // if (node.right && node.right.key <= node.key) return false;
    // return BST.isBinaryTree(node.left) && BST.isBinaryTree(node.right);
  }
}
```

## 插入和查找

![二叉查找树中的查找](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/bst_search.jpg)

可以将*递归调用前*的代码想象成*沿着树向下*走：它会将给定的键和每个节点的键相比较并根据结果向左或者向右移动到下一个节点；可以将*递归调用后*的代码想象成*沿着树向上爬*。归于get操作这对应着一系列的返回指令（return），但对于put方法，这意味着重置搜索路径上每个父节点指向子节点的链接，并增加路径上每个节点中计数器的值。在一个简单的BST中，唯一的新链接就是在最底层指向新节点的链接，重置更上层的链接可以通过比较语句来避免。同样，我们只需要将路径上每个节点中的计数器的值加1，但是我们使用了更加通用的代码：使之等于节点的所有子节点的计数器加1.

插入元素的过程有点像在链表中追加节点的递归实现：

```js
class Node {
  constructor(value, next) {
    this.value = value;
    this.next = next;
  }
}

// 向单链表中插入节点

/**
 * 非递归实现
 */
function linkedListInsert1(head, value) {
  if (!head) return new Node(value);
  let cur = head;
  while (cur.next) {
    cur = cur.next;
  }
  cur.next = new Node(value);
  return head;
}

/**
 * 递归实现
 */
function linkedListInsert2(head, value) {
  if (!head) return new Node(value);
  head.next = linkedListInsert2(head.next, value);
  return head;
}
```

BST和快排非常相似：树的根节点类似于快排中的标定点（左侧的都比它小，右侧的都比它大）

*新节点会连接到树底层的空链接上*，树的其他部分则不会改变，第一个插入的键是根节点，第二个被插入的键是根节点的2个子节点之一。因为只有查找和插入路径上的节点才会被访问，所以随着树的增长，被访问的节点占树的总结点的比例也会不断降低。

## 有序性相关的方法&删除操作

### max & min

如果根节点的左链接为空，那么最小节点就是根节点；如果左链接非空，则树中的最小键就是左子树的最小值。这句话天然具有递归定义；找到最大键同理。注意：最小值或者最大值不一定是叶子节点。

### ceil & floor

![计算floor的函数](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/bst_floor.jpg)

如果给定的键小于BST的根节点，那么小于等于key的最大键floor(key)一定在根节点的左子树中；如果给定的key大于BST的根节点，那么只有当右子树中存在小于等于key的节点的时候，小于等于key的最大键才会出现在右子树中，否则根节点就是最大键。上面的这段说明是floor方法的递归实现。

### select & rank

BST的选择操作和基于切分的[数组选择操作#对数组求select](quick_sort.md)类似。BST中每个节点的计数器count就是用来支持这个操作的。

假设我们想找到排名为k的键（即树中正好有k个小于它的键）。如果左子树中的节点数t大于k，那么我们就继续（递归地）在左子树中查找排名为k的键；如果t等于k，我们就返回根节点中的键；如果t小于k，我们我们就（递归地）在右子树中寻找排名为(k-t-1)的键。

![BST中的select](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/bst_select.jpg)

rank是select的逆方法，它会返回给定键的排名。它的实现和select类似：如果给定的键和根节点的键相等，我们就返回左子树中的节点总数t；如果给定的键小于根节点，返回该键在左子树中的排名（递归计算）；如果给定的键大于根节点返回t+1(根节点)加上它在右子树中的排名（递归计算）。

### deleteMin & deleteMax

BST中最难实现的是delete，作为热身可以先考虑deleteMin（删除最小键所对应的键值对）。和put方法一样，我们的递归方法接收一个指向节点的链接，这样就能方便地改变树的结构，将返回的链接赋值给作为参数的链接。对于deleteMn，我们要不断深入根节点的左子树直到遇到一个空链接，然后将指向该节点的链接指向该节点的右子树（只需在递归调用中返回它的右链接即可）。此时已经没有任何链接指向要删除的节点，因此会被gc。我们给出的标准递归代码在删除节点后会正确地设置它的父节点的链接并更新它到根节点的路径上的所有节点的计数器的值。

### delete

我们可以用类似的方式删除任意只有一个子节点（或者没有子节点）的节点，但是应该怎样删除一个拥有2个子节点的节点呢？删除之后我们要处理2颗子树，但是删除节点的父节点只有一条空出来的链接。

解决方案一：在删除节点x后用它的*后继结点*填补它的位置。因为x有一个右子节点，因此它的后继结点就是其右子树中的最小节点。这里的替换仍然能保持BST的性质，因为x.key和它的后继结点的键之间不存在其他的键。我们能用4个简单步骤完成x替换为它的后继结点的任务：

1. 将指向被删除节点的链接保存为t
2. 将x指向它的后继结点min(t.right)
3. 将x的右链接（原本指向一颗所有节点都大于x.key的BST）指向deleteMin(t.right),也就是在删除后所有节点仍然都大于x.key的子BST
4. 将x的左链接（本为空）设为t.left（其下所有键都小于被删除的节点和它的后继结点）

![BST中的删除操作](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/bst_delete.jpeg)

在递归删除后我们会修正被删除的节点的父节点的链接，并将此节点到根节点的路径上的所有节点的计数器减1（这里计数器的值仍然会被设置为其所有子树中的节点总数加1）。尽管这种方法能正确删除一个节点，它的一个缺陷是可能会在某些应用中产生性能问题。这个问题在于选择后继结点是一个随机的决定，且没有考虑树的对称性。事实上，我们也可以选择它的*前驱节点*。

> 如果一颗二叉树的一个节点有2个子节点，那么它的后继节点不会有左孩子，前驱节点不会有右孩子。

> 后继节点是右子树的最小值，根据定义右子树的最小值必然没有左孩子（如果存在左孩子则证明不是最小值）
> 前驱节点是左孩子的最大值，根据定义左孩子的最大值必然没有右孩子（如果存在右孩子则证明还不是最大值）

![delete方法并不符合交换律](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/exam_3_2_23.jpg)

delete方法并不符合交换律，也就是说先删除x后删除y和先删除y后删除x得到的结果不一样。

### 范围查找

要实现能够返回给定范围内键的keys方法，我们首先要实现一个遍历二叉树的基本方法，叫做*中序遍历*。要说明这个方法，我们先看看如何将BST中的所有键按照顺苏打印出来。我们应该先打印出根节点的左子树中的所有键（根据BST的定义，它们应该都小于根节点），然后打印根节点，最后打印右子树中的所有键（大于根节点）。

```js
function print(node) {
  if (!node) return;
  print(node.left);
  console.log(node.key);
  print(node.right);
}
```

要实现keys方法，我们可以修改上述代码，将所有落在给定范围内的键加入到一个队列中，并跳过那些不可能含有所查键的子树。

## BST的遍历

前序遍历是最常用的，中序遍历是有序的。后序遍历有其特定的应用场景：先处理完孩子节点后再处理根节点，如BST的释放内存。前面的几种是DFS，而层序遍历是BFS，BFS算法常用于无权图的最短路径问题。

# 关于BST的其他话题

## 构造最优和最坏的BST

我们认为最优的BST是树的深度最小，而最坏的BST为树的深度最大。当节点的数目可以构成一颗满二叉树（2^n-1）的时候，最优的BST，否则可能最优情况可能有多种，而最坏的情况始终有多种（实际上为2^(n-1)，n为节点数目）。例如当节点数目为2^3-1 = 7的时候最优的BST只有一种：

```bash
     4
   /   \
  2     6
 / \   / \
1   3 5   7
```

当节点个数不能组成完全二叉树的时候，最优情况可能有多种：

```bash
  2
 / \
1   3
     \
      4
      
  2
 / \
1   4
   /
  3

  3
 / \
1   4
 \
  2
  
  3
 / \
2   4
 \
  1

1
 \
  3
 / \
2   4

    4
   /
  2
 / \
1   3            
```

![构造最坏情况下的BST](https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/exam3_2_2%263_2_3.jpg)

## 给定N个值，生成所有的BST

方法也很简单，生成数组的全排列，按照排列的顺序依次进行put，生成BST，最后对生成的BST进行去重：

```js
const _ = require('lodash');

function getAllAssignment(arr) {
  const res = [];

  function dfs(result) {
    if (result.length === arr.length) {
      res.push(result);
      return result;
    }
    for (let i = 0; i < arr.length; i++) {
      if (result.indexOf(arr[i]) === -1) {
        dfs(result.concat(arr[i]));
      }
    }
  }

  dfs([]);
  return res;
}

class Node {
  constructor(key) {
    this.key = key;
    this.left = null;
    this.right = null;
  }
}

class BST {
  constructor() {
    this.root = null;
  }

  _put(node, key) {
    if (!node) return new Node(key);
    if (key < node.key) {
      node.left = this._put(node.left, key);
    } else {
      node.right = this._put(node.right, key);
    }
    return node;
  }

  put(key) {
    this.root = this._put(this.root, key);
  }
  
  toString() {
    return JSON.stringify(this.root);
  }
}

const res = getAllAssignment([1, 2, 3]);
const bsts = [];
for (const seq of res) {
  const bst = new BST();
  for (let item of seq) {
    bst.put(item);
  }
  bsts.push(bst);
}

const uniq = _.uniqBy(bsts,bst => bst.toString());
```

## 二分搜索树的非递归实现

```js
class BST {

  constructor() {
    this.root = null;
  }

  get min() {
    let cur = this.root;
    while (cur) {
      if (!cur.left) {
        break;
      }
      cur = cur.left;
    }
    return cur.key;
  }

  get max() {
    let cur = this.root;
    while (cur) {
      if (!cur.right) {
        break;
      }
      cur = cur.right;
    }
    return cur.key;
  }

  get size() {
    return this.root.count;
  }

  put(key, value) {
    if (!this.root) {
      this.root = new Node(key, value, 1);
      return;
    }
    let cur = this.root;
    let parent = null;
    const path = []; // 保存搜索路径
    while (cur) {
      parent = cur;
      path.push(cur);
      if (key === cur.key) {
        cur.value = value;
        return;
      }
      if (key < cur.key) {
        cur = cur.left;
      } else {
        cur = cur.right;
      }
    }
    const node = new Node(key, value, 1);
    if (key < parent.key) {
      parent.left = node;
    } else {
      parent.right = node;
    }
    // 更新搜索路径上每个节点的计数器
    for (const node of path) {
      node.count++;
    }
  }

  get(key) {
    let cur = this.root;
    while (cur) {
      if (cur.key === key) {
        return cur.value;
      }
      if (key < cur.key) {
        cur = cur.left;
      } else {
        cur = cur.right;
      }
    }
    return null;
  }

  * keys() {
    const stack = [];
    let cur = this.root;
    while (cur || stack.length > 0) {
      if (cur) {
        stack.push(cur); // 将每个中间节点入栈
        cur = cur.left;
      } else {
        cur = stack.pop();
        yield cur.key;
        cur = cur.right;
      }
    }
  }

  floor(key) {
    let cur = this.root;
    let pathSums = null;
    while (cur) {
      if (key === cur.key) {
        return key;
      }
      if (key < cur.key) {
        cur = cur.left;
      } else {
        pathSums = cur.key;
        cur = cur.right;
      }
    }
    return pathSums;
  }

  ceil(key) {
    let cur = this.root;
    let pathSums = null;
    while (cur) {
      if (key === cur.key) {
        return key;
      }
      if (key > cur.key) {
        cur = cur.right;
      } else {
        pathSums = cur.key;
        cur = cur.left;
      }
    }
    return pathSums;
  }

  rank(key) {
    let r = 0;
    let cur = this.root;
    while (cur) {
      if (key < cur.key) {
        cur = cur.left;
      } else if (key > cur.key) {
        r += (1 + this.left.count);
        cur = cur.right;
      } else {
        r += this.left.count;
        return r;
      }
    }
    return r;
  }

  select(k) {
    let cur = this.root;
    while (cur) {
      const sz = this.left.count;
      if (k < sz) {
        cur = cur.left;
      } else if (k > sz) {
        cur = cur.right;
        k = k - sz - 1;
      } else {
        return sz;
      }
    } 
    return null;
  }
}
```

## 使用一组有序数组构造一颗完美平衡的BST

使用有序数组构造和二分搜索等价的BST(在这棵树中查找任意键所产生的比较序列在和用这个数组中使用二分查找所使用的序列相同),参见https://leetcode.com/problems/convert-sorted-array-to-binary-search-tree/discuss/35220/My-Accepted-Java-Solution/225172/

```js
class Node {
  constructor(key) {
    this.key = key;
    this.left = null;
    this.right = null;
  }
}

function sortedArrayToBST(nums) {
  function _helper(lo, hi) {
    if (lo > hi) {
      return null;
    }
    const mid = parseInt((lo + hi) / 2);
    const node = new Node(nums[mid]);
    node.left = _helper(lo, mid - 1);
    node.right = _helper(mid + 1, hi);
    return node;
  }

  return _helper(0, nums.length - 1);
}
```

## 判断一棵树是否是BST

[leetcode.No98.validate-binary-search-tree](https://leetcode-cn.com/problems/validate-binary-search-tree/)

一个常见的错误代码：

```js
function _isBST(node, min, max) {
    if (!node) return true;
    if (node.key <= min || node.key >= max) return false;
    return BST._isBST(node.left, min, node.key) && BST._isBST(node.right, node.key, max);
}

function isBinaryTree(node) {

    return BST._isBST(node, Number.MIN_SAFE_INTEGER, Number.MAX_SAFE_INTEGER);

    // 以下是错误的实现

    //       3
    //     /   \
    //    2     5
    //   /  \
    //  1    4

    // 左子树中不应该有节点4，下面的算法只考虑了局部

    // if (!node) return true;
    // if (node.left && node.left.key >= node.key) return false;
    // if (node.right && node.right.key <= node.key) return falsisBinaryTreenaryTree(node.right);
}
```

上面的错误解只考虑了局部范围内3个节点之间的大小关系，忽略了左子树中的*所有*节点小于根节点，右子树中的*所有*节点大于根节点。我们可以在递归调用的时候传入上下边界，需要将本层的lo和hi继续传递下去。
