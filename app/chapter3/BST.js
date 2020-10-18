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

  postOrder2(){
    let cur = this.root;
    const stack = [];
    const res = [];
    while(cur || stack.length) {
      if (cur) {
        stack.push(cur);
        res.unshift(cur.key);
        cur = cur.right;
      } else {
        const node = stack.pop();
        cur = node.left;
      }
    }
    return res;
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

  // 层序遍历求二叉树的最大深度,参见递归方法this._maxLevel，这两种方法可以解决leetcode104
  levelOrder2() {
    // 加上层次的概念
    let level = 0;
    const q = [this.root];
    while (q.length) {
      let count = q.length; // 这一层的节点的个数，需要从队列中处理完这一层的所有节点后，才能进行下一层的遍历
      while (count--) {
        const node = q.shift();
        if (node.left) q.push(node.left);
        if (node.right) q.push(node.right);
      }
      level++;
    }
    console.log('树的最大深度', level);
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
  pathSumToRoot(node) {
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

  get pathSum() {
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

module.exports = BST;

// const bst = new BST();
// const keys = 'SEARCHEXAMPLE'.split('');
// for (let i = 0; i < keys.length; i++) {
//   bst.put(keys[i], i);
// }

// bst.inOrder2();

// const keys = 'UAY'.split('');
// for (let i = 0; i < keys.length; i++) {
//   bst.put(keys[i], i);
// }
// debugger;

// bst.preOrder();
// console.log();
// bst.preOrder2();

// bst.levelOrder();

//
// for (const k of bst.keys()) {
//   console.log(k);
// }
//
// console.log(bst.height);

// const bst = new BST();
// const keys = 'EASYQUESTION'.split('');
// for (let i = 0; i < keys.length; i++) {
//   bst.put(keys[i], i);
// }
// console.log(bst.toString());
