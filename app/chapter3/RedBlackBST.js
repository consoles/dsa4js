const Node = require('./RedBlackTreeNode');

// 左倾红黑树（标准实现），
class RedBlackBST {
  constructor() {
    this.root = null;
  }

  get size() {
    return RedBlackBST._size(this.root);
  }

  static _isRed(node) {
    return node && node.color === Node.COLOR_RED; // 空节点定义为黑色节点
  }

  static _size(node) {
    return node ? node.count : 0;
  }

  * _keys(node) {
    if (!node) return;
    yield node.key;
    yield* this._keys(node.left);
    yield* this._keys(node.right);
  }

  * keys() {
    return yield* this._keys(this.root);
  }

  /**
   * 颜色翻转
   */
  static _flipColors(node) {
    node.color = Node.COLOR_RED;
    node.left.color = Node.COLOR_BLACK;
    node.right.color = Node.COLOR_BLACK;
  }

  /**
   *     node                  x
   *    /    \      左旋      /  \
   *   T1     x    ----->  node  T3
   *        /   \          /  \
   *       T2   T3        T1  T2
   */
  static _rotateLeft(node) {
    const x = node.right;
    node.right = x.left;
    x.left = node;
    x.color = node.color;
    node.color = Node.COLOR_RED;

    x.count = node.count;
    node.count = 1 + RedBlackBST._size(node.left) + RedBlackBST._size(node.right);

    return x;
  }

  /**
   *     node                  x
   *    /    \      右旋      /  \
   *   x     T2    ----->    y  node
   *  / \                       /  \
   * y  T1                     T1  T2
   */
  static _rotateRight(node) {
    const x = node.left;
    node.left = x.right;
    x.right = node;
    x.color = node.color;
    node.color = Node.COLOR_RED;

    x.count = node.count;
    node.count = 1 + RedBlackBST._size(node.left) + RedBlackBST._size(node.right);
    return x;
  }

  _put(node, key, value) {
    if (!node) return new Node(key, value); // 标准的插入操作，和父节点用红链接相连
    const nodeKey = node.key;
    if (key < nodeKey) {
      node.left = this._put(node.left, key, value);
    } else if (key > nodeKey) {
      node.right = this._put(node.right, key, value);
    } else {
      node.value = value;
    }
    // 注意下面的3个if条件是不互斥的，极端情况下3个if都会进入

    // 只有一条红色右链接
    if (RedBlackBST._isRed(node.right) && !RedBlackBST._isRed(node.left)) {
      node = RedBlackBST._rotateLeft(node);
    }
    // 两条连续的红色左链接
    if (RedBlackBST._isRed(node.left) && RedBlackBST._isRed(node.left.left)) {
      node = RedBlackBST._rotateRight(node);
    }
    // 左右子树都会红色节点，对应2-3树中临时的4节点，需要向上分裂变成3个2节点。
    // 只需要将该节点的左右子树改变为黑色，根节点变为红色节点（因为2-3树中它还要和它的父亲进行融合）
    if (RedBlackBST._isRed(node.left) && RedBlackBST._isRed(node.right)) {
      RedBlackBST._flipColors(node);
    }
    node.count = 1 + RedBlackBST._size(node.left) + RedBlackBST._size(node.right) + 1;
    return node;
  }

  put(key, value) {
    this.root = this._put(this.root, key, value);
    this.root.color = Node.COLOR_BLACK; // 根节点保持为黑色
  }

  _get(node, key) {
    if (!node) return null;
    const nodeKey = node.key;
    if (nodeKey === key) return node.value;
    return key < nodeKey ? this._get(node.left, key) : this._get(node.right, key);
  }

  get(key) {
    return this._get(this.root, key);
  }

  // TODO 比添加节点复杂，很琐碎
  delete(key) {

  }

  isEmpty() {
    return this.size === 0;
  }
}

module.exports = RedBlackBST;
