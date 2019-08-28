class Node {
  constructor(key, value, count = 1, depthSum) {
    this.key = key;
    this.value = value;
    this.count = count;
    this.depthSum = depthSum;
  }
}

class BST {
  constructor() {
    this.root = null;
  }

  put(key, value) {
    this.root = this._put(this.root, key, value, 0);
  }

  static _size(node) {
    return node ? node.count : 0;
  }

  get size() {
    return BST._size(this.root);
  }

  _put(node, key, value, depth) {
    if (!node) return new Node(key, value, 1, depth);
    if (key === node.key) {
      node.value = value;
    } else if (key < node.key) {
      node.left = this._put(node.left, key, value, depth + 1);
    } else {
      node.right = this._put(node.right, key, value, depth + 1);
    }
    node.count = BST._size(node.left) + BST._size(node.right) + 1;
    node.depthSum = depth;
    if (node.left) {
      node.depthSum += node.left.depthSum;
    }
    if (node.right) {
      node.depthSum += node.right.depthSum;
    }
    return node;
  }

  _depthSum(node) {
    // 左子树的深度和 + 右子树的深度和 + 左子树的结点个数 + 右子树的结点个数
    return !node ? 0 : this._depthSum(node.left) + this._depthSum(node.right) + node.count - 1;
  }

  // 平均查找次数 = 树所有结点的深度之和 / 结点个数 + 1。
  get avgCompares() {
    // 递归实现：耗时线性级别，空间和树高成正比
    // return this._depthSum(this.root) / this.size + 1;
    // 在node节点中维护一个depthSum变量，所需空间为线性级别，耗时常数级别
    return this.root.depthSum / this.size + 1;
  }

  // https://alg4.ikesnowy.com/3-2-8/;
  static optCompares(n) {
    // 完全二叉树 = 满二叉树 + 多余节点
    const l = Math.floor(Math.log2(n + 1)); // 满二叉树的层数
    const extraNodeCount = n + 1 - Math.floor(2 ** l); // 多余节点数
    const depthSum = extraNodeCount * l + (l - 2) * 2 ** l + 2;
    return depthSum / n + 1;
  }

}

const a = BST.optCompares(13);
debugger;
