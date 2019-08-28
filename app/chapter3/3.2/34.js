const BST = require('../BST');

const TreeNode = require('../TreeNode');

class Node extends TreeNode {
  constructor(key, value, count) {
    super(key, count);

    this.value = value;
    this.pred = null; // 前驱节点
    this.succ = null; // 后继节点
  }
}

class ThreadedST extends BST {

  _updatePredAndSucc(node) {
    // 后继节点：右子树的最小值
    const succ = this._min(node.right);
    // 前驱节点：左子树的最大值
    const pred = this._max(node.left);

    node.succ = succ;
    node.pred = pred;

    if (succ) {
      succ.pred = node;
    }
    if (pred) {
      pred.succ = node;
    }
  }

  _put(node, key, value,) {

    if (!node) {
      return new Node(key, value, 1);
    }

    if (key === node.key) {
      node.value = value;
    } else if (key < node.key) {
      node.left = this._put(node.left, key, value);
    } else {
      node.right = this._put(node.right, key, value);
    }

    this._updatePredAndSucc(node);
    BST._updateNodeCount(node);
    return node;
  }

  _delete(node, key) {
    if (!node) return null;
    const nodeKey = node.key;
    if (key < nodeKey) {
      node.left = this._delete(node.left,key);
    } else if (key > nodeKey) {
      node.right = this._delete(node.right,key);
    } else {
      if (!node.left) return node.right;
      if (!node.right) return node.left;

      // 使用后继节点替代当前节点
      const t = node;
      node = this._min(t.right);
      // 注意先后顺序！！
      node.right = this._deleteMin(t.right);
      node.left = t.left;
    }
    this._updatePredAndSucc(node);
    BST._updateNodeCount(node);
    return node;
  }

  deleteMax() {
    const maxNode = this._max(this.root);
    super.deleteMax();
    maxNode.pred.succ = null;
  }

  deleteMin() {
    const minNode = this._min(this.root);
    super.deleteMin();
    minNode.succ.pred = null;
  }

  /**
   * key的下一个键，如果key为最大键则返回空
   */
  next(key) {
    if (key === this.max) return null;
    const node = this._get(this.root,key);
    return node ? node.succ : null;
  }

  /**
   * key的上一个键，如果key为最小值则返回空
   */
  prev(key) {
    if (key === this.min) return null;
    const node = this._get(this.root,key);
    return node ? node.pred : null;
  }
}

const bst = new ThreadedST();

const keys = 'SEARCHEXAMPLE'.split('');

for (let i = 0; i < keys.length; i++) {
  bst.put(keys[i], i);
}
bst.delete('S');
debugger;
