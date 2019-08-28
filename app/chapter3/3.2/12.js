const assert = require('assert');

class Node {
  constructor(key, value) {
    this.key = key;
    this.value = value;
    this.left = null;
    this.right = null;
  }
}

class BST {
  constructor() {
    this.root = null;
  }

  get size() {
    return this._size(this.root);
  }

  _size(node) {
    return node ? this._size(node.left) + this._size(node.right) + 1 : 0;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  isEmpty() {
    return this.size === 0;
  }

  _put(node, key, value) {
    if (!node) return new Node(key, value);
    if (key === node.key) {
      node.value = value;
    } else if (key < node.key) {
      node.left = this._put(node.left, key, value);
    } else {
      node.right = this._put(node.right, key, value);
    }
    return node;
  }

  put(key, value) {
    this.root = this._put(this.root, key, value);
  }

  _get(node, key) {
    if (!node) return null;
    if (key === node.key) return node.value;
    return key < node.key ? this._get(node.left, key) : this._get(node.right, key);
  }

  get(key) {
    return this._get(this.root, key);
  }

  delete(key) {
    this.root = this._delete(this.root, key);
  }

  _delete(node, key) {
    if (!node) return null;
    if (key < node.key) {
      node.left = this._delete(node.left, key);
    } else if (key > node.key) {
      node.right = this._delete(node.right, key);
    } else {
      if (!node.left) return node.right;
      if (!node.right) return node.left;

      // 使用t的后继节点（t的右子树的最小值）取代t
      const t = node;
      node = this._min(t.right);
      node.right = this._deleteMin(t.right);
      node.left = t.left;
    }
    return node;
  }

  _deleteMax(node) {
    if (!node.right) return node.left;
    node.right = this._deleteMax(node.right);
    return node;
  }

  deleteMax() {
    this._deleteMax(this.root);
  }

  _deleteMin(node) {
    if (!node.left) return node.right;
    node.left = this._deleteMin(node.left);
    return node;
  }

  deleteMin() {
    this._deleteMin(this.root);
  }

  _floor(node, key, best) {
    if (!node) return best;
    if (node.key === key) return key;
    return key < node.key ? this._floor(node.left, key, best) : this._floor(node.right, key, node.key);
  }

  floor(key) {
    return this._floor(this.root, key, null);
  }

  _ceil(node, key, best) {
    if (!node) return best;
    if (node.key === key) return key;
    return key > node.key ? this._ceil(node.right, key, best) : this._ceil(node.left, key, node.key);
  }

  ceil(key) {
    return this._ceil(this.root, key, null);
  }

  _max(node) {
    return !node.right ? node : this._max(node.right);
  }

  get max() {
    assert(!this.isEmpty(), 'bst must not empty!');
    return this._max(this.root).key;
  }

  _min(node) {
    return !node.left ? node : this._min(node.left);
  }

  get min() {
    assert(!this.isEmpty(), 'bst must not empty!');
    return this._min(node.left).key;
  }

  * _keys(node, lo, hi) {
    if (!node) return;
    const key = node.key;

    if (lo < key) {
      // 左子树中有更小的？
      yield* this._keys(node.left, lo, hi);
    }
    if (key >= lo && key <= hi) {
      yield* key;
    }
    if (hi > key) {
      yield* this._keys(node.right, lo, hi);
    }
  }

  * keys() {
    if (this.isEmpty()) return;
    return yield* this._keys(this.root, this.min, this.max);
  }

  * rangeKeys(lo, hi) {
    if (this.isEmpty() || lo > hi) return;
    return yield* this._keys(this.root, lo, hi);
  }
}
