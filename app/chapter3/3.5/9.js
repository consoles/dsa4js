const _ = require('lodash');

class Node {
  constructor(key, value) {
    this.key = key;
    this.value = [value];
    this.left = null;
    this.right = null;
    this.count = 1;
  }
}

class Node2 extends Node {
  constructor(key, value) {
    super(key, value);
    this.value = value;
  }
}

// 支持重复元素的BST

class BST {
  constructor() {
    this.root = null;
  }

  _put(node, key, value) {
    if (!node) {
      return new Node(key, value);
    }
    if (key === node.key) {
      node.value.push(value);
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
    if (key === node.key) return _.sample(node.value);
    return key < node.key ? this._get(node.left, key) : this._get(node.right, key);
  }

  get(key) {
    return this._get(this.root, key);
  }

  _min(node) {
    return (!node || !node.left) ? node : this._min(node.left);
  }

  _deleteMin(node) {
    if (!node) return null;
    if (!node.left) return node.right;
    node.left = this._deleteMin(node.left);
  }

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
      // 被删除的节点既有左子树，又有右子树，使用被删除节点的后继结点替换删除的节点（右子树的最小值）
      node = this._min(t.right);
      node.right = this._deleteMin(t.right);
      node.left = t.left;
    }
    return node;
  }

  delete(key) {
    this.root = this._delete(this.root, key);
  }

  // 先根遍历
  _toString(node, chars = []) {
    if (node) {
      chars.push(`${node.key} => ${JSON.stringify(node.value)}`);
      this._toString(node.left, chars);
      this._toString(node.right, chars);
    }
  }

  toString() {
    const chars = [];
    this._toString(this.root, chars);
    return chars.join(',');
  }
}

// 支持重复元素的BST
// 左子树小于等于，右子树大于
class BST2 {
  constructor() {
    this.root = null;
  }

  _put(node, key, value) {
    if (!node) {
      return new Node2(key, value);
    }
    if (key < node.key) {
      node.left = this._put(node.left, key, value);
    } else if (key > node.key){
      node.right = this._put(node.right, key, value);
    } else  {
      node.count += 1;
    }
    return node;
  }

  put(key, value) {
    this.root = this._put(this.root, key, value);
  }

  _min(node) {
    if (!node || !node.left) return node;
    return this._min(node.left, node.key);
  }

  _deleteMin(node) {
    if (!node) return null;
    if (!node.left) return node.right;
    node.left = this._deleteMin(node.left, node.key);
    return node;
  }

  _delete(node, key) {
    if (!node) return null;

    if (key < node.key) {
      node.left = this._delete(node.left, key);
    } else if (key > node.key) {
      node.right = this._delete(node.right, key);
    } else {

      if (!node.left) return node.right;
      if (!node.right) {
        if (node.left && node.left.key === key) return null;
        return node.left;
      }

      const t = node;
      node = this._min(node.right);
      node.right = this._deleteMin(t.right);
      if (t.left && t.left.key === key) {
        node.left = null;
      } else {
        node.left = t.left;
      }
    }
    return node;
  }

  delete(key) {
    this.root = this._delete(this.root, key);
  }

  _get(node, key) {
    if (!node) return null;
    if (key === node.key) return node.value;
    return key < node.key ? this._get(node.left, key) : this._get(node.right, key);
  }

  get(key) {
    return this._get(this.root, key);
  }

  _toString(node, chars = []) {
    if (node) {
      const values = new Array(node.count).fill(node.value);
      chars.push(`${node.key} => ${JSON.stringify(values)}`);
      this._toString(node.left, chars);
      this._toString(node.right, chars);
    }
  }

  toString() {
    const chars = [];
    this._toString(this.root, chars);
    return chars.join(',');
  }
}

const kvs = [
  [0, 'a'],
  [0, 'b'],
  [2, 'c'],
  [2, 'd'],
  [4, 'e'],
  [5, 'f'],
];

const bst = new BST2();
for (const [k, v] of kvs) {
  bst.put(k, v);
}

console.log(bst.toString());
console.log(bst.get(0));
console.log(bst.get(0));
bst.delete(0);
console.log(bst.toString());
