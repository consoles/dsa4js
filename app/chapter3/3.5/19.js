const _ = require('lodash');

/**
 * 一对多节点类
 */
class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = [value];
    this.next = next;
  }
}

/**
 * 支持重复元素的顺序查找表
 */
class SST {
  constructor() {
    this.head = null;
    this.size = 0;
  }

  put(key, value) {
    if (this.head) {
      let cur = this.head;
      while (cur) {
        if (cur.key === key) {
          this.size++;
          cur.value.push(value);
          return;
        }
        cur = cur.next;
      }
    }
    this.size++;
    this.head = new Node(key, value, this.head);
  }

  get(key) {
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        return _.sample(cur.value);
      }
      cur = cur.next;
    }
    return null;
  }

  getNode(key) {
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        return cur;
      }
    }
    return null;
  }

  delete(key) {
    let prev = null;
    let cur = this.head;

    while (cur) {
      if (cur.key === key) {
        break;
      }
      prev = cur;
      cur = cur.next;
    }
    if (!cur) return;
    // 删除的是头结点
    if (!prev) {
      this.head = cur.next;
    } else {
      prev.next = cur.next;
    }
    this.size -= cur.value.length;
  }

  * keys() {
    let cur = this.head;
    while (cur) {
      yield cur.key;
      cur = cur.next;
    }
  }
}

/**
 * 基于拉链法的无序 一对多符号表
 */
class HashMultiST {
  constructor(M = 97) {
    this.M = M;
    const sts = [];
    for (let i = 0; i < M; i++) {
      sts.push(new SST());
    }
    this.sts = sts;
  }

  _hash(key) {
    return (key & 0x7fffffff) % this.M;
  }

  put(key, value) {
    const idx = this._hash(key);
    this.sts[idx].put(key, value);
  }

  contains(key) {
    return this.get(key) != null;
  }

  get(key) {
    return this.sts[this._hash(key)].get(key);
  }

  delete(key) {
    this.sts[this._hash(key)].delete(key);
  }

  get size() {
    let sz = 0;
    for (const st of this.sts) {
      sz += st.size;
    }
    return sz;
  }

  isEmpty() {
    return this.size === 0;
  }

  * keys() {
    for (const st of this.sts) {
      yield* st.keys();
    }
  }

  keyAll(key) {
    const node = this.sts[this._hash(key)].getNode(key);
    return node ? node.value : [];
  }
}

class TreeNode extends Node {
  constructor(key, value) {
    super(key, value);
    this.left = null;
    this.right = null;
    this.count = 1;
  }
}

/**
 * 基于BST的有序 一对多符号表
 */
class BSTMultiST {
  constructor() {
    this.root = null;
  }

  _size(node) {
    return node ? node.count : 0;
  }

  get size() {
    return this._size(this.root);
  }

  _updateNodeCount(node) {
    node.count = this._size(node.left) + this._size(node.right) + node.value.length;
  }

  _put(node, key, value) {
    if (!node) return new TreeNode(key, value);
    if (key < node.key) {
      node.left = this._put(node.left, key, value);
    } else if (key > node.key) {
      node.right = this._put(node.right, key, value);
    } else {
      node.count++;
      node.value.push(value);
    }
    this._updateNodeCount(node);
    return node;
  }

  put(key, value) {
    this.root = this._put(this.root, key, value);
  }

  _get(node, key) {
    if (!node) return;
    if (key === node.key) {
      return node;
    }
    return key < node.key ? this._get(node.left, key) : this._get(node.right, key);
  }

  get(key) {
    const node = this._get(this.root, key);
    return node ? _.sample(node.value) : null;
  }

  isEmpty() {
    return this.size === 0;
  }

  * _keys(node) {
    if (node) {
      yield* this._keys(node.left);
      yield node.key;
      yield* this._keys(node.right);
    }
  }

  * keys() {
    yield* this._keys(this.root);
  }

  keyAll(key) {
    const node = this._get(this.root, key);
    return node ? node.value : [];
  }

  _min(node) {
    return node && node.left ? this._min(node.left) : node;
  }

  _deleteMin(node) {
    if (!node.left) {
      return node.right;
    }
    node.left = this._deleteMin(node.left);
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
      if (!node.right) return node.left;

      const t = node;
      node = this._min(node.right);
      node.right = this._deleteMin(t.right);
      node.left = t.left;
    }
    this._updateNodeCount(node);
    return node;
  }

  delete(key) {
    this.root = this._delete(this.root, key);
  }
}

const kvPairs = [
  [0, 'a'],
  [0, 'b'],
  [0, 'c'],
  [0, 'd'],
  [0, 'e'],

  [1, 'A'],
  [2, 'B'],
  [1, 'C'],

  [3, '#']
];

const hashTable = new BSTMultiST();

for (const [key, value] of kvPairs) {
  hashTable.put(key, value);
}

console.log(hashTable.size);
for (const key of hashTable.keys()) {
  console.log(key, '->', hashTable.keyAll(key));
}

hashTable.delete(1);

console.log(hashTable.size);
for (const key of hashTable.keys()) {
  console.log(key, '->', hashTable.keyAll(key));
}
