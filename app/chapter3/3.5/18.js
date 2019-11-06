// 多重集合一个key对应多个value，此处的有序应该只对key进行排序

/**
 * 无序多重集合（基于拉链法符号表）
 */
class UnorderedMultiSet {

  constructor(M = 16) {
    this.N = 0;
    this.M = M;
    this._keys = new Array(M);
  }

  _resize(cap) {
    const s = new UnorderedMultiSet(cap);
    for (let i = 0; i < this.M; i++) {
      if (this._keys[i]) {
        s.add(this._keys[i]);
      }
    }
    this._keys = s._keys;
    this.M = cap;
  }

  _hash(key) {
    return (key & 0x7fffffff) % this.M;
  }

  _get(key) {
    for (let i = this._hash(key); this._keys[i]; i = (i + 1) % this.M) {
      if (this._keys[i] === this._keys[i]) {
        return this._keys[i];
      }
    }
    return null;
  }

  contains(key) {
    return this._get(key) !== null;
  }

  add(key) {
    if (2 * this.N >= this.M) {
      this._resize(2 * this.M);
    }
    let i = this._hash(key);
    for (; this._keys[i]; i = (i + 1) % this.M) {
    }
    this._keys[i] = key;
    this.N++;
  }

  delete(key) {
    if (!this.contains(key)) return;
    let i = this._hash(key);
    while (this._keys[i] === key) {
      this._keys[i] = null;
      this.N--;
      i= (i+1) % this.M;
    }
    // 后面的元素要向移动，因为被删除的元素会留下空位，再次get的时候会返回null

    // 如果遇到空位，则空位后面的必然是哈希值计算正确的
    while (this._keys[i]) {
      const keyToRedo = this._keys[i];
      // re-hash
      this._keys[i] = null;
      this.N--;
      // 由于允许出现重复元素，例如 当前数组为 2,5,3,2，后面可能出现应该被删除的元素
      if (keyToRedo !== key) {
        this.add(keyToRedo);
      }
      i = (i + 1) % this.M;
    }
    if (this.N > 0 && this.N === parseInt(this.N / 8)) {
      this._resize(parseInt(this.N / 2));
    }
  }

  isEmpty() {
    return this.N === 0;
  }

  get size() {
    return this.N;
  }

  * keys() {
    for (const key of this._keys) {
      if (key) {
        yield key;
      }
    }
  }
}

class Node {
  constructor(key) {
    this.key = key;
    this.left = null;
    this.right = null;
    this.count = 1;
    this.repeatCount = 1;
  }
}

/**
 * 有序多重集合（基于BST）
 */
class OrderedMultiSet {
  constructor() {
    this.root = null;
  }

  _get(node, key) {
    if (!node) return null;
    if (node.key === key) return node.value;
    return key < node.key ? this._get(node.left, key) : this._get(node.right, key);
  }

  get(key) {
    return this._get(this.root, key);
  }

  contains(key) {
    return this.get(key) !== null;
  }

  _add(node, key) {
    if (!node) {
      this.N++;
      return new Node(key);
    }
    if (key < node.key) {
      node.left = this._add(node.left, key);
    } else if (key > node.key){
      node.right = this._add(node.right, key);
    } else {
      node.repeatCount++;
    }
    this._updateNodeCount(node);
    return node;
  }

  add(key) {
    this.root = this._add(this.root, key);
  }

  _min(node) {
    return node && node.left ? this._min(node.left) : node;
  }

  _deleteMin(node) {
    if (!node) return null;
    if (!node.left) return node.right;
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
      if (!node.left) {
        return node.right;
      }
      if (!node.right) {
        return node.left;
      }
      const t = node;
      node = this._min(node.right); // 使用右子树的最小值替代当前节点
      node.right = this._deleteMin(t.right);
      node.left =  t.left;
    }
    this._updateNodeCount(node);
    return node;
  }

  delete(key) {
    this.root = this._delete(this.root, key);
  }

  isEmpty() {
    return this.size === 0;
  }

  _size(node) {
    return node ? node.count + node.repeatCount - 1: 0;
  }

  _updateNodeCount(node){
    return node.count = this._size(node.left) + this._size(node.right) + 1;
  }

  get size() {
    return this._size(this.root);
  }

  * _keys(node) {
    if (node) {
      yield* this._keys(node.left);
      for (let i = 0;i < node.repeatCount;i++) {
        yield node.key;
      }
      yield* this._keys(node.right);
    }
  }

  // 中序遍历
  * keys() {
    return yield* this._keys(this.root);
  }
}

// const s1 = new UnorderedMultiSet();
//
// for (let int of [8, 5, 3, 4, 3, 2, 7, 4, 1, 8, 9]) {
//   s1.add(int);
// }
//
// console.log(s1.size);
// for (const key of s1.keys()) {
//   console.log(key);
// }
// console.log(s1.contains(0), s1.contains(2));
// s1.delete(3);
// for (const key of s1.keys()) {
//   console.log(key);
// }
// console.log(s1.size);

const s1 = new OrderedMultiSet();

for (let int of [8, 5, 3, 4, 3, 2, 7, 4, 1, 8, 9]) {
  s1.add(int);
}

console.log(s1.size);
for (const key of s1.keys()) {
  console.log(key);
}
console.log(s1.contains(0), s1.contains(2));
s1.delete(3);
for (const key of s1.keys()) {
  console.log(key);
}
console.log(s1.size);
