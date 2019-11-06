class Node {
  constructor(key, left, right) {
    this.key = key;
    this.left = left;
    this.right = right;
  }
}

class BinarySearchSet {
  constructor() {
    this.root = null;
    this.sz = 0;
  }

  _add(node, key) {
    if (!node) {
      this.sz++;
      return new Node(key);
    }
    if (node.key === key) return node;
    if (key > node.key) {
      node.right = this._add(node.right, key);
    } else {
      node.left = this._add(node.left, key);
    }
    return node;
  }

  add(key) {
    this.root = this._add(this.root, key);
  }

  _min(node) {
    if (!node || !node.left) return node;
    return this._min(node.left);
  }

  _deleteMin(node) {
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
      if (!node.left) return node.right;
      if (!node.right) return node.left;

      const t = node;

      // 用后继结点替换当前节点（右子树的最小值）
      node = this._min(node.right);
      node.right = this._deleteMin(t.right);
      node.left = t.left;
      this.sz--;
    }
  }

  delete(key) {
    this.root = this._delete(this.root, key);
  }

  _contains(node, key) {
    if (!node) return false;
    if (node.key === key) return true;
    return key < node.key ? this._contains(node.left, key) : this._contains(node.right, key);
  }

  contains(key) {
    return this._contains(this.root, key);
  }

  isEmpty() {
    return this.size === 0;
  }

  get size() {
    return this.sz;
  }

  toString() {
    const items = [];
    // 层序遍历(这里其实是BFS)
    const q = [this.root];
    while (q.length > 0) {
      const node = q.pop();
      if (node) {
        items.push(node.key);
      }
      if (node.left) {
        q.push(node.left);
      }
      if (node.right) {
        q.push(node.right);
      }
    }
    return `size = ${items.length} , { ${items.join(',')} }`;
  }
}

const s = new BinarySearchSet();

for (const item of [1, 1, 2, 3, 4, 5, 6]) {
  s.add(item);
}
console.log(s.toString());
s.delete(1);
console.log(s.toString());
console.log(s.contains(1), s.contains(2));
console.log(s.toString());
