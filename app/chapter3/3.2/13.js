// https://algs4.cs.princeton.edu/32bst/NonrecursiveBST.java.html

class Node {
  constructor(key, value, count) {
    this.key = key;
    this.value = value;
    this.count = count;
    this.left = null;
    this.right = null;
  }
}

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
    let ret = null;
    while (cur) {
      if (key === cur.key) {
        return key;
      }
      if (key < cur.key) {
        cur = cur.left;
      } else {
        ret = cur.key;
        cur = cur.right;
      }
    }
    return ret;
  }

  ceil(key) {
    let cur = this.root;
    let ret = null;
    while (cur) {
      if (key === cur.key) {
        return key;
      }
      if (key > cur.key) {
        cur = cur.right;
      } else {
        ret = cur.key;
        cur = cur.left;
      }
    }
    return ret;
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

const bst = new BST();
const arr = [2, 1, 3, 4, 5];
for (const item of arr) {
  bst.put(item, 1);
}
// for (const key of bst.keys()) {
//   console.log(key);
// }
let s = bst.floor(0);
debugger;
