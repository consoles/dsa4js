class Node {
  constructor(value) {
    this.value = value;
    this.left = null;
    this.right = null;
    this.parent = null;
  }
}

class MaxPQ {
  constructor() {
    this.sz = 0;
    this.root = null;
    this.last = null;
  }

  static less(node1, node2) {
    return node1.value < node2.value;
  }

  /**
   * 交换2个节点的值
   * @param node1
   * @param node2
   */
  static swap(node1, node2) {
    [node1.value, node2.value] = [node2.value, node1.value];
  }

  static _swim(node) {
    while (node.parent) {
      if (MaxPQ.less(node.parent, node)) {
        MaxPQ.swap(node.parent, node);
        node = node.parent;
      } else {
        break;
      }
    }
  }

  insert(value) {
    const node = new Node(value);
    this.sz++;
    // 堆为空
    if (!this.last) {
      this.last = this.root = node;
      return;
    }
    // 只有一个节点
    if (this.last === this.root) {
      node.parent = this.root;
      this.root.left = node;
      this.last = node;
      MaxPQ._swim(node);
      return;
    }
    // 定位到最后一个节点的父节点
    let parent = this.last.parent;
    // 右子节点为空，插入到右子节点
    if (!parent.right) {
      node.parent = parent;
      parent.right = node;
    } else {
      // 当前子树已满，需要向上回溯
      // 找到下一个子树（回溯的时候从左子树回溯上去）
      while (parent !== this.root) {
        if (parent !== parent.parent.right) {
          break;
        }
        parent = parent.parent;
      }
      // 已经是满二叉树
      if (parent === this.root) {
        // 一路向左，进入下一层
        while (parent.left) {
          parent = parent.left;
        }
        node.parent = parent;
        parent.left = node;
      } else {
        // 不是满二叉树
        // 向左子树移动，再一路向左
        parent = parent.parent.right;
        while (parent.left) {
          parent = parent.left;
        }
        node.parent = parent;
        parent.left = node;
      }
    }
    this.last = node;
    MaxPQ._swim(node);
  }

  static _sink(node) {
    while (node.left) {
      let toExch = node.left;
      if (node.right && MaxPQ.less(node.left, node.right)) toExch = node.right;
      if (MaxPQ.less(node, toExch)) {
        MaxPQ.swap(node, toExch);
        node = toExch;
      } else {
        break;
      }
    }
  }

  delMax() {
    const ret = this.root.value;
    MaxPQ.swap(this.root, this.last);

    if (this.sz === 2) {
      this.root.left = null;
      this.last = this.root;
      this.sz--;
      return ret;
    }
    if (this.sz === 1) {
      this.last = null;
      this.root = null;
      this.sz--;
      return ret;
    }
    let newLast = this.last;
    if (newLast === this.last.parent.right) {
      newLast = this.last.parent.left;
    } else {
      // 找到上一颗子树
      while (newLast !== this.root) {
        if (newLast !== newLast.parent.left) {
          break;
        }
        newLast = newLast.parent;
      }
      // 已经是满二叉树
      if (newLast === this.root) {
        // 一路向右，回到上一层
        while (newLast.right) {
          newLast = newLast.right;
        }
      } else {
        // 不是满二叉树
        // 向左子树移动，再一路向右
        newLast = newLast.parent.left;
        while (newLast.right) {
          newLast = newLast.right;
        }
      }
    }

    // 删除最后一个节点
    if (this.last.parent.left === this.last) {
      this.last.parent.left = null;
    } else {
      this.last.parent.right = null;
    }

    MaxPQ._sink(this.root);
    // 指向新的最后一个节点
    this.last = newLast;
    this.sz--;
    return ret;
  }

  isEmpty() {
    return this.sz === 0;
  }
}

const arr = [2, 1, 4, 5, 3, 7, -1, 0, 222];
const q = new MaxPQ();
for (const num of arr) {
  q.insert(num);
}
debugger;
while (!q.isEmpty()) {
  console.log(q.delMax());
}
