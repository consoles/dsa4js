module.exports = class RedBlackTreeNode {
  constructor(key, value) {
    this.COLOR_RED = true;
    this.COLOR_BLACK = false;
    this.key = key;
    this.value = value;
    this.count = 1;
    this.color = RedBlackTreeNode.COLOR_RED; // 新创建的节点默认是红颜色，需要和红黑树对应的2-3树中的其他节点融合
    this.left = null;
    this.right = null;
  }

  static get COLOR_RED() {
    return true;
  }

  static get COLOR_BLACK() {
    return false;
  }
};
