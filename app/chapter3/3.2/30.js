const BST = require('../BST');

class BST30 extends BST {
  /**
   * 如果以该节点为根的子树中的所有节点都在min和max之间
   * min，max的确分别是树中的最小和最大的节点并且BST的有序性对树中的所有键都成立返回true，否则返回false
   */
  isOrdered(node, min, max) {
    if (!node) return true;
    if (min && node.key < min) return false;
    if (max && node.key > max) return false;
    return this.isOrdered(node.left, min, node.key) && this.isOrdered(node.right, node.key, max);
  }
}
