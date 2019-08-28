const BST = require('../BST');

class BST29 extends BST {
  static isBST(node) {
    if (!node) return true;
    if (node.count !== BST._size(node.left) + BST._size(node.right) + 1) {
      return false;
    }
    return BST29.isBST(node.left) && BST29.isBST(node.right);
  }
}
