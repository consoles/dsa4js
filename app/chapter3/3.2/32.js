const BST = require('../BST');
const TreeNode = require('../TreeNode');

class BST32 extends BST {
  // 前序遍历
  _hasNoDuplicates(node, set) {
    if (!node) return true;
    const key = node.key;
    if (set.has(key)) return false;
    set.add(key);
    const ret = this._hasNoDuplicates(node.left, set);
    if (!ret) return false;
    return this._hasNoDuplicates(node.right, set);
  }

  hasNoDuplicates(node) {
    return this._hasNoDuplicates(node, new Set());
  }

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

  isBinaryTree(node) {
    if (!node) return true;
    if (node.count !== BST._size(node.left) + BST._size(node.right) + 1) {
      return false;
    }
    return this.isBinaryTree(node.left) && this.isBinaryTree(node.right);
  }

  isBST(node) {
    if (!this.isBinaryTree(node)) return false;
    if (!this.isOrdered(node, this.min, this.max)) return false;
    if (!this.hasNoDuplicates(node)) return false;
    return true;
  }
}

const bst = new BST32();

const root = new TreeNode(4,5);
root.left = new TreeNode(2,3);
root.right = new TreeNode(5,1);
root.left.left = new TreeNode(1,1);
root.left.right = new TreeNode(3,1);

// 关联root和bst
bst.root = root;

const ret = bst.isBST(root);
debugger;
