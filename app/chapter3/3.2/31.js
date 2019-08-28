const BST = require('../BST');
const TreeNode = require('../TreeNode');

class BST31 extends BST {
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
}

const bst = new BST31();

const root = new TreeNode(1);
root.left = new TreeNode(2);
root.right = new TreeNode(3);
root.left.left = new TreeNode(4);
root.left.right = new TreeNode(5);
root.right.left = new TreeNode(3);
const ret = bst.hasNoDuplicates(root);
debugger;
