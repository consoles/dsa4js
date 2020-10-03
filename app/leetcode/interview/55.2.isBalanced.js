/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */

function treeDepth(root) {
    if (!root) return 0
    return Math.max(treeDepth(root.left), treeDepth(root.right)) + 1
}
/**
 * @param {TreeNode} root
 * @return {boolean}
 */
var isBalanced = function (root) {
    if (!root) return true
    const lDepth = treeDepth(root.left)
    const rDepth = treeDepth(root.right)
    if (Math.abs(lDepth - rDepth) > 1) {
        return false
    }
    return isBalanced(root.left) && isBalanced(root.right)
};

const { buildBinaryTreeFromArray } = require('../utils')
let flag = isBalanced(buildBinaryTreeFromArray([3, 9, 20, null, null, 15, 7]))
flag = isBalanced(buildBinaryTreeFromArray([1, 2, 2, 3, 3, null, null, 4, 4]))
debugger