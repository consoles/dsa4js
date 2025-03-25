/**
 * Definition for a binary tree node.
 * function TreeNode(val, left, right) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.left = (left===undefined ? null : left)
 *     this.right = (right===undefined ? null : right)
 * }
 */
/**
 * @param {TreeNode} root
 * @param {number} k
 * @return {number}
 */
var kthSmallest = function(root, k) {
    // 1. 中序遍历 => 有序数组 => 取数组索引
    const sortedArr = []
    function inOrder(node) {
        if (node) {
            inOrder(node.left)
            sortedArr.push(node.val)
            inOrder(node.right)
        }
    }
    inOrder(root)
    return sortedArr[k-1]
};

var kthSmallest2 = function(root, k) {
    // 中序遍历的同时记录遍历节点的个数
    let nodeCount = 0
    let res = null
    function inOrder(node) {
        if (node) {
            inOrder(node.left)
            nodeCount++
            if (nodeCount === k) {
                res = node.val
                return
            }
            inOrder(node.right)
        }
    }
    inOrder(root)
    return res
}
