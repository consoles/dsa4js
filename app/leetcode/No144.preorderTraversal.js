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
 * @return {number[]}
 */
var preorderTraversal = function (root) {
    // 递归
    // const res = []
    // function preOrder(root) {
    //     if (!root) return
    //     res.push(root.val)
    //     preOrder(root.left)
    //     preOrder(root.right)
    // }
    // preOrder(root)
    // return res

    // 迭代（模拟系统栈）
    const res = []
    if (!root) return res
    const stack = [root]
    while (stack.length > 0) {
        const node = stack.pop()
        if (!node) {
            continue
        }
        res.push(node.val)
        if (node.right) {
            stack.push(node.right)
        }
        if (node.left) {
            stack.push(node.left)
        }
    }
    return res
};