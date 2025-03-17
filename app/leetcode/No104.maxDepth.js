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
 * @return {number}
 */
var maxDepth = function(root) {
    // if (!root) return 0
    // const maxL = maxDepth(root.left)
    // const maxR = maxDepth(root.right)
    // return 1 + Math.max(maxL, maxR)

    let level = 0
    if (!root) return level
    const q = [root]
    while (q.length) {
        let size = q.length
        while(size--) {
            const node = q.shift()
            if (node.left) q.push(node.left)
            if (node.right) q.push(node.right)
        }
        level++
    }
    return level
};
