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
 * @return {number[][]}
 */
var levelOrder = function(root) {
    const res = []
    if (!root) return res
    const q = [root]
    while(q.length) {
        let size = q.length
        const vals = []
        while(size--) {
            const node = q.shift()
            vals.push(node.val)
            if (node.left) {
                q.push(node.left)
            }
            if (node.right) {
                q.push(node.right)
            }
        }
        res.push(vals)
    }
    return res
};

var levelOrder2 = function(root) {
    // 使用 dfs，每次先遍历左子树
    const m = {} // level -> nodelist
    function dfs(node, level) {
        if (!node) return
        m[level] = m[level] || []
        dfs(node.left, level+1)
        m[level].push(node.val)
        dfs(node.right, level+1)
    }
    dfs(root, 0)
    const res = []
    for (const level of Object.keys(m).sort((a,b) => a-b)) {
        res.push(m[level])
    }
    return res
}
