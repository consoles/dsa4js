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
var averageOfLevels = function (root) {
    // bfs
    const res = []
    if (!root) return res
    const q = [root]
    while (q.length) {
        const size = q.length
        let s = 0
        for (let i = 0; i < size; i++) {
            const node = q.shift()
            s += node.val
            if (node.left) {
                q.push(node.left)
            }
            if (node.right) {
                q.push(node.right)
            }
        }
        res.push(s / size)
    }
    return res
};

var averageOfLevels2 = function (root) {
    // dfs 保存每一层的节点个数和节点和
    const m = {} // level -> {count: 0, sum: 0}
    function dfs(node, level) {
        if (!node) return 
        m[level] = m[level] || {count: 0, sum: 0}
        m[level].count++
        m[level].sum += node.val
        level++
        dfs(node.left, level)
        dfs(node.right, level)
    }
    dfs(root, 0)
    const res = []
    for (const level of Object.keys(m).sort((a,b) => a-b)) {
        res.push(m[level].sum / m[level].count)
    }
    return res
}
