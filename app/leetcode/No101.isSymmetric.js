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
 * @return {boolean}
 */
var isSymmetric = function(root) {
    if (!root) return true
    // function isMirror(l,r) {
    //     if (!l && !r) return true
    //     if (!l || !r) return false
    //     if (l.val !== r.val) return false
    //     // 左子树 = 右子树， 右子树 = 左子树
    //     return isMirror(l.left, r.right) && isMirror(l.right, r.left)
    // }
    // return isMirror(root.left, root.right)

    // 类似 BFS, 左子树 = 右子树 && 右子树 = 左子树
    const q = [root.left, root.right]
    while(q.length) {
        const l = q.shift()
        const r = q.shift()
        if (!l && !r) continue // 需要进行进一步比较
        if (!l || !r) return false
        if (l.val !== r.val) return false
        q.push(l.left, r.right, l.right, r.left)
    }
    return true
};
