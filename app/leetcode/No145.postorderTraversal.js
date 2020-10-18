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
var postorderTraversal = function(root) {
    const res = []
    // function dfs(root) {
    //     if (!root) return 
    //     dfs(root.left)
    //     dfs(root.right)
    //     res.push(root.val)
    // }
    // dfs(root)

    // https://leetcode-cn.com/problems/binary-tree-postorder-traversal/solution/die-dai-jie-fa-shi-jian-fu-za-du-onkong-jian-fu-za/
    // 迭代
    const stack = []
    let cur = root
    while(cur || stack.length) {
        if (cur) {
            // 每遇到一个节点，就把它加入结果集，并把该节点保存到中间结果中
            res.unshift(cur.val)
            stack.push(cur)
            // 先遍历右子树，一直走到空
            cur = cur.right
        } else {
            // 右子树走到空，就从获取已经遍历过左子树的中间结果，将它出栈，并遍历它的左子树
            const node = stack.pop()
            cur = node.left
        }
    }
    return res
};

let root = {
    val:1,
    right:{
        val:2,
        left:{
            val:3
        }
    }
}

const res = postorderTraversal(root)
console.log(res);