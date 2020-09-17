/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @return {number[]}
 */
var inorderTraversal = function(root) {
    // dfs
    // const path = []
    // function inOrder(root){
    //     if (!root) return
    //     inOrder(root.left)
    //     path.push(root.val)
    //     inOrder(root.right)
    // }
    // inOrder(root)
    // return path

    // 进阶：迭代法 ,参见chapter3/BST.js#inOrder2
    let cur = root
    const stack = []
    const path = []
    while(cur || stack.length) {
        if (cur) {
            stack.push(cur)
            cur = cur.left
        } else {
            cur = stack.pop()
            path.push(cur.val)
            cur = cur.right
        }
    }
    return path
};

const root = {
    val:1,
    left:null,
    right:{
        val:2,
        left:{
            val:3
        }
    }
}

const res = inorderTraversal(root)
console.log(res);