/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @return {boolean}
 */
var isValidBST = function(root) {
    function isValid(node,min,max) {
        if (!node) return true
        if (node.val <= min || node.val >= max) return false
        return isValid(node.left,min,node.val) && isValid(node.right,node.val,max)
    }
    return isValid(root,Number.MIN_SAFE_INTEGER,Number.MAX_SAFE_INTEGER)
};

let root = {
    val:2,
    left:{
        val:1
    },
    right:{
        val:3
    }
}

root = {
    val:5,
    left:{
        val:1
    },
    right:{
        val:4,
        left:{
            val:3
        },
        right:{
            val:6
        }
    }
}

console.log(isValidBST(root));