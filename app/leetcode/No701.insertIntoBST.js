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
 * @param {number} val
 * @return {TreeNode}
 */
var insertIntoBST = function(root, val) {
    if (!root) return {val,left:null,right:null};
    const rootVal = root.val;
    if (val < rootVal) {
        root.left = insertIntoBST(root.left,val);
    } else {
        root.right = insertIntoBST(root.right,val);
    }
    return root;
};

const root = {
    val:4,
    left:{
        val:2,
        left:{
            val:1
        },
        right:{
            val:3
        }
    },
    right:{
        val:7
    }
}

const res = insertIntoBST(root,5);
debugger;