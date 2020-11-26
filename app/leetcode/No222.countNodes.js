/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @return {number}
 */
var countNodes = function (root) {
    // 题目要求了完全二叉树，这个题目应该有更有效率的做法，先这样干了
    // function count(root) {
    //     return root ? 1 + count(root.left) + count(root.right) : 0;
    // }
    // return count(root);

    function countLevel(root) {
        let level = 0;
        while (root) {
            level++;
            root = root.left;
        }
        return level;
    }

    if (!root) return 0;
    const leftLevel = countLevel(root.left);
    const rightLevel = countLevel(root.right);

    // 左右两个子树的高度一样，则左子树一定是满二叉树，进而可以直接算出左子树中的节点个数为 2^left-1，右子树的节点个数为 countNodes(root.right) ，根节点个数为1
    if (leftLevel === rightLevel) {
        return 2 ** leftLevel - 1 + countNodes(root.right) + 1;
    }
    // 最后一层不满，倒数第二层满了，可以直接得到右子树的节点个数
    return countNodes(root.left) + 2 ** rightLevel - 1 + 1;
};

const { buildBinaryTreeFromArray } = require('./utils');
const root = buildBinaryTreeFromArray([1, 2, 3, 4, 5, 6]);
const count = countNodes(root)
debugger