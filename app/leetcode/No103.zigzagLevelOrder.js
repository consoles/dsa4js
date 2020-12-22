/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */

const { buildBinaryTreeFromArray } = require("./utils");

/**
 * @param {TreeNode} root
 * @return {number[][]}
 */
var zigzagLevelOrder = function (root) {
    // 题目写了levelOrder肯定是要用层序遍历，那就干起来吧
    const res = [];
    if (!root) return res;
    const q = [root];
    let reverse = false;
    while (q.length) {
        let size = q.length;
        const items = [];
        while (size--) {
            const item = q.shift();
            if (item.left) {
                q.push(item.left);
            }
            if (item.right) {
                q.push(item.right);
            }
            if (reverse) {
                items.unshift(item.val);
            } else {
                items.push(item.val);
            }
        }
        res.push(items);
        reverse = !reverse;
    }
    return res;
};

const root = buildBinaryTreeFromArray([3, 9, 20, null, null, 15, 7]);
const res = zigzagLevelOrder(root);
debugger;
