/**
 * // Definition for a Node.
 * function Node(val, left, right, next) {
 *    this.val = val === undefined ? null : val;
 *    this.left = left === undefined ? null : left;
 *    this.right = right === undefined ? null : right;
 *    this.next = next === undefined ? null : next;
 * };
 */

/**
 * @param {Node} root
 * @return {Node}
 */
var connect = function(root) {
    // 方法1：层序遍历(BFS)，同一层的数据构造链表
    if (!root) return root

    const q = [root]
    while(q.length) {
        let size = q.length
        let prev = null
        while(size--) {
            const node = q.shift()
            // if (!prev) {
            //     prev = node
            // } else {
            //     prev.next = node
            //     prev = node
            // }
            if(prev) {
                prev.next = node
            }
            prev = node
            if (node.left) {
                q.push(node.left)
            }
            if (node.right) {
                q.push(node.right)
            }
        }
    }
    return root 
};

const {buildBinaryTreeFromArray} = require('./utils')
const root = buildBinaryTreeFromArray([1,2,3,4,5,6,7])
connect(root)
debugger

// 层序遍历基于BFS，不同之处在于BFS每次只会取出一个节点来进行拓展，而层序遍历每次将队列中的所有数据取出来拓展，即`while(size--)`。