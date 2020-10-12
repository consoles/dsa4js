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
var maxDepth = function (root) {
    // if (!root) return 0
    // return 1 + Math.max(maxDepth(root.left), maxDepth(root.right))

    // 思路2：层序遍历
    let depth = 0
    if (!root) return depth
    const q = [root]
    while (q.length) {
        let size = q.length
        while (size--) {
            const node = q.shift()
            if (node.left) {
                q.push(node.left)
            }
            if (node.right) {
                q.push(node.right)
            }
        }
        depth++
    }
    return depth
};

const root = {
    val: 3,
    left: {
        val: 9
    },
    right: {
        val: 20,
        left: {
            val: 15
        },
        right: {
            val: 7
        }
    }
}

const depth = maxDepth(root)
debugger