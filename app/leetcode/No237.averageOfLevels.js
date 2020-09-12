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
var averageOfLevels = function(root) {
    if (!root) return []
    // bfs
    const q = [root]

    const res = []

    while(q.length) {
        let size = q.length
        let sum = 0
        for(let i = 0;i < size;i++) {
            const item = q.shift()
            sum += item.val
            if (item.left) {
                q.push(item.left)
            }
            if (item.right) {
                q.push(item.right)
            }
        }
        res.push(sum / size)    
    }
    return res
};

const root = {
    val:3,
    left:{
        val:9
    },
    right:{
        val:20,
        left:{
            val:15
        },
        right:{
            val:7
        }
    }
}

console.log(averageOfLevels(root))