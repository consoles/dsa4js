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
var getMinimumDifference = function (root) {
    // 朴素解法，中序遍历得到一个递增的数组，对递增数组中求两个元素之差的绝对值的最小值，答案为相邻两个元素之差的最小值
    let sortedArr = []
    function inOrder(root) {
        if (!root) return
        inOrder(root.left)
        sortedArr.push(root.val)
        inOrder(root.right)
    }

    inOrder(root)
    let min = Number.MAX_SAFE_INTEGER
    let prev = -1
    for (const num of sortedArr) {
        if (prev === -1) {
            prev = num
        } else {
            min = Math.min(min, num - prev)
            prev = num
        }
    }

    return min

    // 不需要显式创建数组的解法
    // let min = Number.MAX_VALUE
    // let prev = -1 // 保存前一个root.val
    // function dfs(root) {
    //     if (!root) return
    //     // 中序遍历

    //     dfs(root.left)
    //     // 处理逻辑
    //     if (prev === -1) {
    //         prev = root.val
    //     } else {
    //         // 因为中序遍历是一个从小到大的过程，所以root.val 一定是大于prev
    //         min = Math.min(min, root.val - prev)
    //         prev = root.val
    //     }
    //     dfs(root.right)
    // }
    // dfs(root)
    // return min
};

let root = {
    val: 1,
    right: {
        val: 3,
        left: {
            val: 2
        }
    }
}

let res = getMinimumDifference(root)
console.log(res);