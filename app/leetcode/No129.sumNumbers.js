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
 * @return {number}
 */
var sumNumbers = function(root) {
    const nums = []
    function dfs(node, path) {
        if (!node) return
        path += node.val + ""
        // 如果是叶子节点，将路径加入结果数组
        if (!node.left && !node.right) {
            nums.push(parseInt(path))
            return
        }
        dfs(node.left, path)
        dfs(node.right, path)
    }
    dfs(root, "")
    let sum = 0
    console.log(nums)
    for(const num of nums) {
        sum += parseInt(num)
    }
    return sum
};

var sumNumbers2 = function(root) {
    let sum = 0
    function dfs(node, num) {
        if (!node) return
        num = num * 10 + node.val
        // 如果是叶子节点，将路径加入结果数组
        if (!node.left && !node.right) {
            sum += num
            return
        }
        dfs(node.left, num)
        dfs(node.right, num)
    }
    dfs(root, 0)
};

var sumNumbers3 = function(root) {
    // 不使用全局变量
    function dfs(node, sum) {
        if (!node) return 0
        sum = sum * 10 + node.val
        if (!node.left && !node.right) {
            return sum
        }
        return dfs(node.left, sum) + dfs(node.right, sum)
    }

    return dfs(root, 0)
}

var sumNumbers4 = function(root) {
    if (!root) return 0
    // 使用 bfs
    let sum = 0
    const q = [{node:root, sum: root.val}]
    while(q.length) {
        const o = q.shift()
        const node = o.node
        const num = o.sum
        if (!node.left && !node.right) {
            sum += num
        }
        if (node.left) {
            q.push({node: node.left, sum: num * 10 + node.left.val})
        }
        if (node.right) {
            q.push({node: node.right, sum: num * 10 + node.right.val})
        }
    }
    return sum
}

const root1 = {
    val: 1,
    left: {
        val: 2
    },
    right: {
        val: 3
    }
}

const r1 = sumNumbers4(root1)
console.log(r1)
