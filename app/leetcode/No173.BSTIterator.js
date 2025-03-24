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
 */
var BSTIterator = function(root) {
    // 方法1：中序遍历二叉树，可以得到顺序数组
    // 这种方法实现简单，初始化方法为 O(N)，next 和 hasNext 是 O(1)
    const vals = []
    function dfs(node) {
        if (node) {
            dfs(node.left)
            vals.push(node.val)
            dfs(node.right)
        }
    }
    dfs(root)
    this.curIndex = 0
    this.vals = vals
};

/**
 * @return {number}
 */
BSTIterator.prototype.next = function() {
    return this.vals[this.curIndex++]
};

/**
 * @return {boolean}
 */
BSTIterator.prototype.hasNext = function() {
    return this.curIndex < this.vals.length
};

/** 
 * Your BSTIterator object will be instantiated and called as such:
 * var obj = new BSTIterator(root)
 * var param_1 = obj.next()
 * var param_2 = obj.hasNext()
 */

var BSTIterator2 = function(root) {
    // 方法2：使用栈模拟(维护一个单调递减栈)
    const stack = []
    // 不断放入左子树，这样栈顶元素就是当前最小值
    while (root) {
        stack.push(root)
        root = root.left
    }
    this.stack = stack
}

BSTIterator2.prototype.next = function() {
    const node = this.stack.pop()
    const val = node.val
    let cur = node.right
    while (cur) {
        this.stack.push(cur)
        cur = cur.left
    }
    return val
}

BSTIterator2.prototype.hasNext = function() {
    return this.stack.length > 0
}
