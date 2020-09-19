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
var sumOfLeftLeaves = function(root) {
    // 这个题目要注意：只有一个节点的时候虽然也是叶子节点，但是不能算是左叶子
    let sum = 0
    if (!root) return sum
    // 结合父亲节点才可以判断是否是左子节点，给定一个节点不能孤立判断是否是左子节点
    if(root.left && !root.left.left && !root.left.right) {
        sum += root.left.val
    }
    return sum + sumOfLeftLeaves(root.left) + sumOfLeftLeaves(root.right)
};

var sumOfLeftLeaves = function(root) {
    // 上面那个是二叉树的前序遍历，非递归实现如下：
    let sum = 0
    if (!root) return sum
    const stack = [root]
    while(stack.length) {
        const node = stack.pop()
        if (node.left && !node.left.left && !node.left.right) {
            sum += node.left.val
        }
        if (node.left) {
            stack.push(node.left)
        }
        if (node.right) {
            stack.push(node.right)
        }
    }
    return sum
};

var sumOfLeftLeaves = function(root) {
   /**
    * 方法1另一种思路：既然我们要根据父节点才能确定是否是左孩子，那么我们就可以在遍历左右子树的是否传入
    * @param {*} root 
    * @param {*} isLeft 
    */ 
   function dfs(root,isLeft) {
       if (!root) return 0
       if (!root.left && !root.right) {
           return isLeft ? root.val : 0
       }
       return dfs(root.left,true) + dfs(root.right,false)
   }
   return dfs(root,false)
};

var sumOfLeftLeaves = function(root) {
    // bfs
    let sum = 0
    if (!root) return sum
    const q = [root]
    while(q.length) {
        const node = q.shift()
        if (node.left && !node.left.left && !node.left.right) {
            sum += node.left.val
        }
        if (node.left) {
            q.push(node.left)
        }
        if (node.right) {
            q.push(node.right)
        }
    }
    return sum
 };

let root = {
    val:3,
    left:{
        val:9
    },
    right:{
        val:30,
        left:{
            val:15
        },
        right:{
            val:7
        }
    }
}

// root = {val:1}
console.log(sumOfLeftLeaves(root))