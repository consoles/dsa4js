/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @param {number} sum
 * @return {number[][]}
 */
var pathSum = function (root, sum) {
  const paths = []

  function dfs(root, path, partSum) {
    if (!root) return

    partSum += root.val
    path.push(root.val)

    if (!root.left && !root.right) {
      if (partSum === sum) {
        paths.push(path.slice())
      }
      // 千万注意：这里不能加return，注意下面的状态重置
      // 如果确实需要return需要将path重置后return
    }

    dfs(root.left, path, partSum)
    dfs(root.right, path, partSum)

    path.pop()
  }

  dfs(root, [], 0)
  return paths
};

const root = {
  val: 5,
  left: {
    val: 4,
    left: {
      val: 11,
      left: {
        val: 7
      },
      right: {
        val: 2
      }
    }
  },
  right: {
    val: 8,
    left: {
      val: 13
    },
    right: {
      val: 4,
      left: {
        val: 5
      },
      right: {
        val: 1
      }
    }
  }
}

const res = pathSum(root, 22)
console.log(res)
