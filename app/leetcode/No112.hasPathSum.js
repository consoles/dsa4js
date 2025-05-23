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
 * @return {boolean}
 */
var hasPathSum = function (root, sum) {

  // dfs
  // function dfs(root, curSum) {
  //   if (!root) return false;
  //   curSum += root.val;
  //   if (!root.left && !root.right) return curSum === sum;
  //   let flag = dfs(root.left, curSum);
  //   if (flag) {
  //     return true;
  //   }
  //   flag = dfs(root.right, curSum);
  //   return flag;
  // }
  //
  // if (!root) return false;
  //
  // return dfs(root, 0);

  // 不得不佩服官方的题解简单多了
  if (!root) return false;
  sum -= root.val;
  return !root.left && !root.right ? sum === 0 : hasPathSum(root.left, sum) || hasPathSum(root.right, sum);
};

var hasPathSum2 = function (root, sum) {
  if (!root) return false
  // bfs
  // 遍历节点的同时，计算当前路径和
  const qNode = [root]
  const qSum = [root.val]
  while (qNode.length) {
    const node = qNode.shift()
    const curSum = qSum.shift()
    if (!node.left && !node.right) {
      if (curSum === sum) return true
    }
    if (node.left) {
      qNode.push(node.left)
      qSum.push(curSum + node.left.val)
    }
    if (node.right) {
      qNode.push(node.right)
      qSum.push(curSum + node.right.val)
    }
  }
  return false
}

r = {
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
      right: {
        val: 1
      }
    }
  }
};

a = hasPathSum(r, 22);
debugger;
