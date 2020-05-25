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
