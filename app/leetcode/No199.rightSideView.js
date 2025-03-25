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
var rightSideView = function (root) {

  // const res = [];
  // // 1。层序遍历中每一层的最后一个节点
  // if (!root) return res;
  // const q = [root];
  //
  // while (q.length) {
  //   let size = q.length;
  //   let node = null;
  //   while (size--) {
  //     node = q.shift();
  //     if (node.left) {
  //       q.push(node.left);
  //     }
  //     if (node.right) {
  //       q.push(node.right);
  //     }
  //   }
  //   if (node) res.push(node.val);
  // }
  // return res;

  const res = [];

  // 2.dfs, 先递归右子树，保证首次遇到的一定是右边的节点
  function dfs(root, level) {
    if (!root) return;
    // 本层第一个被访问的节点（最右边的节点）
    if (res.length === level) {
      res.push(root.val);
    }
    level++;
    dfs(root.right, level);
    dfs(root.left, level);
  }

  dfs(root, 0);
  return res;
};

// root = {
//   val: 1,
//   left: {
//     val: 2,
//     right: {
//       val: 5
//     }
//   },
//   right: {
//     val: 3,
//     right: {
//       val: 4
//     }
//   }
// };

root = {
  val: 1,
  left: {
    val: 2,
    left: {
      val: 4,
      right: {
        val: 7,
        right: {
          val: 9,
          right: {
            val: 10
          }
        }
      }
    },
    right: {
      val: 5
    }
  },
  right: {
    val: 3,
    right: {
      val: 6,
      left: {
        val: 8
      }
    }
  }
};

rightSideView(root);
