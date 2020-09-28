/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */

/**
 * @param {TreeNode} root
 * @param {TreeNode} p
 * @param {TreeNode} q
 * @return {TreeNode}
 */
var lowestCommonAncestor = function (root, p, q) {
  // 如果两个节点值都小于根节点，说明他们都在根节点的左子树上，我们往左子树上找
  // 如果两个节点值都大于根节点，说明他们都在根节点的右子树上，我们往右子树上找
  // 如果一个节点值大于根节点，一个节点值小于根节点，说明他们他们一个在根节点的左子树上一个在根节点的右子树上，那么根节点就是他们的最近公共祖先节点。
  //如果根节点和p,q的差相乘是正数，说明这两个差值要么都是正数要么都是负数，也就是说
  //他们肯定都位于根节点的同一侧，就继续往下找
  while ((root.val - p.val) * (root.val - q.val) > 0)
    root = p.val < root.val ? root.left : root.right;
  //如果相乘的结果是负数，说明p和q位于根节点的两侧，如果等于0，说明至少有一个就是根节点
  return root;
};

let root = {
  val: 6,
  left: {
    val: 2,
    left: {
      val: 0
    },
    right: {
      val: 4,
      left: {
        val: 3
      },
      right: {
        val: 5
      }
    }
  },
  right: {
    val: 8,
    left: {
      val: 7
    },
    right: {
      val: 9
    }
  }
}

let ret = lowestCommonAncestor(root, root.left, root.right)
debugger
