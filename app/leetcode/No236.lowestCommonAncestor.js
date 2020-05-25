/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * 后序遍历DFS
 * 存在3种情况：
 * p,q分别位于左右两颗子树
 * p，q都位于左子树
 * p,q都位于右子树
 *
 * 第一种情况最简单的，直接返回根节点就完事了
 * 后两种情况相当于把原来问题缩小了
 * @param {TreeNode} root
 * @param {TreeNode} p
 * @param {TreeNode} q
 * @return {TreeNode}
 */
var lowestCommonAncestor = function (root, p, q) {
  if (!root || root === p || root === q) return root;
  // 左右子树已经计算出了 结果
  // 在左子树中寻找最近公共祖先
  const left = lowestCommonAncestor(root.left, p, q);
  // 在右子树中寻找最近公共祖先
  const right = lowestCommonAncestor(root.right, p, q);
  if (!left) return right;
  if (!right) return left;
  // 一边一个
  return root;
};

const r1 = {
  val: 3,
  left: {
    val: 5,
    left: {
      val: 6
    },
    right: {
      val: 2,
      left: {
        val: 7
      },
      right: {
        val: 4
      }
    }
  },
  right: {
    val: 1,
    left: {
      val: 0
    },
    right: {
      val: 8
    }
  }
};

const p = r1.left;
const q = r1.left.right.right;

console.log(lowestCommonAncestor(r1,p,q));
