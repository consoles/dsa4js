/**
 * @param {TreeNode} s 题目隐含s父树，t是子树
 * @param {TreeNode} t
 * @return {boolean}
 */
var isSubtree = function (s, t) {

  // 方法1：DFS

  function isSameTree(s, t) {
    if (!s && !t) return true;
    if ((s && !t) || (!s && t) || (s.val !== t.val)) return false;
    return isSameTree(s.left, t.left) && isSameTree(s.right, t.right);
  }

  if (!s) return false;
  return isSameTree(s, t) || isSubtree(s.left, t) || isSubtree(s.right, t);
};

const t1 = {
  val: 3,
  left: {
    val: 4,
    left: {
      val: 1
    },
    right: {
      val: 2,
      left: {
        val: 0
      }
    }
  },
  right: {
    val: 5
  }
};

const t2 = {
  val: 4,
  left: {
    val: 1
  },
  right: {
    val: 2
  }
};

ret = isSubtree(t1, t2);
console.log(ret);
