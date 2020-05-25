/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @return {boolean}
 */
var isValidBST = function (root) {
  if (!root) return true;

  // function isValid(root, min, max) {
  //   if (!root) return true;
  //   if (root.val <= min || root.val >= max) return false;
  //   return isValid(root.left, min, root.val) && isValid(root.right, root.val, max);
  // }
  //
  // return isValid(root, Number.MIN_SAFE_INTEGER, Number.MAX_SAFE_INTEGER);

  // 合法的BST中序遍历后一定是生序排列
  // function inOrder(root) {
  //   if (!root) return true;
  //   let f = inOrder(root.left);
  //   if (!f) return false;
  //   if (root.val <= last) return false;
  //   last = root.val;
  //   return inOrder(root.right);
  // }
  //
  // let last = Number.MIN_SAFE_INTEGER;
  // return inOrder(root);

  // 中序遍历的非递归实现
  let last = Number.MIN_SAFE_INTEGER;
  const stack = [];
  while (stack.length || root) {
    while (root) {
      stack.push(root);
      root = root.left;
    }
    root = stack.pop();
    if (root.val <= last) {
      return false;
    }
    last = root.val;
    root = root.right;
  }
  return true;
};

r = {
  val: 1,
  right: {
    val: 1
  }
};

r = {
  val: 2,
  left: {
    val: 1
  },
  right: {
    val: 3
  }
};

// r = {
//   val: 5,
//   left: {
//     val: 1
//   },
//   right: {
//     val: 4,
//     left: {
//       val: 3
//     },
//     right: {
//       val: 6
//     }
//   }
// };

ret = isValidBST(r);
debugger;

