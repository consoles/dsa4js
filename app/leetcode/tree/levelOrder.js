/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @return {number[][]}
 */
var levelOrder = function (root) {

  const res = [];

  if (!root) return res;
  //
  // const q = [root];
  //
  // while (q.length) {
  //   let size = q.length;
  //   const items = [];
  //   while (size--) {
  //     const node = q.shift();
  //     items.push(node.val);
  //     if (node.left) q.push(node.left);
  //     if (node.right) q.push(node.right);
  //   }
  //   res.push(items);
  // }
  // return res;

  // dfs
  function dfs(root, level) {
    if (!root) return;
    if (!res[level]) res[level] = [];
    res[level].push(root.val);
    dfs(root.left, level + 1);
    dfs(root.right, level + 1);
  }

  dfs(root, 0);

  return res;
};

r = {
  val: 3,
  left: {
    val: 9
  },
  right: {
    val: 20,
    left: {
      val: 15
    },
    right: {
      val: 7
    }
  }
};

res = levelOrder(r);
debugger;
