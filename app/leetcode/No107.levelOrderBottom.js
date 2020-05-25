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
var levelOrderBottom = function (root) {
  // dfs
  // const res = [];
  //
  // function dfs(root, level) {
  //   if (!root) return;
  //   res[level] = res[level] || [];
  //   res[level].push(root.val);
  //   level++;
  //   dfs(root.left, level);
  //   dfs(root.right, level);
  // }
  //
  // dfs(root, 0);
  // return res.reverse();

  // bfs
  const res = [];
  if (!root) return res;
  const q = [root];
  while (q.length) {
    let size = q.length;
    const items = [];
    while (size--) {
      const node = q.shift();
      items.push(node.val);
      if (node.left) {
        q.push(node.left);
      }
      if (node.right) {
        q.push(node.right);
      }
    }
    res.unshift(items);
  }
  return res;

};
