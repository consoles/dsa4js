/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @return {number}
 */
var minDepth = function (root) {
  // 这个有问题[1,2]会返回1，题目要求返回2
  // return !root ? 0 : 1 + Math.min(minDepth(root.left), minDepth(root.right));

  // BFS
  // if (!root) return 0;
  // const q = [root];
  // let level = 0;
  // while (q.length) {
  //   level++;
  //   let size = q.length;
  //   while (size--) {
  //     const node = q.shift();
  //     if (!node.left && !node.right) {
  //       return level;
  //     }
  //     if (node.left) {
  //       q.push(node.left);
  //     }
  //     if (node.right) {
  //       q.push(node.right);
  //     }
  //   }
  // }
  //
  // return level;

  // dfs
  // https://leetcode-cn.com/problems/minimum-depth-of-binary-tree/solution/li-jie-zhe-dao-ti-de-jie-shu-tiao-jian-by-user7208/
  if (!root) return 0;
  if (!root.left && !root.right) return 1;
  // 如果左孩子或者右孩子有一个为空，需要返回大的那个孩子的深度（因为这里 的叶子节点是没有子节点的）
  const d1 = minDepth(root.left);
  const d2  = minDepth(root.right);
  // 其中 一个孩子为空说明d1和d2其中有一个是0，所以可以直接用d1+d2+1
  if (!root.left || !root.right) return d1 + d2 + 1;
  return Math.min(d1,d2) + 1;
};
