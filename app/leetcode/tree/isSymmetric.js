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
var isSymmetric = function (root) {

  // 偷个懒，树格式化为JSON
  if (!root) return true;
  // 想偷懒发现有bug，这样判断的是投影而不是镜像
  // return JSON.stringify(root.left) === JSON.stringify(root.right);

  // 两个树互为镜像必须满足：根节点具有相同的值；每颗树的右子树都和另一个树的左子树 镜像对称
  // function isMirror(t1, t2) {
  //   if (!t1 && !t2) return true;
  //   if (!t1 || !t2) return false;
  //   return t1.val === t2.val && isMirror(t1.left, t2.right) && isMirror(t1.right, t2.left);
  // }
  //
  // return isMirror(root.left, root.right);

  // 利用队列进行迭代，类似BFS
  const q = [root.left, root.right];
  while (q.length) {
    const t1 = q.shift();
    const t2 = q.shift();
    if (!t1 && !t2) continue;
    if (!t1 || !t2) return false;
    if (t1.val !== t2.val) return false;
    // 按照左右子节点相反的顺序插入到队列中
    q.push(t1.left);
    q.push(t2.right);
    q.push(t1.right);
    q.push(t2.left);
  }
  return true;
};
