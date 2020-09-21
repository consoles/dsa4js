/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @return {TreeNode}
 */
var convertBST = function (root) {
  // 反向中序遍历BST
  let sum = 0

  // 以右->根->左的顺序遍历二叉树，将遍历顺序的前一个结点的累加值记录起来，和当前结点相加，得到当前结点的累加值。
  // 对于每一个节点，先递归它的右子树，里面的节点值都比当前节点大，把它们累加给 sum。
  // 再 “处理” 当前节点，sum 累加上当前节点值，更新当前节点值，
  // 再递归它的左子树，sum 保存了所有 比当前节点大的 节点值的和，加上当前节点值，更新 sum，更新当前节点值。
  // 不管访问到哪个节点，sum始终保存了：比当前节点值大的所有节点值的和。
  function dfs(root) {
    if (root) {
      dfs(root.right)
      sum += root.val
      root.val = sum
      dfs(root.left)
    }
    return root
  }

  // 转换完成后右子树最小，左子树最大
  return dfs(root)
};

/** 2次遍历
 * @param {TreeNode} root
 * @return {TreeNode}
 */
var convertBST = function (root) {
  // 先中序遍历得到节点顺序数组，然后依次修改节点中的value
  const nodes = []

  function inOrder(root) {
    if (!root) return
    inOrder(root.left)
    nodes.push(root)
    inOrder(root.right)
  }

  inOrder(root)

  // 从后向前修改节点的值(开始位置是倒数第2个位置)
  for (let i = nodes.length - 2; i >= 0; i--) {
    nodes[i].val += nodes[i + 1].val
  }
  return root
};

const root = {
  val: 5,
  left: {
    val: 2
  },
  right: {
    val: 13
  }
}

const r = convertBST(root)
debugger
