/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {number[]} inorder
 * @param {number[]} postorder
 * @return {TreeNode}
 */
var buildTree = function (inorder, postorder) {
  if (inorder.length <= 0) return null;
  // 先创建根节点
  const val = postorder[postorder.length - 1]
  const root = {val}
  const index = inorder.indexOf(val)
  const inOrderLeft = inorder.slice(0, index)
  const inOrderRight = inorder.slice(index + 1)
  const postOrderLeft = []
  const postOrderRight = []
  for (const item of postorder) {
    if (inOrderLeft.includes(item)) {
      postOrderLeft.push(item)
    } else if (inOrderRight.includes(item)) {
      postOrderRight.push(item)
    }
  }
  root.left = buildTree(inOrderLeft, postOrderLeft)
  root.right = buildTree(inOrderRight, postOrderRight)
  return root
};

inorder = [9, 3, 15, 20, 7]
postorder = [9, 15, 7, 20, 3]

const root = buildTree(inorder, postorder)
debugger
