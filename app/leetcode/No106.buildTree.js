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
  // 新版本 leetcode 添加了大数据类型的测试用例后已经无法工作了
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

var buildTree2 = function (inorder, postorder) {
  // inorder: [l, root, r]
  // postorder: [l, r, root]
  // 第一种方法思路没有错，可以有下面的优化点
  // 1. 每次使用 indexOf 在中序数组中查找根节点，时间复杂度为 O(n)
  // 2. 每次使用 slice 在中序数组中查找根节点，时间复杂度为 O(n)
  // 3. 重新构造 postOrderLeft 和 postOrderRight，时间复杂度为 O(n)

  const inorderNodePosition = new Map()
  for (let i = 0; i < inorder.length; i++) {
    inorderNodePosition.set(inorder[i], i)
  }

  
  function _buildTree(inOrderStart, inOrderEnd, postOrderStart, postOrderEnd) {
    if (inOrderStart > inOrderEnd) return null
    if (postOrderStart > postOrderEnd) return null

    const rootVal = postorder[postOrderEnd]
    const rootInOrderIndex = inorderNodePosition.get(rootVal)
    const node = {val: rootVal}
    // 左子树：中序数组 [inOrderStart, rootInOrderIndex - 1]
    // 右子树：中序数组 [rootInOrderIndex + 1, inOrderEnd]
    // 上面的 2 个是非常容易理解的
    // 我们可以推导出左右子树的元素个数
    const leftTreeNodeCount = (rootInOrderIndex - 1) - inOrderStart + 1
    const rightTreeNodeCount = inOrderEnd - (rootInOrderIndex + 1) + 1
    // 左子树：后序数组 [postOrderStart, postOrderStart + leftTreeNodeCount - 1]
    // 右子树：后序数组 [postOrderStart + leftTreeNodeCount, postOrderEnd - 1]
    node.left = _buildTree(inOrderStart, rootInOrderIndex - 1, postOrderStart, postOrderStart + leftTreeNodeCount - 1)
    node.right = _buildTree(rootInOrderIndex + 1, inOrderEnd, postOrderStart + leftTreeNodeCount, postOrderEnd - 1)
    return node
  }
  const root = _buildTree(0, inorder.length - 1, 0, postorder.length - 1)
  return root
}

inorder = [9, 3, 15, 20, 7]
postorder = [9, 15, 7, 20, 3]

const root = buildTree2(inorder, postorder)
debugger
