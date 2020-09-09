/**
 * Definition for a binary tree node.
 * function TreeNode(val, left, right) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.left = (left===undefined ? null : left)
 *     this.right = (right===undefined ? null : right)
 * }
 */
/**
 * @param {TreeNode} root
 * @return {void} Do not return anything, modify root in-place instead.
 */
var flatten = function (root) {
  // 1.先序遍历求得列表，然后根据列表构建链表
  const list = []

  function preOrder(root) {
    if (!root) {
      return
    }
    list.push(root)
    preOrder(root.left)
    preOrder(root.right)
  }

  preOrder(root)

  for (let i = 1; i < list.length; i++) {
    const prev = list[i - 1],
      cur = list[i]
    prev.right = cur
    prev.left = null
  }
};

const root = {
  val: 1,
  left: {
    val: 2,
    left: {
      val: 3
    },
    right: {
      val: 4
    }
  },
  right: {
    val: 5,
    right: {
      val: 6
    }
  }
}

flatten(root)
console.log(root)
