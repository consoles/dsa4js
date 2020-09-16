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
var invertTree = function (root) {

  // 交换放在递归之后
  // if (!root) return null
  //
  // invertTree(root.left)
  // invertTree(root.right)
  //
  // // 执行交换
  // const tmp = root.left
  // root.left = root.right
  // root.right = tmp
  // return root

  // 交换放在递归之前
  // if (!root) return null
  // const tmp = root.left
  // root.left = root.right
  // root.right = tmp
  //
  // invertTree(root.left)
  // invertTree(root.right)
  //
  // return root

  // bfs写法
  if (!root) return null
  const q = [root]
  while (q.length) {
    const node = q.shift()

    // 交换
    const tmp = node.left
    node.left = node.right
    node.right = tmp

    if (node.left) {
      q.push(node.left)
    }
    if (node.right) {
      q.push(node.right)
    }
  }

  return root
};

const root = {
  val: 4,
  left: {
    val: 2,
    left: {
      val: 1
    },
    right: {
      val: 3
    }
  },
  right: {
    val: 7,
    left: {
      val: 6
    },
    right: {
      val: 9
    }
  }
}

invertTree(root)
debugger
