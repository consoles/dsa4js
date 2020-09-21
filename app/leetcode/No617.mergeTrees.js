function mergeTrees(root1, root2) {
  if (!root1) return root2
  if (!root2) return root1
  let val = 0
  if (root1) {
    val += root1.val
  }
  if (root2) {
    val += root2.val
  }
  const node = {val}
  node.left = mergeTrees(root1.left, root2.left)
  node.right = mergeTrees(root1.right, root2.right)
  return node
}

function mergeTrees(root1, root2) {
  if (!root1) return root2
  if (!root2) return root1
  const q = [root1, root2]
  while (q.length) {
    const node1 = q.shift()
    const node2 = q.shift()
    // 此时2个节点一定不为空，相加
    node1.val += node2.val
    if (node1.left && node2.left) {
      q.push(node1.left)
      q.push(node2.left)
    } else if (!node1.left) {
      node1.left = node2.left
    }
    if (node1.right && node2.right) {
      q.push(node1.right)
      q.push(node2.right)
    } else if (!node1.right) {
      node1.right = node2.right
    }
  }
  return root1
}

const root1 = {
  val: 1,
  left: {
    val: 2,
    left: {
      val: 4
    },
    right: {
      val: 5
    }
  },
  right: {
    val: 3
  }
}

const root2 = {
  val: 6,
  left: {
    val: 7,
    right: {
      val: 9
    }
  },
  right: {
    val: 8,
    right: {
      val: 10
    }
  }
}

const r = mergeTrees(root1, root2)
debugger
