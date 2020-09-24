/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @return {number[]}
 */
var findMode = function (root) {
  const counter = new Map()

  function dfs(root) {
    if (!root) return
    const val = root.val
    if (!counter.has(val)) {
      counter.set(val, 1)
    } else {
      counter.set(val, counter.get(val) + 1)
    }
    dfs(root.left)
    dfs(root.right)
  }

  dfs(root)

  let maxCount = 0
  for (const count of counter.values()) {
    if (count > maxCount) {
      maxCount = count
    }
  }
  const res = []
  for (const [item, count] of counter) {
    if (count === maxCount) {
      res.push(item)
    }
  }
  return res
};

var findMode = function (root) {
  // 使用几个变量来表示，就少了hash表的开销
  let base = -1
  let count = 0
  let maxCount = 0
  let res = []

  function update(x) {
    if (x === base) {
      count++
    } else {
      count = 1
      base = x
    }
    if (count > maxCount) {
      maxCount = count
      res = [base]
    }
    else if (count === maxCount) {
      res.push(base)
    }
  }

  // 利用BST中序遍历得到的是有序数组，相同的数一定集中在同一段
  function dfs(root) {
    if (!root) return
    dfs(root.left)
    update(root.val)
    dfs(root.right)
  }

  dfs(root)

  return res
};

let root = {
  val: 1,
  right: {
    val: 2,
    left: {
      val: 2
    }
  }
}

root = {
  val: 2,
  left: {
    val: 1
  },
  right: {
    val: 2
  }
}

const res = findMode(root)
debugger
