var maxDepth = function (root) {
  // 递归
  // if (!root) return 0;
  // return Math.max(maxDepth(root.left), maxDepth(root.right)) + 1;

  // BFS求深度
  let depth = 0;
  const q = [];
  if (root) q.push(root);
  while (q.length) {
    let size = q.length; // 这一层有多少个节点
    while (size--) {
      const node = q.shift();
      if (node.left) {
        q.push(node.left);
      }
      if (node.right) {
        q.push(node.right);
      }
    }
    depth++;
  }
  return depth;
};

root = {
  val: 3,
  left: {
    val: 9
  },
  right: {
    val: 20,
    left: {
      val: 15
    },
    right: {
      val: 7
    }
  }
};

d = maxDepth(root);
console.log(d);
