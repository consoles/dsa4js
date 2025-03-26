/**
 * @param {number} n
 * @param {number} k
 * @return {number[][]}
 */
var combine = function (n, k) {
  // const paths = []
  //
  // function dfs(start, path) {
  //   if (path.length === k) {
  //     paths.push(path.slice())
  //     return
  //   }
  //   // 遍历所有可能的搜索起点
  //   // for (let i = start; i <= n; i++) {
  //   //   path.push(i);
  //   //   // 下一轮搜索，设置搜索起点要加1，因为组合数不能出现重复元素
  //   //   dfs(i + 1, path)
  //   //   // 重点理解：深度优先遍历有回头的过程，因此递归之前做了什么，递归之后需要做相同操作的逆向操作
  //   //   path.pop()
  //   // }
  //
  //   // 剪枝，搜索起点上界 + 接下来要选择的元素个数 - 1 = n
  // 很朴素的一个情况: [1,2,3,4,5] 中选 3 个数字，当起点位于 4 的时候已经不可能选出来了
  //   for (let i = start; i <= n - (k - path.length) + 1; i++) {
  //     path.push(i)
  //     dfs(i + 1, path)
  //     path.pop()
  //   }
  // }
  //
  // dfs(1, [])
  // return paths

  // 思路3：每个数字可以选和不选，构建一颗二叉树
  // const paths = []
  //
  // /**
  //  *
  //  * @param start
  //  * @param k 剩下应该考虑的数
  //  * @param path
  //  */
  // function dfs(start, k, path) {
  //   if (k === 0) {
  //     paths.push(path.slice())
  //     return
  //   }
  //   if (start > n - k + 1) {
  //     return
  //   }
  //   // if (start === n + 1) {
  //   //   return
  //   // }
  //   // 不选择当前的数
  //   dfs(start + 1, k, path)
  //
  //   // 选择当前数
  //   path.push(start)
  //   dfs(start + 1, k - 1, path)
  //   path.pop()
  // }
  //
  // dfs(1, k, [])
  // return paths

  // 思路4：排列组合公式 Cnk = Cn-1k-1 + Cn-1k
  const paths = []

  function helper(n, k, path) {
    if (k === 0) {
      paths.push(path.slice())
      return
    }
    if (n < k) {
      return
    }
    // 选n
    path.push(n);
    helper(n - 1, k - 1, path)
    path.pop()

    // 不选n
    helper(n - 1, k, path)
  }

  helper(n, k, [])
  return paths
};

const n = 4, k = 2

console.log(combine(n, k))
