/**
 * 分析数组:[1,2,3] 的全排列
 * 以 1 开头的全排列 :[1,2,3], [1,3,2] 即: 1 + [2,3] 的全排列（递归结构体提现在这里）
 * 以 2 开头的全排列 :[2,1,3], [2,3,1] 即: 2 + [1,3] 的全排列
 * 以 3 开头的全排列 :[3,1,2], [3,2,1] 即: 3 + [1,2] 的全排列
 * 
 * 按照顺序枚举每一位可能出现的情况，已经选择的数字在当前要选择的数字中不能出现
 * 
 * 使用 DFS 有回头的过程，在回头之后，状态变量需要设置为和先前一样。因此在回到上一层节点的过程中，需要撤销上一次的选择，这个操作是【状态重置】
 * @param {number[]} nums
 * @return {number[][]}
 */
var permute = function (nums) {
  // 基于used数组的写法，数组下标从0开始

  // const paths = [];
  // const n = nums.length;
  // const used = new Array(n).fill(false)
  //
  // function dfs(index, path) {
  //   if (path.length === n) {
  //     paths.push(path.slice())
  //     return
  //   }
  //   for (let i = 0; i < nums.length; i++) {
  //     if (!used[i]) {
  //       path.push(nums[i])
  //       used[i] = true
  //       dfs(index + 1, path)
  //       path.pop()
  //       used[i] = false
  //     }
  //   }
  // }
  //
  // dfs(0, [])
  // return paths;

  // 基于交换的写法
  function swap(i, j) {
    [nums[i], nums[j]] = [nums[j], nums[i]]
  }

  const n = nums.length
  const paths = []

  function dfs(start) {
    if (start === n) {
      paths.push(nums.slice())
      return
    }
    for (let i = start; i < n; i++) {
      swap(i, start)
      dfs(start + 1)
      swap(i, start)
    }
  }

  dfs(0)
  // 基于交换的写法最后输出的结果是[3,2,1],[3,1,2]，而基于used数组的写法是顺序的
  // console.log(paths)
  return paths
};

permute([1, 2, 3]);
