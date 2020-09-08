/**
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
