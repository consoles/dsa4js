/**
 * @param {number[]} nums
 * @return {number[][]}
 */
var permuteUnique = function (nums) {
  const res = []
  const n = nums.length

  nums.sort((a, b) => a - b)

  const used = new Array(n).fill(false)

  function dfs(start, path) {
    if (start === n) {
      res.push(path.slice())
      return
    }
    for (let i = 0; i < n; i++) {
      // 上一次搜索的数刚刚被撤销，肯定会发生重复（避免重复）
      if (i > 0 && nums[i] === nums[i - 1] && !used[i - 1]) continue
      if (!used[i]) {
        used[i] = true
        path.push(nums[i])
        dfs(start + 1, path)
        path.pop()
        used[i] = false
      }
    }
  }

  dfs(0, [])
  return res
};

// const res = permuteUnique([1, 1, 2])
// console.log(res)

// 下面这种方法就只能在全排列的结果中进行去重了
function permute(nums) {
  const n = nums.length

  function swap(i, j) {
    if (i === j) return
    [nums[i], nums[j]] = [nums[j], nums[i]]
  }

  const paths = []

  function dfs(index) {
    if (index === n) {
      paths.push(nums.slice())
      return
    }
    for (let i = index; i < n; i++) {
      swap(index, i)
      dfs(index + 1)
      swap(index, i)
    }
  }

  dfs(0)
  return paths
}

console.log(permute([1, 2, 3]))
