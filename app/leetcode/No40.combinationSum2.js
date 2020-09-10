/**
 * @param {number[]} candidates
 * @param {number} target
 * @return {number[][]}
 */
var combinationSum2 = function (candidates, target) {
  const n = candidates.length
  const paths = []
  candidates.sort((a, b) => a - b)

  function dfs(index, path, target) {
    if (target === 0) {
      paths.push(path.slice())
      return
    }
    for (let i = index; i < n; i++) {
      const num = candidates[i]
      const newTarget = target - num
      if (newTarget < 0) {
        break
      }
      // 去重
      if (i > index && num === candidates[i - 1]) {
        continue
      }
      path.push(num)
      dfs(i + 1, path, newTarget)
      path.pop()
    }
  }

  dfs(0, [], target)
  return paths
};

candidates = [10, 1, 2, 7, 6, 1, 5], target = 8;
candidates = [2, 5, 2, 1, 2], target = 5;

res = combinationSum2(candidates, target);
console.log(res)
