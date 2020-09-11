/**
 * @param {number} k
 * @param {number} n
 * @return {number[][]}
 */
var combinationSum3 = function (k, n) {
  const paths = []

  function dfs(start, k, sum, path) {
    if (k === 0 && sum === 0) {
      paths.push(path.slice())
      return
    }
    if (k <= 0 || sum <= 0) return
    for (let i = start; i <= 9; i++) {
      path.push(i)
      dfs(i + 1, k - 1, sum - i, path)
      path.pop()
    }
  }

  dfs(1, k, n, [])
  return paths
};

k = 3, n = 9;
console.log(combinationSum3(k, n))
