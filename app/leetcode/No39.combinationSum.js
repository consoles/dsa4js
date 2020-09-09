/**
 * @param {number[]} candidates
 * @param {number} target
 * @return {number[][]}
 */
var combinationSum = function (candidates, target) {

  // 先排序，然后再加入路径的时候进行判断，如果当前值小于路径中的最后一个元素就不将其加入路径

  // const paths = []
  // const n = candidates.length
  //
  // candidates.sort((a, b) => a - b)
  //
  // function dfs(path, partSum) {
  //   if (partSum >= target) {
  //     if (partSum === target) {
  //       paths.push(path.slice())
  //     }
  //     return
  //   }
  //   for (let i = 0; i < n; i++) {
  //     const num = candidates[i]
  //     const last = path[path.length - 1]
  //     if (!last || last <= num) {
  //       path.push(num)
  //       partSum += num
  //       dfs(path, partSum)
  //       path.pop()
  //       partSum -= num
  //     }
  //   }
  // }
  //
  // dfs([], 0)
  // return paths

  const paths = []
  const n = candidates.length

  // /**
  //  * @param index 从index开始寻找
  //  * @param target
  //  * @param path
  //  */
  // function dfs(index, target, path) {
  //   if (target <= 0) {
  //     if (target === 0) {
  //       paths.push(path.slice())
  //     }
  //     return
  //   }
  //   for (let i = index; i < n; i++) {
  //     target -= candidates[i]
  //     path.push(candidates[i])
  //     dfs(i, target, path)
  //     path.pop()
  //     // 注意：这一句重置是要写的，因为for循环的下次可能用到target变量，或者你可以采用函数的形参传递过去，换而言之，上面的 target -= candidates[i] 对target产生了副作用
  //     target += candidates[i]
  //   }
  // }

  // 先排序的剪枝优化
  candidates.sort((a, b) => a - b)

  function dfs(index, target, path) {
    // 这里只做等于0的判断即可，小于0的放在下面for循环中
    if (target === 0) {
      paths.push(path.slice())
      return
    }
    for (let i = index; i < n; i++) {
      const num = target - candidates[i]
      if (num < 0) {
        break
      }
      path.push(candidates[i])
      // 注意：由于每一个元素可以重复使用，下一轮搜索的起点依然是 i，这里非常容易弄错
      dfs(i, num, path)
      path.pop()
    }
  }

  dfs(0, target, [])
  return paths
};

candidates = [2, 3, 6, 7], target = 7;
candidates = [2, 3, 5], target = 8;

const res = combinationSum(candidates, target)
console.log(res)
