/**
 * @param {number[]} A
 * @return {number}
 */
var longestMountain = function (A) {
  // 模拟上坡和下坡
  let ans = 0, n = A.length
  let i = 0
  while (i < n) {
    let up = 0, down = 0
    // 上坡
    while (i < n - 1 && A[i] < A[i + 1]) {
      i++
      up++
    }
    // 下坡
    if (up > 0) {
      while (i < n - 1 && A[i] > A[i + 1]) {
        i++
        down++
      }
    }
    // 找到山脉（同时完成上坡和下坡），更新答案
    if (up > 0 && down > 0) {
      ans = Math.max(ans, up + down + 1)
    }
    // 爬坡失败，需要向前移动一个位置
    if (up === 0) {
      i++
    }
  }
  return ans
};

const A = [2, 1, 4, 7, 3, 2, 5];
const ret = longestMountain(A)
console.log(ret)
