/**
 * @param {number[]} A
 * @return {boolean}
 */
var validMountainArray = function (A) {
  // 双指针：从左向右找最大，从右向走找最大，判断左右指针是否重合
  const len = A.length
  let l = 0, r = len - 1
  while (l + 1 < len && A[l] < A[l + 1]) {
    l++
  }
  while (r > 0 && A [r - 1] > A[r]) {
    r--
  }
  return l > 0 && r < len - 1 && l === r
};

