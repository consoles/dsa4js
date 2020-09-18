/**
 * @param {number[][]} matrix
 * @param {number} target
 * @return {boolean}
 */
var searchMatrix = function (matrix, target) {

  // 暴力法

  const m = matrix.length
  if (m <= 0) return false
  const n = matrix[0].length
  //
  // for (let i = 0; i < m; i++) {
  //   for (let j = 0; j < n; j++) {
  //     if (matrix[i][j] === target) return true
  //   }
  // }
  // return false

  // 从右上角开始逼近
  let i = 0, j = n - 1
  while (i < m && j >= 0) {
    const item = matrix[i][j]
    if (item === target) {
      return true
    }
    if (item < target) {
      i++
    } else {
      j--
    }
  }
  return false
};

const matrix = [
  [1, 4, 7, 11, 15],
  [2, 5, 8, 12, 19],
  [3, 6, 9, 16, 22],
  [10, 13, 14, 17, 24],
  [18, 21, 23, 26, 30]
]

console.log(searchMatrix(matrix, 5))
console.log(searchMatrix(matrix, 20))
