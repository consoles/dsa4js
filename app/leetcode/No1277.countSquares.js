/**
 * @param {number[][]} matrix
 * @return {number}
 */
var countSquares = function (matrix) {
  // 暴力法(统计以(i,j)为起点的矩形的个数)
  let count = 0;
  const rows = matrix.length;
  if (rows <= 0) return count;
  const cols = matrix[0].length;

  // function check(i, j, offset) {
  //   for (let x = i; x <= i + offset; x++) {
  //     for (let y = j; y <= j + offset; y++) {
  //       if (matrix[x][y] == '0') {
  //         return false;
  //       }
  //     }
  //   }
  //   return true;
  // }
  //
  // for (let i = 0; i < rows; i++) {
  //   for (let j = 0; j < cols; j++) {
  //     if (matrix[i][j] == 1) {
  //       const maybeMaxSide = Math.min(rows - i, cols - j);
  //       for (let s = 0; s < maybeMaxSide; s++) {
  //         if (check(i, j, s)) {
  //           count++;
  //         }
  //       }
  //     }
  //   }
  // }
  //
  // return count;

  // 动态规划
  // dp[i][j]为右下角的最大全1正方形的边长为x，则以(i,j)为右下角的边长为1的正方形1个，边长为2的正方形1个，边长为x的正方形1个 => 即以(i,j)为右下角的正方形有x个
  const dp = new Array(rows);
  for (let i = 0; i < rows; i++) {
    dp[i] = new Array(cols);
  }

  for (let i = 0; i < rows; i++) {
    for (let j = 0; j < cols; j++) {
      if (i === 0 || j === 0) {
        dp[i][j] = matrix[i][j];
      } else if (matrix[i][j] == 0) {
        dp[i][j] = 0;
      } else {
        dp[i][j] = Math.min(dp[i - 1][j], dp[i][j - 1], dp[i - 1][j - 1]) + 1;
      }
      count += dp[i][j];
    }
  }
  return count;
};

const matrix =
  [
    [0, 1, 1, 1],
    [1, 1, 1, 1],
    [0, 1, 1, 1]
  ];

let res = countSquares(matrix);
console.log(res);
