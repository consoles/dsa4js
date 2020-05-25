/**
 * @param {character[][]} matrix
 * @return {number}
 */
var maximalSquare = function (matrix) {

  // 暴力法

  // let maxSide = 0; // 最大边长
  // if (!matrix || matrix.length === 0 || matrix[0].length === 0) {
  //   return maxSide;
  // }
  //
  // const rows = matrix.length;
  // const cols = matrix[0].length;
  //
  // function checkSquare(i, j, offset) {
  //   // 检查以(i,j)为左上角，(i+offset,j+offset)为右下角的正方形是否全是1
  //   let flag = true;
  //   for (let x = i; x <= i + offset; x++) {
  //     for (let y = j; y <= j + offset; y++) {
  //       if (matrix[x][y] == 0) {
  //         flag = false;
  //         break;
  //       }
  //     }
  //     if (!flag) {
  //       return false;
  //     }
  //   }
  //   return true;
  // }
  //
  // for (let i = 0; i < matrix.length; i++) {
  //   for (let j = 0; j < matrix[i].length; j++) {
  //     if (matrix[i][j] == 1) {
  //       // 可能的最大边长
  //       const maybeMaxSize = Math.min(rows - i, cols - j);
  //
  //       let side = 1;
  //       while (side < maybeMaxSize) {
  //         if (checkSquare(i, j, side)) {
  //           side++;
  //         } else {
  //           break;
  //         }
  //       }
  //       maxSide = Math.max(maxSide, side);
  //     }
  //   }
  // }
  //
  // return maxSide ** 2;

  // DP
  // dp[i][j]表示以(i,j)为右下角，且只包含1的正方形的边长的最大值。如果我们能计算出所有的dp(i,j)的值，那么其中的最大值即为矩阵中只包含1的正方形的边长的最大值，其平方即为最大正方形的面积
  // 对于每个位置(i,j)，检查在矩阵中该位置的值：
  // 1.如果该位置的值是0，则dp[i][j] = 0，因为当前位置不可能在由1组成的正方形当中
  // 2.如果该位置的值是1，则dp[i][j]的值由其上方、左方和左上方3个相邻位置的dp值决定，具体而言：当前位置的元素值等于相邻3个位置中的最小值加1
  let maxSide = 0;
  if (!matrix || matrix.length === 0 || matrix[0].length === 0) {
    return maxSide;
  }

  const rows = matrix.length, cols = matrix[0].length;

  const dp = new Array(rows);
  for (let i = 0; i < dp.length; i++) {
    dp[i] = new Array(cols);
  }

  for (let i = 0; i < rows; i++) {
    for (let j = 0; j < cols; j++) {
      if (matrix[i][j] == '1') {
        if (i === 0 || j === 0) {
          // 注意边界条件
          dp[i][j] = 1;
        } else {
          dp[i][j] = Math.min(
            dp[i - 1][j],
            dp[i][j - 1],
            dp[i - 1][j - 1]
          ) + 1;
        }
        maxSide = Math.max(maxSide, dp[i][j]);
      } else {
        dp[i][j] = 0;
      }
    }
  }
  return maxSide ** 2;
};

const matrix = [
  [1, 0, 1, 0, 0],
  [1, 0, 1, 1, 1],
  [1, 1, 1, 1, 1],
  [1, 0, 0, 1, 0],
];

const area = maximalSquare(matrix);
console.log(area);
