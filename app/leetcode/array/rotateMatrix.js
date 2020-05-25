// 旋转图像

// 给定一个 n × n 的二维矩阵表示一个图像。
//
// 将图像顺时针旋转 90 度。
//
// 说明：
//
// 你必须在原地旋转图像，这意味着你需要直接修改输入的二维矩阵。请不要使用另一个矩阵来旋转图像。
//
// 示例 1:
//
// 给定 matrix =
//   [
//     [1,2,3],
//     [4,5,6],
//     [7,8,9]
//   ],
//
//   原地旋转输入矩阵，使其变为:
//   [
//     [7,4,1],
//     [8,5,2],
//     [9,6,3]
//   ]
// 示例 2:
//
// 给定 matrix =
//   [
//     [ 5, 1, 9,11],
//     [ 2, 4, 8,10],
//     [13, 3, 6, 7],
//     [15,14,12,16]
//   ],
//
//   原地旋转输入矩阵，使其变为:
//   [
//     [15,13, 2, 5],
//     [14, 3, 4, 1],
//     [12, 6, 8, 9],
//     [16, 7,10,11]
//   ]

/**
 * @param {number[][]} matrix
 * @return {void} Do not return anything, modify matrix in-place instead.
 */
var rotate = function (matrix) {
  // 第一行第x个位置的元素变为最后一列的第x个元素（0,x）=> (x,n-1)
  // 对于矩阵中的i行第j列的元素，旋转后变为第j行的倒数第i列 matrix[i][j] -> matrix[j][n-1-i]

  const n = matrix.length;

  // 方法1：引入辅助数组
  // const m = new Array(n);
  // for (let i = 0; i < n; i++) {
  //   m[i] = new Array(n);
  // }
  // for (let i = 0; i < n; i++) {
  //   for (let j = 0; j < n; j++) {
  //     m[j][n - 1 - i] = matrix[i][j];
  //   }
  // }
  // for (let i = 0; i < n; i++) {
  //   for (let j = 0; j < n; j++) {
  //     matrix[i][j] = m[i][j];
  //   }
  // }

  function swap(x1, y1, x2, y2) {
    [matrix[x1][y1], matrix[x2][y2]] = [matrix[x2][y2], matrix[x1][y1]];
  }

  // 水平翻转  => 对角线翻转
  for (let i = 0; i < parseInt(n / 2); i++) {
    for (let j = 0; j < n; j++) {
      swap(i, j, n - 1 - i, j);
    }
  }
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < i; j++) {
      swap(i, j, j, i);
    }
  }
};
