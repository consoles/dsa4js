// 给你一幅由 N × N 矩阵表示的图像，其中每个像素的大小为 4 字节。请你设计一种算法，将图像旋转 90 度。
//
// 不占用额外内存空间能否做到？
//
//  
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
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/rotate-matrix-lcci
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

/**
 * @param {number[][]} matrix
 * @return {void} Do not return anything, modify matrix in-place instead.
 * 第一行的第x个元素在旋转后恰好是倒数第一列的第x个元素
 * 对于矩阵中第 i 行的第 j 个元素，在旋转后，它出现在第j行的倒数第 i 列。
 * matrix[row][col]  = matrix[col][n-row-1]
 */
var rotate = function (matrix) {
  const n = matrix.length;
  // const m = new Array(n);
  // for (let i = 0; i < n; i++) {
  //   m[i] = new Array(n);
  // }
  // for (let i = 0; i < n; i++) {
  //   for (let j = 0; j < n; j++) {
  //     m[j][n - i - 1] = matrix[i][j];
  //   }
  // }
  // for (let i = 0; i < n; i++) {
  //   for (let j = 0; j < n; j++) {
  //     matrix[i][j] = m[i][j];
  //   }
  // }

  function _swap(x1, y1, x2, y2) {
    [matrix[x1][y1], matrix[x2][y2]] = [matrix[x2][y2], matrix[x1][y1]];
  }

  // 2次翻转：水平翻转（第一行交换最后一行,第二行交换倒数第二行）,对角线翻转
  for (let i = 0; i < parseInt(n / 2); i++) {
    for (let j = 0; j < n; j++) {
      _swap(i, j, n - i - 1, j);
    }
  }
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < i; j++) {
      _swap(i, j, j, i);
    }
  }
};
