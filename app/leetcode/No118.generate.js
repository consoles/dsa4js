/**
 * @param {number} numRows
 * @return {number[][]}
 */
var generate = function (numRows) {

  const res = [];
  for (let i = 0; i < numRows; i++) {
    res[i] = [];
    // 第一行
    if (!res[i - 1]) {
      res[i] = [[1]];
    } else {
      for (let j = 0; j <= i; j++) {
        const x = res[i - 1][j - 1];
        const y = res[i - 1][j];
        let sum = 1;
        if (x && y) {
          sum = x + y;
        }
        res[i][j] = sum;
      }
    }
  }
  return res;
};

generate(5);
