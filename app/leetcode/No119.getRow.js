/**
 * @param {number} rowIndex
 * @return {number[]}
 */
var getRow = function (rowIndex) {
  // 1
  // 1   1
  // 1   2   1
  // 1   3   3   1
  // 1   4   6   4   1
  // 1   5  10  10   5   1
  const res = new Array(rowIndex + 1).fill(0);
  // 每一行的最后一个数是1，从倒数第二个元素开始，它等于上一行在这个 位置的元素  + 上一行在这个 位置的全一个元素
  for (let i = 0; i <= rowIndex; i++) {
    res[i] = 1;
    for (let j = i - 1; j > 0; j--) {
      res[j] += res[j - 1];
    }
  }
  return res;
};
