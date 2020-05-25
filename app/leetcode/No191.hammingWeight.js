/**
 * @param {number} n - a positive integer
 * @return {number}
 */
var hammingWeight = function (n) {
  let count = 0;
  // while (n) {
  //   count += (n & 1);
  //   n >>>= 1;
  // }

  // let mask = 1;
  // for (let i = 0; i < 32; i++) {
  //   if ((n & mask) !== 0) {
  //     count++;
  //   }
  //   mask <<= 1;
  // }

  while (n) {
    count++;
    // 对于任意数字n, n & (n-1) 会将最后的1个1变成0，其他位不变
    n &= (n - 1);
  }

  return count;
};
