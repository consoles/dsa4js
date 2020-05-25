/**
 * 10只有2和5相乘可以得到，我们只需要找到有多少个2-5对即可
 * 含有2的因子每2个出现1次，含有5的因子每5个出现1次，因此对于每一个因子5一定能找到一个因子2与之对应，我们统计5的因子的个数即可
 * @param {number} n
 * @return {number}
 */
var trailingZeroes = function (n) {

  // let count2 = 0, count5 = 0;
  //
  // for (let i = 2; i <= n; i++) {
  //   let value = i;
  //   while (value % 2 === 0) {
  //     count2++;
  //     value = parseInt(value / 2);
  //   }
  //   while (value % 5 === 0) {
  //     count5++;
  //     value = parseInt(value / 5);
  //   }
  // }
  // return Math.min(count2, count5);

  // let count = 0;
  // for (let i = 1; i <= n; i++) {
  //   let N = i;
  //   while (N) {
  //     if (N % 5 === 0) {
  //       count++;
  //       N = parseInt(N / 5);
  //     } else {
  //       break;
  //     }
  //   }
  // }
  // return count;

  let nCount = 0;
  while (n) {
    nCount += parseInt(n / 5);
    n = parseInt(n / 5);
  }
  return nCount;
};
