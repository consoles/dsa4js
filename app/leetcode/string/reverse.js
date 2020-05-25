// 整数反转

// 给出一个 32 位的有符号整数，你需要将这个整数中每位上的数字进行反转。
//
// 示例 1:
//
// 输入: 123
// 输出: 321
// 示例 2:
//
// 输入: -123
// 输出: -321
// 示例 3:
//
// 输入: 120
// 输出: 21
// 注意:
//
//   假设我们的环境只能存储得下 32 位的有符号整数，则其数值范围为 [−2^31,  2^31 − 1]。请根据这个假设，如果反转后整数溢出那么就返回 0。

/**
 * @param {number} x
 * @return {number}
 *
 * 注意溢出 的问题
 */
var reverse = function (x) {
  let sum = 0;
  const min = -(2 ** 31);
  const max = 2 ** 31 - 1;
  while (x) {
    const pop = x % 10;
    // 7是2^31 - 1的个位数max % 10
    if (sum > max / 10 || sum === max / 10 && pop > 7) return 0;
    if (sum < min / 10 || sum === min / 10 && pop < -8) return 0;
    sum = sum * 10 + pop;
    x = parseInt(x / 10);
  }
  return sum;
};

const ret = reverse(1534236469);
debugger
