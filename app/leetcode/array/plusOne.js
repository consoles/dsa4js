// 加一
// 给定一个由整数组成的非空数组所表示的非负整数，在该数的基础上加一。
//
// 最高位数字存放在数组的首位， 数组中每个元素只存储单个数字。
//
// 你可以假设除了整数 0 之外，这个整数不会以零开头。
//
// 示例 1:
//
// 输入: [1,2,3]
// 输出: [1,2,4]
// 解释: 输入数组表示数字 123。
// 示例 2:
//
// 输入: [4,3,2,1]
// 输出: [4,3,2,2]
// 解释: 输入数组表示数字 4321。

/**
 * @param {number[]} digits
 * @return {number[]}
 */
var plusOne = function (digits) {

  // 方法1：模拟,出错：存在溢出的问题

  // const n = digits.length;
  // let sz = n;
  //
  // let sum = 0;
  //
  // while (sz--) {
  //   const num = digits.shift();
  //   sum = sum * 10 + num;
  // }
  //
  // sum += 1;
  //
  // while (sum > 0) {
  //   const d = sum % 10;
  //   sum = parseInt(sum / 10);
  //   digits.unshift(d);
  // }
  //
  // return digits;

  // const res = [];
  // let i = digits.length;
  // let carry = 1;
  //
  // while (i--) {
  //   const sum = carry + digits[i];
  //   res.push(sum % 10);
  //   carry = parseInt(sum / 10);
  // }
  //
  // if (carry === 1) {
  //   res.push(carry);
  // }
  //
  // return res.reverse();

  // 分析：由于是加1操作，所以进位只有可能是9+1 = 10这种情况，可以简化
  for (let i = digits.length - 1; i >= 0; i--) {
    digits[i]++;
    digits[i] = digits[i] % 10;
    // 没有发生进位
    if (digits[i] !== 0) {
      return digits;
    }
  }

  // 99999 + 1全部进位的情况，此时digits是全0
  // digits = new Array(digits.length + 1).fill(0);
  // digits[0] = 1;
  digits.unshift(1);
  return digits;
};

const res = plusOne([4, 3, 2, 1]);
console.log(res);
