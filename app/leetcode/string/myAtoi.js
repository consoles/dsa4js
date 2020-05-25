// 请你来实现一个 atoi 函数，使其能将字符串转换成整数。
//
// 首先，该函数会根据需要丢弃无用的开头空格字符，直到寻找到第一个非空格的字符为止。接下来的转化规则如下：
//
// 如果第一个非空字符为正或者负号时，则将该符号与之后面尽可能多的连续数字字符组合起来，形成一个有符号整数。
// 假如第一个非空字符是数字，则直接将其与之后连续的数字字符组合起来，形成一个整数。
// 该字符串在有效的整数部分之后也可能会存在多余的字符，那么这些字符可以被忽略，它们对函数不应该造成影响。
// 注意：假如该字符串中的第一个非空格字符不是一个有效整数字符、字符串为空或字符串仅包含空白字符时，则你的函数不需要进行转换，即无法进行有效转换。
//
// 在任何情况下，若函数不能进行有效的转换时，请返回 0 。
//
// 提示：
//
// 本题中的空白字符只包括空格字符 ' ' 。
// 假设我们的环境只能存储 32 位大小的有符号整数，那么其数值范围为 [−231,  231 − 1]。如果数值超过这个范围，请返回  INT_MAX (231 − 1) 或 INT_MIN (−231) 。
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/string-to-integer-atoi
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

/**
 * @param {string} str
 * @return {number}
 */
var myAtoi = function (str) {

  const min = -(2 ** 31);
  const max = 2 ** 31 - 1;

  // let num = 0;
  // let isNegative = false;
  // let parseFlag = false;
  // for (const c of str) {
  //
  //   const offset = c.charCodeAt(0) - '0'.charCodeAt(0);
  //
  //   if (!parseFlag) {
  //     if (c === ' ') continue;
  //     if (c === '-') {
  //       isNegative = true;
  //       parseFlag = true;
  //       continue;
  //     }
  //     if (c === '+') {
  //       parseFlag = true;
  //       continue;
  //     }
  //     if (offset < 0 || offset > 9) {
  //       break;
  //     }
  //     num *= 10;
  //     num += offset;
  //     parseFlag = true;
  //   } else {
  //     if (offset >= 0 && offset <= 9) {
  //       num *= 10;
  //       num += offset;
  //       if (!parseFlag) {
  //         parseFlag = true;
  //       }
  //     } else {
  //       break;
  //     }
  //   }
  // }
  //
  // if (isNegative) num *= -1;
  //
  // num = Math.min(num, max);
  // num = Math.max(num, min);
  //
  // return num;

  // 有限状态机解法
  const START = 'start';
  const SIGNED = 'signed';
  const IN_NUM = 'in_number';
  const END = 'end';

  // https://leetcode-cn.com/problems/string-to-integer-atoi/solution/zi-fu-chuan-zhuan-huan-zheng-shu-atoi-by-leetcode-/
  class Automation {
    constructor() {
      const map = new Map();
      map.set(START, [START, SIGNED, IN_NUM, END]);
      map.set(SIGNED, [END, END, IN_NUM, END]);
      map.set(IN_NUM, [END, END, IN_NUM, END]);
      map.set(END, [END, END, END, END]);
      this.map = map;
      this.state = START;
      this.sign = 1;
      this.ans = 0;
    }

    getCol(c) {
      if (c === ' ') return 0;
      if (c === '+' || c === '-') return 1;
      const offset = c.charCodeAt(0) - '0'.charCodeAt(0);
      if (offset >= 0 && offset <= 9) return 2;
      return 3;
    }

    get(c) {
      const state = this.state = this.map.get(this.state)[this.getCol(c)];
      if (state === IN_NUM) {
        this.ans = this.ans * 10 + c.charCodeAt(0) - '0'.charCodeAt(0);
        if (this.sign === 1) {
          this.ans = Math.min(this.ans, max);
        } else {
          this.ans = Math.min(this.ans, -min);
        }
      } else if (state === SIGNED) {
        this.sign = c === '+' ? 1 : -1;
      }
    }
  }

  const automaton = new Automation();
  for (const c of str) {
    automaton.get(c);
  }
  return automaton.sign * automaton.ans;
};

// let str = '42';
// let ret = myAtoi(str);
// console.log(typeof ret, ret);
//
// str = '   -42';
// ret = myAtoi(str);
// console.log(typeof ret, ret);
//
// str = '4193 with words';
// ret = myAtoi(str);
// console.log(typeof ret, ret);
//
// str = 'words and 987';
// ret = myAtoi(str);
// console.log(typeof ret, ret);

str = '-91283472332';
ret = myAtoi(str);
console.log(typeof ret, ret);

str = '  -0012a42';
ret = myAtoi(str);
console.log(typeof ret, ret);
