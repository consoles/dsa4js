/**
 * @param {number} x
 * @return {boolean}
 */
var isPalindrome = function (x) {
  // 方法1，转化为字符串后 用双指针
  // const s = x + '';
  // for (let l = 0, r = s.length - 1; l < r; l++, r--)
  //   if (s[l] !== s[r]) return false;
  // return true;

  // 方法2，数学解法（最高位等于最低位）
  // 1221 / 1000 = 1,1221 % 10 = 1
  // if (x < 0) return false;
  // let div = 1;
  // while (x / div >= 10) div *= 10;
  // while (x) {
  //   const high = parseInt(x / div);
  //   const low = x % 10;
  //   if (high !== low) return false;
  //   x = parseInt((x % div) / 10); // 取余舍弃了最高位 ，除以10再舍弃最低位
  //   div /= 100; // 一下子减少了2位
  // }
  // return true;

  // 方法3，取出后半段数字进行翻转
  // 以0结尾的数肯定不是回文数
  if (x < 0 || (x % 10 === 0 && x !== 0)) return false;

  let revertedNumber = 0;
  // 对半或过半？
  while (x > revertedNumber) {
    revertedNumber = revertedNumber * 10 + x % 10;
    x = parseInt(x / 10);
  }
  // 如果是偶数的话，revertNum 和 x 相等；如果是奇数的话，最中间的数字就在revertNum 的最低位上，将它除以 10 以后应该和 x 相等。
  return x === revertedNumber || x * 10 === revertedNumber;
};
