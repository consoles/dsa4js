/**
 * @param {number} x
 * @return {number}
 */
var mySqrt = function (x) {
  // let ans = 1;
  // while (ans * ans < x) {
  //   ans++;
  // }
  // if (ans * ans > x) ans--;
  // return ans;

  // 二分法：一个数的平方根肯定不会超过自己的一半

  // let l = 0;
  // let r = parseInt(x / 2) + 1;
  // while (l < r) {
  //   const mid = (l + r) >> 1;
  //   const num = mid * mid;
  //   if (x === num) {
  //     return mid;
  //   }
  //   if (num < x) {
  //     l = mid + 1;
  //   } else {
  //     r = mid - 1;
  //   }
  // }
  // if (l * l > x) {
  //   return l - 1;
  // }
  // return l;

  // 换底公式
  // x^(1/2) = e^(1/2*lnX)
  // if (x === 0) return 0;
  // // 指数函数和对数函数的参数和返回值均为浮点数，因此运算过程中会存在误差。
  // const ans = parseInt(Math.exp(0.5 * Math.log(x)));
  // if ((ans + 1) ** 2 <= x) return ans + 1;
  // return ans;

  // 优化的二分查找，不需要考虑边界值了
  let l = 0, r = x, ans = 0;
  while (l <= r) {
    const mid = (l + r) >> 1;
    if (mid * mid <= x) {
      ans = mid;
      l = mid + 1;
    } else {
      r = mid - 1;
    }
  }
  return ans;

  // 牛顿迭代法，不是我等凡夫俗子想得出来的

};

let res = mySqrt(17);
console.log(res);
