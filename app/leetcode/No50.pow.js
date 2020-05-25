/**
 * @param {number} x
 * @param {number} n
 * @return {number}
 */
var myPow = function (x, n) {

  // 普通的迭代会超时，这里采用快速幂算法
  function quickMul(x, n) {
    if (n === 0) return 1;
    const y = quickMul(x, parseInt(n / 2));
    return n % 2 === 0 ? y * y : y * y * x;
  }

  return n >= 0 ? quickMul(x, n) : 1 / quickMul(x, -n);
};
