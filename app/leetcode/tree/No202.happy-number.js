/**
 * @param {number} n
 * @return {boolean}
 */
var isHappy = function (n) {
  const set = new Set();
  while (true) {
    if (set.has(n)) {
      break;
    }
    set.add(n);
    let sum = 0;
    while (n) {
      const num = n % 10;
      n = n / 10 | 0;
      sum += num ** 2;
    }
    if (sum === 1) {
      return true;
    }
    n = sum;
  }
  return false;
};

let ret  = isHappy(3);
console.log(ret)
