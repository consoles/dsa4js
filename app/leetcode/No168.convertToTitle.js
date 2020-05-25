/**
 * @param {number} n
 * @return {string}
 */
var convertToTitle = function (n) {
  let str = '';
  const radix = 26;
  while (n) {
    n--;
    str = String.fromCodePoint(65 + (n % radix)) + str;
    n = parseInt(n / 26);
  }
  return str;
};

convertToTitle(28);
