/**
 * @param {string} a
 * @param {string} b
 * @return {string}
 */
var addBinary = function (a, b) {
  let s = '';
  let i = a.length - 1, j = b.length - 1;
  const base = '0'.charCodeAt(0);
  let carry = 0;
  let sum = 0;
  while (i >= 0 || j >= 0) {
    const x = i >= 0 ? a.charCodeAt(i) - base : 0;
    const y = j >= 0 ? b.charCodeAt(j) - base : 0;
    sum = carry + x + y;
    s = sum % 2 + s + '';
    carry = parseInt(sum / 2);
    if (i >= 0) i--;
    if (j >= 0) j--;
  }
  if (carry) {
    s = carry + s;
  }
  return s;
};

// a = "11", b = "1"
a = '1010', b = '1011';
addBinary(a, b);
