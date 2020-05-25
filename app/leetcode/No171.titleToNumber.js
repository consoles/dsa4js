/**
 * @param {string} s
 * @return {number}
 */
var titleToNumber = function (s) {
  // 模仿科学计数法
  // let res = 0;
  // if (!s || !s.length) return res;
  // const map = new Map();
  // const radix = 26;
  // for (let i = 0; i < radix; i++) {
  //   map.set(String.fromCodePoint(65 + i), i + 1);
  // }
  // let pow = 0;
  // for (let i = s.length - 1; i >= 0; i--) {
  //   const base = map.get(s[i]);
  //   res += base * (radix ** pow);
  //   pow++;
  // }
  // return res;

  let res = 0;
  if (!s || !s.length) return res;

  for (let i = 0; i < s.length; i++) {
    const num = s.codePointAt(i) - 65 + 1;
    res = res * 26 + num;
  }
  return res;
};

console.log(titleToNumber('A'));
console.log(titleToNumber('AB'));
console.log(titleToNumber('ZY'));
