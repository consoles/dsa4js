/**
 * @param {string} s
 * @return {number}
 */
var romanToInt = function (s) {

  const map = {
    'I': 1,
    'V': 5,
    'X': 10,
    'L': 50,
    'C': 100,
    'D': 500,
    'M': 1000
  };

  let sum = 0;
  let i = 0;
  while (i < s.length) {
    let num = map[s[i]];
    if (i + 1 < s.length) {
      const nextNum = map[s[i + 1]];
      if (nextNum > num) {
        num = nextNum - num;
        i++;
      }
    }
    sum += num;
    i++;
  }
  return sum;
};

romanToInt('IIX');
