/**
 * @param {string} s
 * @return {boolean}
 */
var isValid = function (s) {
  const map = {
    ')': '(',
    ']': '[',
    '}': '{'
  };

  const stack = [];
  for (const c of s) {
    // 左括号，入栈
    if (!map[c]) {
      stack.push(c);
    } else {
      const cc = stack.pop();
      if (map[c] !== cc) {
        return false;
      }
    }
  }
  return stack.length === 0;
};
