/**
 * @param {string} s
 * @return {boolean}
 */
var validPalindrome = function (s) {
  let l = 0, r = s.length - 1;
  while (l < r) {
    const c1 = s[l], c2 = s[r];
    if (c1 === c2) {
      l++;
      r--;
    } else {
      let flag = true;
      // 跳过l这个字符后是不是回文串?
      for (let ll = l + 1, rr = r; ll < rr; ll++, rr--) {
        if (s[ll] !== s[rr]) {
          flag = false;
          break;
        }
      }
      if (flag) return flag;
      // 跳过r这个字符后是不是回文串？
      for (let ll = l, rr = r - 1; ll < rr; ll++, rr--) {
        if (s[ll] !== s[rr]) {
          return false;
        }
      }
      return true;
    }
  }
  return true;
};
