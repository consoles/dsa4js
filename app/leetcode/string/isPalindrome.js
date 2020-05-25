// 验证回文字符串

// 给定一个字符串，验证它是否是回文串，只考虑字母和数字字符，可以忽略字母的大小写。
//
// 说明：本题中，我们将空字符串定义为有效的回文串。
//
// 示例 1:
//
// 输入: "A man, a plan, a canal: Panama"
// 输出: true
// 示例 2:
//
// 输入: "race a car"
// 输出: false

/**
 * @param {string} s
 * @return {boolean}
 */
var isPalindrome = function (s) {
  // 双指针
  let i = 0, j = s.length - 1;
  const re = /[0-9a-zA-Z]/;
  while (i < j) {
    if (!re.test(s[i])) {
      i++;
      continue;
    }
    if (!re.test(s[j])) {
      j--;
      continue;
    }
    if (s[i].toLowerCase() !== s[j].toLowerCase()) {
      return false;
    }
    i++;
    j--;
  }
  return true;
};

const s = 'race a car';
let res = isPalindrome(s);
console.log(res);
