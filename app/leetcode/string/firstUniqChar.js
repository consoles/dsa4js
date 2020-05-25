// 字符串中的第一个唯一字符

// 给定一个字符串，找到它的第一个不重复的字符，并返回它的索引。如果不存在，则返回 -1。
//
// 案例:
//
//   s = "leetcode"
// 返回 0.
//
// s = "loveleetcode",
//   返回 2.
//
//
// 注意事项：您可以假定该字符串只包含小写字母。

/**
 * @param {string} s
 * @return {number}
 */
var firstUniqChar = function (s) {

  // 对字符进行计数
  const counter = {};
  for (const c of s) {
    counter[c] = counter[c] || 0;
    counter[c]++;
  }

  for (let i = 0; i < s.length; i++) {
    if (counter[s[i]] === 1) {
      return i;
    }
  }

  return -1;
};

s = 'loveleetcode';
let i = firstUniqChar(s);
console.log(i);
