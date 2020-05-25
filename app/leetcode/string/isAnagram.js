// 给定两个字符串 s 和 t ，编写一个函数来判断 t 是否是 s 的字母异位词。

// 示例 1:
//
// 输入: s = "anagram", t = "nagaram"
// 输出: true
// 示例 2:
//
// 输入: s = "rat", t = "car"
// 输出: false
// 说明:
//   你可以假设字符串只包含小写字母。
//
// 进阶:
//   如果输入字符串包含 unicode 字符怎么办？你能否调整你的解法来应对这种情况？

/**
 * @param {string} s
 * @param {string} t
 * @return {boolean}
 */
var isAnagram = function (s, t) {
  if (s.length !== t.length) return false;
  // 排序后每个位置上的字符相同
  // const code1 = [];
  // for (const c of s) {
  //   code1.push(c.charCodeAt(0));
  // }
  // code1.sort((a, b) => a - b);
  // const code2 = [];
  // for (const c of t) {
  //   code2.push(c.charCodeAt(0));
  // }
  // code2.sort((a, b) => a - b);
  // for (let i = 0; i < code1.length; i++) {
  //   if (code1[i] !== code2[i]) {
  //     return false;
  //   }
  // }
  // return true;

  // 字符计数
  const counter = {};
  // for (const c of s) {
  //   counter[c] = counter[c] || 0;
  //   counter[c]++;
  // }
  //
  // for (const c of t) {
  //   if (counter[c] <= 0 || !counter[c]) return false;
  //   counter[c]--;
  // }
  // return true;

  // s负责增加计数器，t负责减少计数器,如果s和t中都是相同字符，则计数器应该是0
  for (let i = 0; i < s.length; i++) {
    const c1 = s[i];
    const c2 = t[i];
    counter[c1] = counter[c1] || 0;
    counter[c2] = counter[c2] || 0;
    counter[c1]++;
    counter[c2]--;
  }
  // 检查
  for (const k of Object.keys(counter)) {
    if (counter[k] !== 0) {
      return false;
    }
  }
  return true;
};
