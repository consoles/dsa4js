/**
 * @param {string} s
 * @return {number}
 */
var lengthOfLongestSubstring = function (s) {
  // 暴力法
  // let len = 0;
  // // 双指针,[i,j]
  // let i = 0, j = 0;
  // while (i < s.length) {
  //   j = i;
  //   const set = new Set();
  //   while (j < s.length && !set.has(s[j])) {
  //     set.add(s[j]);
  //     j++;
  //   }
  //   len = Math.max(len, set.size);
  //   i++;
  // }
  // return len;


  // 滑动窗口，例如字符串(a)bcabc，的最长无重复子串为(abc)abc,a(b)cabc的最长无重复子串为a(bca)bc
  // 我们可以得到：无重复元素的左边界为i,右边界为j，[i,j]为闭区间，则[i+1,j]最起码是无重复的，即以i+1起始的字符串我们不需要一个字符一个字符去尝试（上面的暴力法），只需要从j开始尝试，直到右侧出现了重复字符

  const set = new Set(); // 记录某个字符是否出现过
  let i = 0; // 左指针
  let j = -1; // 右指针，初始值为 -1，相当于我们在字符串的左边界的左侧，还没有开始移动
  const n = s.length;

  let len = 0;

  while (i < n) {
    // 左指针向右移动一格，移除窗口中的一个字符
    if (i !== 0) {
      set.delete(s[i - 1]);
    }
    // 如果窗口中没有重复元素，则不断移动右指针放大窗口
    while (j + 1 < n && !set.has(s[j + 1])) {
      set.add(s[++j]);
    }
    // 窗口[i,j]是无重复元素的最长子串
    len = Math.max(len, j - i + 1);
    i++;
  }
  return len;
};

// console.log(lengthOfLongestSubstring('abcabcbb'));
// console.log(lengthOfLongestSubstring('bbbbb'));
console.log(lengthOfLongestSubstring('pwwkew'));
