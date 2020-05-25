/**
 * @param {string} s
 * @return {number}
 */
var lengthOfLastWord = function (s) {
  // 双指针，i,j初始化的时候指向字符串末尾，j指向从后向前的第一个非空格字符，i指向从后向前的第一个空格字符
  // let i = s.length - 1, j = s.length - 1;
  // while (j >= 0) {
  //   if (s[j] === ' ') {
  //     j--;
  //     i--;
  //   } else {
  //     break;
  //   }
  // }
  // while (i >= 0) {
  //   if (s[i] !== ' ') {
  //     i--;
  //   } else {
  //     break;
  //   }
  // }
  //
  // return Math.max(0, j - i);

  if (!s || s.length === 0) return 0;
  let count = 0;
  for (let i = s.length - 1; i >= 0; i--) {
    // 从右向左的第一个非空格字符开始计数，再次遇到空格则终止
    if (s[i] === ' ') {
      if (count === 0) continue;
      break;
    }
    count++;
  }
  return count;
};

lengthOfLastWord(' HelloWorld ');
