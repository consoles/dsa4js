// 给定一个字符串，逐个翻转字符串中的每个单词。
//
//  
//
// 示例 1：
//
// 输入: "the sky is blue"
// 输出: "blue is sky the"
// 示例 2：
//
// 输入: "  hello world!  "
// 输出: "world! hello"
// 解释: 输入字符串可以在前面或者后面包含多余的空格，但是反转后的字符不能包括。
// 示例 3：
//
// 输入: "a good   example"
// 输出: "example good a"
// 解释: 如果两个单词间有多余的空格，将反转后单词间的空格减少到只含一个。
//  
//
// 说明：
//
// 无空格字符构成一个单词。
// 输入字符串可以在前面或者后面包含多余的空格，但是反转后的字符不能包括。
// 如果两个单词间有多余的空格，将反转后单词间的空格减少到只含一个。
//  
//
// 进阶：
//
// 请选用 C 语言的用户尝试使用 O(1) 额外空间复杂度的原地解法。
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/reverse-words-in-a-string
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

/**
 * @param {string} s
 * @return {string}
 */
var reverseWords = function (s) {
  // 方案1：系统api
  // return s.trim().split(/\s+/).reverse().join(' ');

  // 1. 去除不必要的空格
  // 2. 反转整个字符串
  // 3. 反转每个单词
  // let i = 0, j = s.length - 1;
  // while (i <= j && s[i] === ' ') i++;
  // while (i <= j && s[j] === ' ') j--;
  // let ss = [];
  // while (i <= j) {
  //   const c = s[i];
  //   if (c !== ' ') ss.push(c);
  //   else if (ss[ss.length - 1]!== ' ') ss.push(c);
  //   i++;
  // }
  //
  // function reverse(ss, start, end) {
  //   while (start < end) {
  //     [ss[start], ss[end]] = [ss[end], ss[start]];
  //     start++;
  //     end--;
  //   }
  // }
  //
  // // 此时的ss已经被去除了头尾空格并且中间空格只有1个
  // // 反转整个字符串
  // reverse(ss, 0, ss.length-1);
  // // 反转每个单词
  // let start = 0, end = 0;
  // const n = ss.length;
  // while (start < n) {
  //   // 循环到单词末尾
  //   while (end < n && ss[end] !== ' ') end++;
  //   // 反转单词
  //   reverse(ss, start, end - 1);
  //   // 更新start
  //   start = end + 1;
  //   end = start;
  // }
  // return ss.join('');

  // 借助栈：扫描单词不断入栈，最先入栈的元素在最底部
  // const stack = [];
  // let i = 0;
  // let ss = '';
  // while (i < s.length) {
  //   const c = s[i];
  //   if (c !== ' ') {
  //     ss += c;
  //   } else {
  //     if (ss.length) {
  //       stack.push(ss);
  //     }
  //     ss = '';
  //   }
  //   i++;
  // }
  // if (ss.length) {
  //   stack.push(ss);
  // }
  // ss = '';
  // while (stack.length) {
  //   ss += stack.pop() + ' ';
  // }
  // return ss.trimRight();

  // 从右向左扫描，见到单词则加入res
  let ss  = '';
  let start = s.length - 1, end = start;
  while (start >= 0) {
    // end位于从右向左的第一个非空格地方
    while (end >= 0 && s[end] === ' ') end--;
    start = end;
    // start位于从右向左的第一个空格处
    while (start >= 0 && s[start] !== ' ') start--;
    // 单词[start+1,end]
    ss += s.slice(start + 1,end + 1) + ' ';
    end = start;
  }
  return ss.trimEnd();
};

let ret = reverseWords('a good    example');
console.log(ret, ret.length);
