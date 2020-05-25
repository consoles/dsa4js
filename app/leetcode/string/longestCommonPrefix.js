// 编写一个函数来查找字符串数组中的最长公共前缀。
//
// 如果不存在公共前缀，返回空字符串 ""。
//
// 示例 1:
//
// 输入: ["flower","flow","flight"]
// 输出: "fl"
// 示例 2:
//
// 输入: ["dog","racecar","car"]
// 输出: ""
// 解释: 输入不存在公共前缀。
// 说明:
//
//   所有输入只包含小写字母 a-z 。

/**
 * @param {string[]} strs
 * @return {string}
 */
var longestCommonPrefix = function (strs) {
  // let res = '';
  // let i = 0;
  // while (true) {
  //   let c = null;
  //   for (const str of strs) {
  //     if (i >= str.length) {
  //       return res;
  //     }
  //     if (!c) c = str[i];
  //     else if (c !== str[i]) {
  //       return res;
  //     }
  //   }
  //   if (!c) {
  //     return res;
  //   }
  //   res += c;
  //   i++;
  // }

  // if (strs.length === 0) return '';
  // let prefix = strs[0];
  // for (let i = 1; i < strs.length; i++) {
  //   while (strs[i].indexOf(prefix) !== 0) {
  //     prefix = prefix.substring(0, prefix.length - 1);
  //     if (prefix.length === 0) return '';
  //   }
  // }
  // return prefix;

  // if (strs.length === 0) return '';
  // let res = strs[0];
  // for (let i = 0; i < strs[0].length; i++) {
  //   const c = strs[0][i];
  //   for (let j = 1; j < strs.length; j++) {
  //     if (i === strs[j].length || strs[j][i] !== c) {
  //       return res.slice(0, i);
  //     }
  //   }
  // }
  // return res;

  function commonPrefix(str1, str2) {
    const minStr = str1.length < str2.length ? str1 : str2;
    let res = minStr;
    for (let i = 0; i < minStr.length; i++) {
      if (str1[i] !== str2[i]) {
        return res.slice(0, i);
      }
    }
    return res;
  }

  // 分治
  function lcp(l, r) {
    if (l === r) return strs[l];
    const mid = l + parseInt((r - l) / 2);
    const lcpLeft = lcp(l, mid);
    const lcpRight = lcp(mid + 1, r);
    return commonPrefix(lcpLeft, lcpRight);
  }

  if (!strs || strs.length === 0) return '';
  return lcp(0, strs.length - 1);
};

const ret = longestCommonPrefix(['flower', 'flow', 'flight']);
console.log(ret);
