function isPalindrome(s, i, j) {
  while (i < j) {
    if (s[i] !== s[j]) {
      return false;
    }
    i++;
    j--;
  }
  return true;
}

/**
 * 回文串的递归表示
 * @param s
 * @returns {boolean}
 */
function isValid(s) {
  function valid(s, i, j) {
    if (i === j) return true;
    if (s[i] !== s[j]) {
      return false;
    }
    return valid(s, i + 1, j - 1);
  }

  return !s || valid(s, 0, s.length - 1);
}

// console.log(isValid('aba'));
// console.log(isValid('ab'));
// console.log(isValid('aabaa'));

/**
 * 最长回文子串
 * @param {string} s
 * @return {string}
 */
var longestPalindrome = function (s) {
  // if (!s) return s;
  // // 枚举所有长度大于2的子串
  // let res = s.substr(0, 1);
  // let maxLength = 1;
  // for (let i = 0; i < s.length - 1; i++) {
  //   for (let j = i + 1; j < s.length; j++) {
  //     if (j - i + 1 > maxLength && isPalindrome(s, i, j)) {
  //       maxLength = j - i + 1;
  //       res = s.substr(i, maxLength);
  //     }
  //   }
  // }
  // return res;

  // dp
  // 1.定义状态:dp[i][j]表示s[i,j]是否是回文子串

  // 2.思考状态转移方程：dp[i][j] = (s[i] == s[j]) && (dp[i+1][j-1])
  // 分析这个状态转移方程：
  //  1. “动态规划”事实上是在填一张二维表格,i和j的关系是i<=j，因此只需要填写表格的上半部分
  //  2.看到dp[i+1][j-1]就需要考虑边界条件。边界条件是[i+1,j-1]不构成区间，即长度严格小于2，即(j-1) - (i+1) - 1 < 2 => j - i < 3
  // 这个结论很显然：当子串 s[i, j] 的长度等于 2 或者等于 3 的时候，我其实只需要判断一下头尾两个字符是否相等就可以直接下结论了。
  // 如果子串 s[i + 1, j - 1] 只有 1 个字符，即去掉两头，剩下中间部分只有 11 个字符，当然是回文；
  // 如果子串 s[i + 1, j - 1] 为空串，那么子串 s[i, j] 一定是回文子串
  // 因此，在 s[i] == s[j] 成立和 j - i < 3 的前提下，直接可以下结论，dp[i][j] = true，否则才执行状态转移。

  // 3.考虑初始化
  // 初始化的时候，单个字符一定是回文串，因此把对角线先初始化为 1，即 dp[i][i] = 1 ,事实上，初始化的部分都可以省去。因为只有一个字符的时候一定是回文，dp[i][i] 根本不会被其它状态值所参考。

  // 4.考虑输出
  // 只要一得到 dp[i][j] = true，就记录子串的长度和起始位置，没有必要截取，因为截取字符串也要消耗性能，记录此时的回文子串的“起始位置”和“回文长度”即可。

  // 5.考虑状态是否压缩
  // 因为在填表的过程中，只参考了左下方的数值。事实上可以压缩，但会增加一些判断语句，增加代码编写和理解的难度，丢失可读性。在这里不做状态压缩。

  // 下面是编码的时候要注意的事项：总是先得到小子串的回文判定，然后大子串才能参考小子串的判断结果。思路是:
  // 在子串右边界 j 逐渐扩大的过程中，枚举左边界可能出现的位置
  // 左边界枚举的时候可以从小到大，也可以从大到小。

  // const len = s.length;
  // if (len < 2) {
  //   return s;
  // }
  // const dp = new Array(len);
  // for (let i = 0; i < dp.length; i++) dp[i] = new Array(len);
  //
  // // 初始化
  // for (let i = 0; i < len; i++) dp[i][i] = true;
  //
  // let maxLen = 1;
  // let start = 0;
  //
  // for (let j = 1; j < len; j++) {
  //   for (let i = 0; i < j; i++) {
  //
  //     if (s[i] === s[j]) {
  //       if (j - i < 3) {
  //         dp[i][j] = true;
  //       } else {
  //         dp[i][j] = dp[i + 1][j - 1];
  //       }
  //     } else {
  //       dp[i][j] = false;
  //     }
  //
  //     // 只要dp[i][j]成立就表示子串[i,j]是回文，此时记录回文长度和 起始位置
  //     if (dp[i][j]) {
  //       const curLen = j - i + 1;
  //       if (curLen > maxLen) {
  //         maxLen = curLen;
  //         start = i;
  //       }
  //     }
  //   }
  // }
  //
  // return s.substr(start, maxLen);

  // 中心扩散法
  const len = s.length;
  if (len < 2) {
    return s;
  }
  let maxLen = 1;
  let res = s.substr(0, maxLen);

  function centerSpread(s, l, r) {
    // l == r的时候，回文中心是一个字符，回文串的长度是奇数
    // r = l + 1的时候，回文中心是一个空隙 ，回文串的长度 是偶数
    const len = s.length;
    let i = l, j = r;
    while (i >= 0 && j < len) {
      if (s[i] === s[j]) {
        i--;
        j++;
      } else {
        break;
      }
    }
    // 这里要小心，跳出 while 循环时，恰好满足 s[i] != s[j]，因此不能取 i，不能取 j
    return s.substring(i + 1, j);
  }

  // 中心位置枚举到len-2即可
  for (let i = 0; i < len - 1; i++) {
    const oddStr = centerSpread(s, i, i);
    const evenStr = centerSpread(s, i, i + 1);
    const maxLenStr = oddStr.length > evenStr.length ? oddStr : evenStr;
    if (maxLenStr.length > maxLen) {
      maxLen = maxLenStr.length;
      res = maxLenStr;
    }
  }

  return res;
};
