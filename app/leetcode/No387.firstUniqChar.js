/**
 * @param {string} s
 * @return {number}
 */
var firstUniqChar = function (s) {
    // 最简单的方法，对每个单词进行计数得到一个计数器，然后从前向后扫描字符串，返回第一个次数为1的索引
    const counter = new Map();
    for (const c of s) {
        const count = counter.get(c) || 0;
        counter.set(c, count + 1);
    }
    for (let i = 0; i < s.length; i++) {
        const c = s[i];
        const count = counter.get(c);
        if (count === 1) {
            return i;
        }
    }
    return -1;
};

s = "leetcode";
s = "loveleetcode"
let index = firstUniqChar(s);
debugger;
