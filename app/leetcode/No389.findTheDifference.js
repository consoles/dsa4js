/**
 * @param {string} s
 * @param {string} t
 * @return {character}
 */
var findTheDifference = function (s, t) {
    // 模拟的方法，对t字符串进行计数，一个个移除s中的字符，最后保留的字符就是被添加的字符
    // const arr = t.split('');
    // for (const c of s) {
    //     const index = arr.indexOf(c);
    //     if (index !== -1) {
    //         arr.splice(index, 1);
    //     }
    // }
    // return arr[0];

    // 首先遍历字符串 ss，对其中的每个字符都将计数值加 11；然后遍历字符串 tt，对其中的每个字符都将计数值减 11。当发现某个字符计数值为负数时，说明该字符在字符串 tt 中出现的次数大于在字符串 ss 中出现的次数，因此该字符为被添加的字符。
    // const counter = new Map();
    // for (const c of s) {
    //     const count = counter.has(c) ? counter.get(c) : 0;
    //     counter.set(c, count + 1);
    // }
    // for (const c of t) {
    //     if (!counter.has(c)) {
    //         return c;
    //     }
    //     const count = counter.get(c);
    //     if (count <= 0) {
    //         return c;
    //     }
    //     counter.set(c, count - 1);
    // }
    // return ' ';

    // 两个字符串的ascii码值进行分别求和，得到sumS & sumT，sumT - sumS就表示的是被添加的字符
    // let sumS = 0, sumT = 0;
    // for (let i = 0; i < s.length; i++) {
    //     sumS += s.charCodeAt(i);
    // }
    // for (let i = 0; i < t.length; i++) {
    //     sumT += t.charCodeAt(i);
    // }
    // return String.fromCharCode(sumT - sumS);

    // 如果将两个字符串拼接为一个字符串，则问题转化为字符串中出现奇数次的字符，类似于No136只出现一次的数字，可以用位运算解决
    let ret = 0;
    for (let i = 0; i < s.length; i++) {
        ret ^= s.charCodeAt(i);
    }
    for (let i = 0; i < t.length; i++) {
        ret ^= t.charCodeAt(i);
    }
    return String.fromCharCode(ret);
};

// s = "abcd", t = "abcde"
// s = "", t = "y"
// s = "a", t = "aa"
s = "ae", t = "aea"
const res = findTheDifference(s, t);
debugger