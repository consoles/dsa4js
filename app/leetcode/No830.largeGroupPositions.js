/**
 * @param {string} s
 * @return {number[][]}
 */
var largeGroupPositions = function (s) {
    const res = [];
    const len = s.length;
    // let i = 0, j = 0;
    // // 双指针
    // while (i < len && j < len) {
    //     const item = s[i];
    //     while (j < len && s[j] === item) {
    //         j++;
    //     }
    //     if (j - i >= 3) {
    //         res.push([i, j - 1]);
    //     }
    //     i = j;
    // }
    // return res;

    // 1次遍历
    let num = 1;
    for (let i = 0; i < len; i++) {
        if (i === len - 1 || s[i] !== s[i + 1]) {
            if (num >= 3) {
                res.push([i - num + 1, i]);
            }
            num = 1;
        } else {
            num++;
        }
    }
    return res;
};

const s = "abbxxxxzzy";
// const s = "abc";
// const s = "abcdddeeeeaabbbcd"
const res = largeGroupPositions(s);
debugger
