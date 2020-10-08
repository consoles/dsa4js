/**
 * @param {character[]} s
 * @return {void} Do not return anything, modify s in-place instead.
 */
var reverseString = function (s) {
    // 双指针
    // let i = 0, j = s.length - 1
    // while (i < j) {
    //     [s[i], s[j]] = [s[j], s[i]]
    //     i++
    //     j--
    // }

    // 分治
    function reverse(l, r) {
        if (l >= r) return
        [s[l], s[r]] = [s[r], s[l]]
        reverse(l + 1, r - 1)
    }
    reverse(0, s.length - 1)
};

let s = ["h", "e", "l", "l", "o"]
reverseString(s)
console.log(s);
s = ["H", "a", "n", "n", "a", "h"]
reverseString(s)
console.log(s)
