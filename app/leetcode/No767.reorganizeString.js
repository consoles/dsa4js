/**
 * @param {string} S
 * @return {string}
 */
var reorganizeString = function (S) {

    const len = S.length;
    let maxCount = 0;
    let maxChar = '';
    const threshold = Math.ceil(len / 2);

    // 字符计数(计数的同时找出出现最多的字符)
    const counter = new Map();
    for (const s of S) {
        let count = counter.get(s) || 0;
        count++;
        counter.set(s, count);
        if (count > maxCount) {
            maxCount = count;
            maxChar = s;
            // 如果出现次数最多的那个字符的数量大于阈值，说明它不能使得两相邻的字符不同，直接返回空字符串即可
            if (count > threshold) {
                return "";
            }
        }
    }

    // 到这一步说明它可以使得两相邻的字符不同，随便返回一个结果即可
    const res = new Array(len);
    // 先把出现次数最多的字符存储在偶数位置
    let i = 0;
    while (counter.get(maxChar) > 0) {
        counter.set(maxChar, counter.get(maxChar) - 1);
        res[i] = maxChar;
        i += 2;
    }
    // 剩下的字符放在其他位置
    for (let [char, count] of counter) {
        while (count--) {
            if (i >= len) {
                i = 1;
            }
            res[i] = char;
            i += 2;
        }
    }
    return res.join('');
};

S = 'aab';
S = "aaab";
S = "baaba";
S = "eqmeyggvp";
s = reorganizeString(S);
debugger;