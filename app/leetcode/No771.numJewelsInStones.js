/**
 * @param {string} J
 * @param {string} S
 * @return {number}
 */
var numJewelsInStones = function (J, S) {
    let count = 0
    if (!J || J.length < 0) return count
    for (const s of S) {
        if (J.includes(s)) {
            count++
        }
    }
    return count
};

J = "aA", S = "aAAbbbb"
let res = numJewelsInStones(J, S)
console.log(res);
J = "z", S = "ZZ"
res = numJewelsInStones(J, S)
console.log(res);