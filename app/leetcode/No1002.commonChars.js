/**
 * @param {string[]} A
 * @return {string[]}
 */
var commonChars = function (A) {
    // 方法1：针对数组中每个单词中的字母进行计数
    const len = A.length
    const counters = new Array(len)

    const minCounter = new Map()

    const chars = new Set()

    for (let i = 0; i < len; i++) {
        const counter = new Map()
        const str = A[i]
        for (const s of str) {
            let count = 0
            if (!counter.has(s)) {
                count = 1
                counter.set(s, 1)
            } else {
                count = counter.get(s) + 1
            }
            counter.set(s, count)
            chars.add(s)
        }
        counters[i] = counter
    }
    const res = []
    for (const c of chars) {
        let min = Number.MAX_VALUE
        for (const counter of counters) {
            if (!counter.has(c)) {
                min = 0
                break
            } else {
                min = Math.min(min, counter.get(c))
            }
        }
        while (min--) {
            res.push(c)
        }
    }
    return res
};

console.log(commonChars(["bella", "label", "roller"]));
console.log(commonChars(["cool", "lock", "cook"]));
