/**
 * @param {string} S
 * @param {string} T
 * @return {boolean}
 */
var backspaceCompare = function (S, T) {
    // 方法1：模拟
    // 使用栈：遇到普通字符入栈，遇到退个字符出栈
    // let ss = []
    // let tt = []
    // for (const c of S) {
    //     if (c !== '#') {
    //         ss.push(c)
    //     } else {
    //         ss.pop()
    //     }
    // }
    // for (const c of T) {
    //     if (c !== '#') {
    //         tt.push(c)
    //     } else {
    //         tt.pop()
    //     }
    // }
    // // return ss.join('') === tt.join('')
    // // 优化
    // let len = ss.length
    // if (tt.length !== len) return false
    // while (len--) {
    //     if (tt[len] !== ss[len]) return false
    // }
    // return true

    // 方法2：双指针
    // 一个字符是否会被删掉，只取决于该字符后面的退格符，而与该字符前面的退格符无关。
    // 因此当我们逆序地遍历字符串，就可以立即确定当前字符是否会被删掉
    let i = S.length - 1, j = T.length - 1
    let skipS = 0, skipT = 0
    while (i >= 0 || j >= 0) {
        while (i >= 0) {
            if (S[i] === '#') {
                skipS++
                i--
            } else if (skipS > 0) {
                skipS--
                i--
            } else {
                // 循环跳出的时候，i位置的字符是需要对比的字符
                break
            }
        }
        while (j >= 0) {
            if (T[j] === '#') {
                skipT++
                j--
            } else if (skipT > 0) {
                skipT--
                j--
            } else {
                break
            }
        }
        if (i >= 0 && j >= 0) {
            if (S[i] !== T[j]) {
                return false
            }
        } else {
            if (i >= 0 || j >= 0) {
                // 如果有一个字符串指针已经=-1，另一个还指向字符，说明最终字符串不相等
                return false
            }
        }
        i--
        j--
    }
    return true
};

let S = "ab#c", T = "ad#c"
let ret = backspaceCompare(S, T)
debugger
