/**
 * @param {string} digits
 * @return {string[]}
 */
var letterCombinations = function (digits) {
    const res = []
    const n = digits.length
    if (!digits || n === 0) return res

    const digit2alpha = {
        '2': 'abc',
        '3': 'def',
        '4': 'ghi',
        '5': 'jkl',
        '6': 'mno',
        '7': 'pqrs',
        '8': 'tuv',
        '9': 'wxyz'
    }

    function dfs(index, path) {
        if (index === n) {
            res.push(path)
            return
        }
        const alpha = digit2alpha[digits[index]]
        for (const c of alpha) {
            dfs(index + 1, path + c)
        }
    }

    dfs(0, '')
    return res
};

var letterCombinations2 = function (digits) {
    const res = []
    const n = digits.length
    if (!digits || n === 0) return res

    const digit2alpha = {
        '2': 'abc',
        '3': 'def',
        '4': 'ghi',
        '5': 'jkl',
        '6': 'mno',
        '7': 'pqrs',
        '8': 'tuv',
        '9': 'wxyz'
    }

    function dfs(index, path) {
        if (index === n) {
            res.push(path.join(''))
            return
        }
        const alpha = digit2alpha[digits[index]]
        for (const c of alpha) {
            // 通用的回溯模板，方法1利用了字符串的不变性
            path.push(c)
            dfs(index + 1, path + c)
            path.pop()
        }
    }

    dfs(0, [])
    return res
};
