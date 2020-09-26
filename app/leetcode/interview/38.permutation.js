// 方法1：借助set去重

/**
 * @param {string} s
 * @return {string[]}
 */
var permutation = function(s) {
    const n = s.length
    const used = new Array(n).fill(false)
    const paths = []
    function dfs(index,str) {
        if (str.length === n) {
            paths.push(str);
            return;
        }
        for(let i = 0;i < n;i++) {
            if (!used[i]) {
                used[i] = true;
                dfs(index + 1,str + s[i])
                used[i] = false;
            }
        }
    }
    dfs(0,'')
    return [...new Set(paths)]
};

// 方法2：先排序对重复元素进行剪枝

/**
 * @param {string} s
 * @return {string[]}
 */
var permutation = function(s) {
    const n = s.length
    const used = new Array(n).fill(false)
    const paths = []
    // 默认字典序
    const sArr = s.split('').sort()
    function dfs(index,str) {
        if (str.length === n) {
            paths.push(str);
            return;
        }
        for(let i = 0;i < n;i++) {
            if (i > 0 && sArr[i] === sArr[i-1] && !used[i-1]) continue
            if (!used[i]) {
                used[i] = true;
                dfs(index + 1,str + sArr[i])
                used[i] = false;
            }
        }
    }
    dfs(0,'')
    return paths
};

// 参见leetcode47题

// s = "abc"
s = "aab"
const res = permutation(s)
console.log(res);