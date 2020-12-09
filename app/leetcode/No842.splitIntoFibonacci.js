/**
 * @param {string} S
 * @return {number[]}
 */
var splitIntoFibonacci = function (S) {
    // 标准回溯模板
    const len = S.length;
    const maxInt = 2 ** 31 - 1;
    function backtracking(start, res) {
        if (start === len && res.length > 2) {
            return true;
        }
        for (let i = start; i < len; i++) {
            const segment = S.substring(start, i + 1);
            // 剪枝1：数值超过整数最大值
            const val = parseInt(segment);
            if (val > maxInt) {
                break;
            }
            // 防止开头为0
            if (segment.startsWith('0') && segment !== '0') {
                break;
            }
            if (!isFibonacciSequence(res, val)) {
                continue;
            }
            res.push(val);
            // 尝试下一个结果
            const flag = backtracking(i + 1, res);
            if (flag) {
                return true;
            }
            res.pop();
        }
    }

    /**
     * 即将要添加进数组的数是否能和之前的序列组成斐波那契数列
     */
    function isFibonacciSequence(res, val) {
        const len = res.length;
        if (len < 2) return true;
        return res[len - 2] + res[len - 1] === val;
    }

    const res = [];
    backtracking(0, res);
    return res;
};

// S = "123456579"
// S = "11235813"
// S = "112358130"
// S = "0123"
S = "1101111"
res = splitIntoFibonacci(S);
debugger