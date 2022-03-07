/**
 * @param {number[][]} matrix
 * @return {number[]}
 */
var luckyNumbers = function (matrix) {
    // 字面意思求解
    // 取每一行的最小值,然后判断该值是否是这一列的最大值
    const res = [];
    const m = matrix.length;
    if (m < 1) return res;
    const n = matrix[0].length;
    for (let i = 0; i < m; i++) {
        let min = Number.MAX_SAFE_INTEGER;
        let minColIndex = -1;
        for (let j = 0; j < n; j++) {
            const num = matrix[i][j];
            if (num < min) {
                min = num;
                minColIndex = j;
            }
        }
        let flag = true;
        for (let j = 0; j < m; j++) {
            if (matrix[j][minColIndex] > min) {
                flag = false;
            }
        }
        if (flag) {
            res.push(min);
        }
    }
    return res;
};

let res = luckyNumbers([[3, 7, 8], [9, 11, 13], [15, 16, 17]]);
res = luckyNumbers([[1, 10, 4, 2], [9, 3, 8, 7], [15, 16, 17, 12]]);
res = luckyNumbers([[7, 8], [1, 2]]);
debugger