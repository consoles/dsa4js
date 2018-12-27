// 糟糕的打乱。
// 假设在我们的乱序代码中你选择的是一个 0 到 N - 1 而非 i 到 N - 1 之间的随机整数。
// 证明得到的结果并非均匀地分布在 N! 种可能性之间。
// 用上一题中的测试检验这个版本。

const _ = require('lodash');
const { initMatrix, printMatrix } = require('../../util');

function worseShuffle(arr) {
    const n = arr.length;
    for (let i = 0; i < n; i++) {
        let r = _.random(0, n - 1);
        [arr[i], arr[r]] = [arr[r], arr[i]];
    }
    return arr;
}

function shuffleTest(m, n) {

    const record = initMatrix(m, m);
    const arr = new Array(m);

    for (let i = 0; i < n; i++) {
        // init
        for (let i = 0; i < m; i++) {
            arr[i] = i;
        }
        worseShuffle(arr);
        for (let i = 0; i < m; i++) {
            record[i][arr[i]]++; // 打乱后位于该位置的次数(参照索引二叉堆，这个中间多了一个弯)
        }
    }
    printMatrix(record);
}

shuffleTest(8, 80000);
