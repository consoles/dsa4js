// 乱序检查。
// 通过实验检查表 1.1.10 中的乱序代码是否能够产生预期的效果。
// 编写一个程序 ShuttleTest，
// 接受命令行参数 M 和 N， 将大小为 M 的数组打乱 N 次且在每次打乱之前都将数组重新初始化为 a[i] = i。
// 打印一个 M×M 的表格，对于所有的列 j，行 i 表示的是 i 在打乱后落到 j 的位置的次数。
// 数组中的所有元素的值都应该接近于 N / M。

const _ = require('lodash');
const { initMatrix, printMatrix } = require('../../util');

function shuffle(arr) {
    const n = arr.length;
    for (let i = 0; i < n; i++) {
        // 将arr[i]和arr[i...n-1]中的任意一个元素交换
        let r = i + _.random(0, n - i - 1);
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
        shuffle(arr);
        for (let i = 0; i < m; i++) {
            record[i][arr[i]]++; // 打乱后位于该位置的次数(参照索引二叉堆，这个中间多了一个弯)
        }
    }
    printMatrix(record);
}

shuffleTest(8, 80000);

// 10016  	9946   	9984   	9937   	9912   	9940   	10040  	10225
// 10017  	9880   	9891   	9975   	10055  	9989   	9964   	10229
// 10052  	10199  	9894   	9981   	9981   	10029  	9974   	9890
// 10058  	9988   	10073  	10003  	10050  	10065  	9957   	9806
// 9980   	9985   	10206  	9892   	10051  	9914   	10101  	9871
// 9930   	10109  	9971   	10092  	10118  	9920   	9813   	10047
// 10083  	9791   	9891   	10083  	9965   	10064  	10137  	9986
// 9864   	10102  	10090  	10037  	9868   	10079  	10014  	9946