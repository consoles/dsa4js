/**
 * @param {number[][]} A
 * @return {number}
 */
var matrixScore = function (A) {
    const rowCount = A.length;
    if (rowCount <= 0) return 0;
    const colCount = A[0].length;
    if (colCount <= 0) return 0;
    // 贪心的思想：
    // step1：扫描每一行，如果0排列在1的前面，则该行需要变换
    // for (let i = 0; i < rowCount; i++) {
    //     if (A[i][0] === 0) {
    //         for (let j = 0; j < colCount; j++) {
    //             A[i][j] = A[i][j] === 0 ? 1 : 0;
    //         }
    //     }
    // }
    // // step 2，扫描每一列，如果该列中0的个数大于1的个数，则进行交换
    // for (let j = 0; j < colCount; j++) {
    //     // 统计每一列中0 & 1的个数
    //     let zeroCount = 0;
    //     let oneCount = 0;
    //     for (let i = 0; i < rowCount; i++) {
    //         if (A[i][j] === 0) {
    //             zeroCount++;
    //         } else {
    //             oneCount++;
    //         }
    //     }
    //     if (zeroCount > oneCount) {
    //         // 交换该列
    //         for (let i = 0; i < rowCount; i++) {
    //             A[i][j] = A[i][j] === 0 ? 1 : 0;
    //         }
    //     }
    // }
    // // 计算二进制的和
    // let totalSum = 0;
    // for (let i = 0; i < rowCount; i++) {
    //     let sum = 0;
    //     for (let j = 0; j < colCount; j++) {
    //         sum += A[i][j] * 2 ** (colCount - j - 1);
    //     }
    //     totalSum += sum;
    // }
    // return totalSum;

    // 还是上面的思路，不实际进行元素的翻转
    let sum = rowCount * 2 ** (colCount - 1); // 最优的情况下，最左边的列肯定是1
    // 考虑其他列
    for (let j = 1; j < colCount; j++) {
        let nOnes = 0;
        for (let i = 0; i < rowCount; i++) {
            if (A[i][0] === 1) {
                nOnes += A[i][j];
            } else {
                nOnes += (1 - A[i][j]); // 如果这一行进行了行反转，则该元素的实际取值为1-A[i][j]
            }
        }
        const k = Math.max(nOnes, rowCount - nOnes);
        sum += k * (2 ** (colCount - j - 1));
    }
    return sum;
};

const A = [[0, 0, 1, 1], [1, 0, 1, 0], [1, 1, 0, 0]];
const res = matrixScore(A);
debugger;