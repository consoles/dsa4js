// 矩阵的局部最小元素。
// 给定一个含有 N ^ 2 个不同整数的 N×N 数组 a[]。
// 设计一个运送时间和 N 成正比的算法来找出一个局部最小元素：
// 满足 a[i][j] < a[i + 1][j]、a[i][j] < a[i][j + 1]、a[i][j] < a[i - 1][j] 以及 a[i][j] < a[i][j - 1] 的索引 i 和 j。
// 程序运行时间在最坏情况下应该和 N 成正比。

// O(N^2)
const minMatrix = matrix => {
    const len = matrix.length;
    for (let i = 1; i < len; i++) {
        for (let j = 1; j < len; i++) {
            const item = matrix[i][j];
            if (item < matrix[i + 1][j] && item < matrix[i][j + 1] && item < matrix[i - 1][j] && item < matrix[i][j - 1]) {
                return [i, j];
            }
        }
    }
    return null;
};

const findMinCol = (matrix, midRow, colStart, colEnd) => {
    let min = Number.MAX_SAFE_INTEGER;
    let colPos = -1;
    for (let i = colStart; i < colEnd; i++) {
        if (matrix[midRow][i] < min) {
            min = matrix[midRow][i];
            colPos = i;
        }
    }
    return colPos;
};

const findLocalmin = (matrix, rowStart, rowEnd, colStart, colEnd) => {

    const midRow = rowStart + Math.floor((rowEnd - rowStart) / 2);
    const minColPos = findMinCol(matrix, midRow, colStart, colEnd);

    const midRowMin = matrix[midRow][minColPos]; // 中间行中的最小值

    // 只需要和 上一行 & 下一行比较就行
    if (midRowMin < matrix[midRow + 1][minColPos] && midRowMin < matrix[midRow - 1][minColPos]) {
        return [midRow, minColPos];
    }

    // 中间一行的最小值大于下一行，则说明下一行中间值比较小，在矩阵的下半部分继续寻找
    if (midRowMin > matrix[midRow + 1][minColPos]) {
        return findLocalmin(matrix, midRow, rowEnd, colStart, colEnd);
    }
    return findLocalmin(matrix, rowStart, midRow, colStart, colEnd);
};

const minMatrix2 = matrix => {
    const end = matrix.length - 1;
    return findLocalmin(matrix, 0, end, 0, end);
};

const minMatrix3 = matrix => {
    const len = matrix.length;
    let minRowIndex = 0, maxRowIndex = len - 1;
    while (minRowIndex <= maxRowIndex) {
        const midRowIndex = minRowIndex + Math.floor((maxRowIndex - minRowIndex) / 2);
        let min = Number.MAX_SAFE_INTEGER;
        let minColPos = -1;
        for (let i = 0; i < len; i++) {
            if (matrix[midRowIndex][i] < min) {
                min = matrix[midRowIndex][i];
                minColPos = i;
            }
        }
        const item = matrix[midRowIndex][minColPos];
        if (item < matrix[midRowIndex - 1][minColPos] && item < matrix[midRowIndex + 1][minColPos]) {
            return [midRowIndex, minColPos];
        }
        if (item > matrix[midRowIndex - 1][minColPos]) {
            maxRowIndex = midRowIndex - 1;
        } else {
            minRowIndex = midRowIndex + 1;
        }
    }
    return null;
};

const matrix = [
    [1, 1, 2, 3],
    [0, -2, 1, 2],
    [3, 4, 2, 5],
    [0, 9, -8, 9]
];

let ret = minMatrix3(matrix);
debugger