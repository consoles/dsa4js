// 打印M行N列矩阵的转置

const matrix = [
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12]
];

const { print, println } = require('../util');

const invert = matrix => {
    const M = matrix.length;
    const N = matrix[0].length;

    const invertMatrix = new Array(N);

    for (let i = 0; i < N; i++) {
        invertMatrix[i] = new Array(M);
        for (let j = 0; j < M; j++) {
            invertMatrix[i][j] = matrix[j][i];
            print(matrix[j][i] + '\t');
        }
        println();
    }
    return invertMatrix;
}

const ret = invert(matrix);
console.log(ret);