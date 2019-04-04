// 使用加权 quick-union 算法（请见算法 1.5）完成练习 1.5.1 。

const UF = require('./uf3');
const uf = new UF(10);

const pairs = [
    [9, 0],
    [3, 4],
    [5, 8],
    [7, 2],
    [2, 1],
    [5, 7],
    [0, 3],
    [4, 2]
];

for (const [p, q] of pairs) {
    uf.union(p, q);
    console.log(uf.ids, uf.currentAccessArrayCount);
}

// [9, 1, 2, 3, 4, 5, 6, 7, 8, 9] 7
// [9, 1, 2, 3, 3, 5, 6, 7, 8, 9] 7
// [9, 1, 2, 3, 3, 5, 6, 7, 5, 9] 7
// [9, 1, 7, 3, 3, 5, 6, 7, 5, 9] 7
// [9, 7, 7, 3, 3, 5, 6, 7, 5, 9] 8
// [9, 7, 7, 3, 3, 7, 6, 7, 5, 9] 7
// [9, 7, 7, 9, 3, 7, 6, 7, 5, 9] 8
// [9, 7, 7, 9, 3, 7, 6, 7, 5, 7] 10