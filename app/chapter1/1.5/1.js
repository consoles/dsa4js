// 使用 quick-find 算法处理序列 9-0 3-4 5-8 7-2 2-1 5-7 0-3 4-2 。
// 对于输入的每一对整数，给出 id[] 数组的内容和访问数组的次数。

const UF = require('./uf1');
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

// [9, 1, 2, 3, 4, 5, 6, 7, 8, 9] 13
// [9, 1, 2, 3, 3, 5, 6, 7, 8, 9] 13
// [9, 1, 2, 3, 3, 5, 6, 7, 5, 9] 13
// [9, 1, 7, 3, 3, 5, 6, 7, 5, 9] 13
// [9, 7, 7, 3, 3, 5, 6, 7, 5, 9] 13
// [9, 5, 5, 3, 3, 5, 6, 5, 5, 9] 15
// [9, 5, 5, 9, 9, 5, 6, 5, 5, 9] 14
// [9, 9, 9, 9, 9, 9, 6, 9, 9, 9] 17