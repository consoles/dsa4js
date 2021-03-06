// 使用 quick - union 算法（请见 1.5.2.3 节代码框）完成练习 1.5.1。另外，在处理完输入的每对整数之后画出 id[] 数组表示的森林。

const UF = require('./uf2');
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

// [0, 1, 2, 3, 4, 5, 6, 7, 8, 0] 2
// [0, 1, 2, 4, 4, 5, 6, 7, 8, 0] 2
// [0, 1, 2, 4, 4, 8, 6, 7, 8, 0] 2
// [0, 1, 2, 4, 4, 8, 6, 2, 8, 0] 2
// [0, 1, 1, 4, 4, 8, 6, 2, 8, 0] 2
// [0, 1, 1, 4, 4, 8, 6, 2, 1, 0] 5
// [4, 1, 1, 4, 4, 8, 6, 2, 1, 0] 3
// [4, 1, 1, 4, 1, 8, 6, 2, 1, 0] 3

