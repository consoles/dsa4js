/**
 * @param {number} m
 * @param {number} n
 * @return {number}
 */
var uniquePaths = function (m, n) {
    // const paths = [];
    // const dirs = [[0, 1], [1, 0]];
    // function dfs(x, y, res) {
    //     if (x >= m || y >= n) return;
    //     if (x === m - 1 && y === n - 1) {
    //         paths.push(res.slice());
    //         return;
    //     }
    //     for (const [xOffset, yOffset] of dirs) {
    //         const newX = x + xOffset;
    //         const newY = y + yOffset;
    //         if (newX < m && newY < n) {
    //             res.push([newX, newY]);
    //             dfs(newX, newY, res);
    //             res.pop();
    //         }
    //     }
    // }
    // dfs(0, 0, [[0, 0]]);
    // return paths.length;

    // 上面的代码可以计算出所有的路径，但是对于23*12的矩阵占用了过多内存无法得出所有路径
    // const dirs = [[0, 1], [1, 0]];
    // let count = 0;
    // function dfs(x, y) {
    //     if (x >= m || y >= n) return;
    //     if (x === m - 1 && y === n - 1) {
    //         count++;
    //     }
    //     for (const [xOffset, yOffset] of dirs) {
    //         const newX = x + xOffset;
    //         const newY = y + yOffset;
    //         if (newX < m && newY < n) {
    //             dfs(newX, newY);
    //         }
    //     }
    // }
    // dfs(0, 0);
    // return count;

    // 改为上面的代码之后又报错：超时！！！
    // 参考评论区的解法，用DP来解
    // const f = new Array(m).fill(0).map(() => new Array(n).fill(0));
    // // 到第一行或者第一列的方式只有1个。
    // for (let i = 0; i < m; i++) {
    //     f[i][0] = 1;
    // }
    // for (let j = 0; j < n; j++) {
    //     f[0][j] = 1;
    // }
    // for (let i = 1; i < m; i++) {
    //     for (let j = 1; j < n; j++) {
    //         f[i][j] = f[i - 1][j] + f[i][j - 1];
    //     }
    // }
    // return f[m - 1][n - 1];

    // 组合数学:需要往下走n-1步，往右走m-1步，总共需要走n+m-2步。他无论往右走还是往下走他的总的步数是不会变的。也就相当于总共要走n+m-2步，往右走m-1步总共有多少种走法
    const N = m + n - 2;
    let res = 1;
    for (let i = 1; i < m; i++) {
        res = res * (N - (m - 1) + i) / i;
    }
    return res;
};

m = 3, n = 2;
m = 7, n = 3;
m = 23, n = 12;
res = uniquePaths(m, n);
debugger