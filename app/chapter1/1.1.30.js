// 1.1.30 数组练习。编写一段程序，创建一个 N×N 的布尔数组 a[][]。其中当 i 和 j 互质时(没有相同 因子)，a[i][j] 为 true，否则为 false。

/**
 * 判断a,b是否互质
 * 
 * 辗转相除 求最大公约数
 */
const isMutual = (a, b) => {
    const gcd = (p, q) => {
        while (q !== 0) {
            let r = p % q;
            p = q;
            q = r;
        }
        return p;
    }

    return gcd(a, b) === 1;
};

const buildPrimMatrix = n => {

    const matrix = new Array(n);
    for (let i = 0; i < n; i++) {
        matrix[i] = new Array(n);
        for (let j = 0; j < n; j++) {
            matrix[i][j] = (i == 0 || j == 0) ? false : isMutual(i, j);
        }
    }

    return matrix;
};

const ret = buildPrimMatrix(9);
debugger