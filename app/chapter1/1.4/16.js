// 最接近一对（一维）。
// 编写一个程序，给定一个含有 N 个 double 值的数组 a[]，
// 在其中找到一对最接近的值：两者之差（绝对值）最小的两个数。
// 程序在最坏情况下所需的运行时间应该是线性对数级别的。

const minDistance = doubles => {
    doubles.sort((a, b) => a - b);
    let minDiff = Number.MAX_SAFE_INTEGER;
    let pair = null;
    for (let i = 0; i < doubles.length - 1; i++) {
        const diff = doubles[i + 1] - doubles[i];
        if (diff < minDiff) {
            minDiff = diff;
            pair = [doubles[i], doubles[i + 1]];
        }
    }
    return pair;
};

const doubles = [-4, -2, 1, 0, 2, 33, 55, 66, 68];
const pair = minDistance(doubles);
debugger