// 最遥远的一对（一维）。
// 编写一个程序，给定一个含有 N 个 double 值的数组 a[]，
// 在其中找到一对最遥远的值：两者之差（绝对值）最大的两个数。
// 程序在最坏情况下所需的运行时间应该是线性级别的。(注意是线性级别：找到最大值和最小值即可)

const maxDistance = doubles => {
    let min = Number.MAX_SAFE_INTEGER;
    let max = Number.MIN_SAFE_INTEGER;
    for (let num of doubles) {
        if (num > max) {
            max = num;
        }
        // 注意：这里不能用else if
        if (num < min) {
            min = num;
        }
    }
    return [min, max];
};

const doubles = [-4, -2, 1, 0, 2, 33, 55, 66, 68];
const pair = maxDistance(doubles);
debugger