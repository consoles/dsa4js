// 随机输入下的 3 - sum 问题。
// 猜测找出 N 个随机 int 值中和为 0 的整数三元组的数量所需的时间并验证你的猜想。
// 如果你擅长数学分析，请为此问题给出一个合适的数学模型， 其中所有值均匀的分布在 - M 和 M 之间，且 M 不能是一个小整数。

const N = 100000;
const _ = require('lodash');

const threeSumSlow = nums => {
    let cnt = 0;
    const n = nums.length;
    for (let i = 0; i < n; i++) {
        for (let j = 0; j < n; j++) {
            for (let k = 0; k < n; k++) {
                if (i < j && j < k) {
                    if (nums[i] + nums[j] + nums[k] === 0) {
                        cnt++;
                    }
                }
            }
        }
    }
    return cnt;
};

let lastCost = 0;
for (let i = 100; true; i += i) {
    const arr = [];
    for (let j = 0; j < i; j++) {
        arr.push(_.random(-N, N));
    }
    const start = Date.now();
    threeSumSlow(arr);
    const cost = Date.now() - start;
    const rate = cost / lastCost;
    lastCost = cost;
    console.log(`i = ${i},cost = ${cost},rate = ${rate}`);
}

// i = 100, cost = 5, rate = Infinity
// i = 200, cost = 11, rate = 2.2
// i = 400, cost = 80, rate = 7.2727272727272725
// i = 800, cost = 626, rate = 7.825
// i = 1600, cost = 4973, rate = 7.944089456869009
// i = 3200, cost = 39576, rate = 7.958174140357933

// 设t=a * N^3 / 6，则常数a = 0.00000724658203125