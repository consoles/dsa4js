// 模拟掷骰子。以下代码能够计算每种两个骰子之和的准确概率分布：
// dist[i] 的值就是两个骰子之和为 i 的概率。

const _ = require('lodash');

const SIDES = 6;
const dist = new Array(2 * SIDES + 1).fill(0);

for (let i = 1; i <= SIDES; i++) {
    for (let j = 1; j <= SIDES; j++) {
        dist[i + j] += 1;
    }
}
for (let i = 2; i <= 2 * SIDES; i++) {
    dist[i] /= 36;
}

// const sum = _.sum(dist);
// debugger;

// 用实验模拟 N 次掷骰子，
// 并在计算两个 1 到 6 之间的随机整数之和时记录每个值出现频率以验证它们的概率。
// N 要多大才能够保证你的经验数据和准确数据的吻合程度达到小数点后 3 位？

// 进行n次同时投掷2个骰子的实验，求两个骰子点数和的频率
function rollDice(n) {
    const dist = new Array(2 * SIDES + 1).fill(0);
    for (let i = 0; i < n; i++) {
        let sum = _.random(1, 6) + _.random(1, 6);
        dist[sum]++;
    }
    return dist.map(x => x / n);
}

/**
 * 检查频率和概率的误差是否在precision内
 */
function isAccept(freq, precision) {
    for (let i = 0; i < freq.length; i++) {
        if (Math.abs(freq[i] - dist[i]) > precision) {
            return false;
        }
    }
    return true;
}

function mock() {

    const precision = 0.001;
    let n = 10;

    console.time('mock');
    while (true) {
        let freq = rollDice(n);;
        if (isAccept(freq, precision)) {
            break;
        }
        if (n % 1e4 === 0) console.log(n);
        n++;
    }
    console.timeEnd('mock');
    console.log('mock finish , n = ', n);
}

mock();

// mock: 23051.146ms
// mock finish, n = 27842