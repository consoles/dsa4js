// 2.1.35不均匀的概率分布

// 编写一个用例，使用非均匀分布的概率来生成随机排列的数据，包括：高斯分布、泊松分布、几何分布、离散分布（参见练习2.1.28）评估并验证这些输入数据对本节讨论的算法的性能影响

const assert = require('assert');

/**
 * 生成符合正态分布的随机数
 * 
 * @param {*} average 正态分布期望值μ
 * @param {*} standardDeviation 正态分布标准差δ
 */
const normalDistribution = (average, standardDeviation) => {
    const a = Math.random();
    const b = Math.random();
    const z = Math.sqrt(-2 * Math.log(a)) * Math.cos(2 * Math.PI * b);
    return z * standardDeviation + average;
};

/**
 * 生成符合泊松分布的随机数
 * 
 * @param {*} average 泊松分布的期望值λ
 */
const poissionDistribution = average => {
    let x = 0;
    let p = Math.pow(Math.E, -average);
    let s = p;
    let u = Math.random();
    do {
        x++;
        p *= average / x;
        s += p;
    } while (u > s);
    return x;
};

/**
 * 生成符合几何分布的随机数
 * 
 * @param {*} p 集合分布的概率
 */
const geometryDistribution = p => {
    assert(p >= 0 && p <= 1, '概率区间[0,1]');
    return Math.ceil(Math.log(1 - Math.random()) / Math.log(1 - p));
};

/**
 * 根据指定的几率数组产生符合离散分布的随机数
 * 
 * @param {*} probabilities 各取值的可能性
 */
const discreteDistribution = probabilities => {
    assert(Array.isArray(probabilities), '概率数组必须提供');
    const EPSION = 1e-14;
    let sum = 0;
    for (const prob of probabilities) {
        assert(prob > 0, `${prob} must be nonnegative.`);
        sum += prob;
    }
    // 浮点数精度问题
    assert(sum >= 1 - EPSION && sum <= 1 + EPSION, 'sum of array dose not eq 1.0');

    while (true) {
        const r = Math.random();
        sum = 0;
        for (const [index, prob] of probabilities.entries()) {
            sum += prob;
            if (sum > r) {
                return index;
            }
        }
    }
};

const n = 1e5;
const arr1 = [];
const arr2 = [];
const arr3 = [];
const arr4 = [];
for (let i = 0; i < n; i++) {
    arr1.push(normalDistribution(0, 1));
    arr2.push(poissionDistribution(0.5));
    arr3.push(geometryDistribution(0.5));
    arr4.push(discreteDistribution([0.3, 0.1, 0.2, 0.2, 0.15, 0.05]));
}

const { insertSort } = require('../../sort');

console.time('正态分布');
insertSort(arr1);
console.timeEnd('正态分布');

console.time('泊松分布');
insertSort(arr2);
console.timeEnd('泊松分布');

console.time('几何分布');
insertSort(arr3);
console.timeEnd('几何分布');

console.time('离散分布');
insertSort(arr4);
console.timeEnd('离散分布');

// 正态分布: 6725.781ms
// 泊松分布: 1426.021ms
// 几何分布: 4400.103ms
// 离散分布: 5307.876ms