// 二项分布
let count = 0;
const binomial = (N, k, p) => {
    count++;
    if (count % 10000000 === 0) console.log(count);
    if (N === 0 && k === 0) return 1;
    if (N < 0 || k < 0) return 0;
    return (1 - p) * binomial(N - 1, k, p) + p * binomial(N - 1, k - 1, p);
};

// 5分钟算不出来，舍弃
// console.time('二项分布-递归');
// const ret = binomial(30, 15, .25);
// console.timeEnd('二项分布-递归');
// console.log(ret, count);

const cache = {};
const binomial2 = (N, k, p) => {
    let key = `${N}-${k}`;
    count++;
    if (cache[key] !== void 0) return cache[key];
    if (N === 0 && k === 0) return cache[key] = 1;
    if (N < 0 || k < 0) return cache[key] = 0;
    return cache[key] = (1 - p) * binomial2(N - 1, k, p) + p * binomial2(N - 1, k - 1, p);
};

console.time('二项分布-递归-缓存');
const ret = binomial2(100, 50, .25);
console.timeEnd('二项分布-递归-缓存');
console.log(ret, count);

// https://zhuanlan.zhihu.com/p/43450263
// 伯努利试验（独立重复试验），又称二项分布