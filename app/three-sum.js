const count = nums => {
    const n = nums.length;
    let cnt = 0;
    for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
            for (let k = j + 1; k < n; k++) {
                if (nums[i] + nums[j] + nums[k] === 0) {
                    cnt++;
                }
            }
        }
    }
    return cnt;
};

// 数组中元素各不相同的快速解法，参见two-sum，运用了二分查找
// 复杂度N^2lgN
const count2 = nums => {
    const binarySearch = require('./binarySearch').binarySearch;
    const n = nums.length;
    nums.sort((a, b) => a - b);
    let cnt = 0;
    for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
            const sum = nums[i] + nums[j];
            const index = binarySearch(nums, -sum);
            if (index > j) {
                cnt++;
            }
        }
    }
    return cnt;
}

const _ = require('lodash');

const test = n => {
    const MAX = 1e6;
    const arr = [];
    while (n--) {
        arr.push(_.random(-MAX, MAX));
    }
    const start = Date.now();
    const cnt = count(arr);
    return {
        cnt,
        elapsedTime: Date.now() - start
    };
}

let n = 250;
let prev = test(n);
while (true) {
    n += n;
    const cur = test(n);
    const { cnt, elapsedTime } = cur;
    console.log(n, cnt, elapsedTime, elapsedTime / prev.elapsedTime);
    prev = cur;
}

// 500 5 33 3.3
// 1000 63 225 6.818181818181818
// 2000 490 1709 7.595555555555555
// 4000 3940 13493 7.895260386190754
// 8000 31565 107293 7.951752760690728

// T(N) = aN^3
// T(4000) = a4000^3 => a = 2.02765625e-7
// T(8000) = 2.02765625e-7 * 8000^3 = 103816

// count1: 1921.406ms
// 500000
// count2: 92.305ms
// 500000
// const { randomArray } = require('./util');

// const arr = _.uniq(randomArray(-1000, 1000, 100000));

// console.time('count1');
// let ret = count(arr);
// console.timeEnd('count1');
// console.log(ret);

// console.time('count2');
// ret = count2(arr);
// console.timeEnd('count2');
// console.log(ret);