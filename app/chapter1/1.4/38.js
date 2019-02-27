// 3 - sum 的初级算法的实现。通过实验评估以下 ThreeSum 内循环的实现性能。

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

const threeSumFaster = nums => {
    const binarySearch = require('../../binarySearch').binarySearch;
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
const MAX = 1e8;

const test = n => {
    const arr = [];
    for (let i = 0; i < n; i++) {
        arr.push(_.random(-MAX, MAX));
    }
    const start = Date.now();
    threeSumSlow(arr);
    const time1 = Date.now();
    threeSumFaster(arr);
    const time2 = Date.now();
    const cost1 = time1 - start;
    const cost2 = time2 - time1;
    return {
        n,
        '初级3-sum': cost1,
        '二分查找-sum': cost2,
        'rate': cost1 / cost2
    }
}

let n = 50;
while (true) {
    const ret = test(n);
    console.log(JSON.stringify(ret));
    n += n;
}

// { "n": 50, "初级3-sum": 3, "二分查找-sum": 2, "rate": 1.5 }  = 
// { "n": 100, "初级3-sum": 3, "二分查找-sum": 2, "rate": 1.5 }
// { "n": 200, "初级3-sum": 10, "二分查找-sum": 7, "rate": 1.4285714285714286 }
// { "n": 400, "初级3-sum": 84, "二分查找-sum": 8, "rate": 10.5 }
// { "n": 800, "初级3-sum": 635, "二分查找-sum": 29, "rate": 21.896551724137932 }
// { "n": 1600, "初级3-sum": 4946, "二分查找-sum": 118, "rate": 41.91525423728814 }
// { "n": 3200, "初级3-sum": 39682, "二分查找-sum": 511, "rate": 77.65557729941291 }