// 运行时间。使用 DoublingRatio 估计在你的计算机上用 TwoSumFast、TwoSum、ThreeSumFast以及 ThreeSum 处理一个含有 100 万个整数的文件所需的时间。

const twosum = arr => {
    let cnt = 0;
    for (let i = 0; i < arr.length; i++) {
        for (let j = i + 1; j < arr.length; j++) {
            if (arr[i] + arr[j] === 0) {
                cnt++;
            }
        }
    }
    return cnt;
};

const twosumfast = arr => {
    const binarySearch = require('../../binarySearch').binarySearch;
    arr.sort((a, b) => a - b); // NlgN
    let cnt = 0;
    // NlgN
    for (let i = 0; i < arr.length; i++) {
        let index = binarySearch(arr, -arr[i]); // lgN
        if (index > i) {
            cnt++;
        }
    }
    return cnt;
};

const threeSum = nums => {
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

const threeSumFast = nums => {
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

const arr = [];
for (let i = 0; i < 10e4; i++) {
    arr.push(_.random(-100000, 100000));
}

const test = fn => {
    let arr1K = arr.slice(0, 1e3);
    let arr2K = arr.slice(0, 2e3);
    let arr4K = arr.slice(0, 4e3);
    let arr8K = arr.slice(0, 8e3);

    let lastCost = 0;
    let rate = 0;
    console.log(fn.name);
    console.log(`数据量\t耗时\t比值`);
    for (const arr of [arr1K, arr2K, arr4K, arr8K]) {
        const start = Date.now();
        fn.call(this, arr);
        const end = Date.now();
        const cost = end - start;
        rate = cost / lastCost;
        console.log(`${arr.length}\t${cost}\t${rate}`);
        lastCost = cost;
    }
    const arr10W = arr.slice();
    const start = Date.now();
    fn.call(this, arr10W);
    const tookTime = Date.now() - start;
    console.log('10W数据量估计值', 10e4 / 8e3 * rate, '实际值', tookTime);
}

for (const fn of [twosum, twosumfast, threeSum, threeSumFast]) {
    test(fn);
    console.log();
}