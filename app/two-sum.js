// 找出数组中和为0的整数对的数量

/**
 * 平方级别解决two-sum
 */
const twosum1 = arr => {
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

/**
 * 线性对数级别(但是只能解决数组中没有重复元素的情况)，时间虽然快了，但是这个也是局限性。
 * 快速排序 + 二分查找
 */
const twosum2 = arr => {
    const binarySearch = require('./binarySearch').binarySearch;
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

// two sum1: 305.861ms
// 9876
// two sum2: 21.548ms
// 9876
const { randomArray } = require('./util');
const _ = require('lodash');

const arr = _.uniq(randomArray(-10000, 10000, 1000000));

console.time('two sum1');
let ret = twosum1(arr);
console.timeEnd('two sum1');
console.log(ret);

console.time('two sum2');
ret = twosum2(arr);
console.timeEnd('two sum2');
console.log(ret);

