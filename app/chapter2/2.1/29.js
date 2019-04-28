// 2.1.29
// 希尔排序的递增序列。
// 通过实验比较算法 2.3 中所使用的递增序列和递增序列
// 1, 5, 19, 41, 109, 209, 505, 929, 2161, 3905, 8929, 16001, 36289, 64769, 146305, 260609
// （这是通过序列 9×4 ^ (k) - 9×2 ^ (k) + 1 和 4 ^ (k) - 3×2 ^ (k) + 1 综合得到的）。
// 可以参考练习 2.1.11。

// 这个序列更优，是这本书的作者提出的

const assert = require('assert');

const swap = require('../../swap');
const { randomDoubleArray, isSorted } = require('../../util');

const shellSort = arr => {

    const n = arr.length;

    let h = 1;
    const seq = [];
    while (h < n / 3) {
        seq.push(h);
        h = 3 * h + 1;
    }

    // seq 1,4,13,40

    for (let i = seq.length - 1; i >= 0; i--) {
        let h = seq[i];
        for (let j = h; j < n; j++) {
            for (let k = j; k >= h && arr[k] < arr[k - h]; k -= h) {
                swap(arr, k, k - h);
            }
        }
    }

    assert(isSorted(arr));
};

const shellSort2 = arr => {

    const n = arr.length;

    const seq = [1, 5, 19, 41, 109, 209, 505, 929, 2161, 3905, 8929, 16001, 36289, 64769, 146305, 260609];

    for (let i = seq.length - 1; i >= 0; i--) {
        let h = seq[i];
        for (let j = h; j < n; j++) {
            for (let k = j; k >= h && arr[k] < arr[k - h]; k -= h) {
                swap(arr, k, k - h);
            }
        }
    }
    assert(isSorted(arr));
};

const n = 20e4;
const arr = randomDoubleArray(n);
const arr1 = arr.slice();
const arr2 = arr.slice();

let start = Date.now();
shellSort(arr1);
let end = Date.now();
console.log('递增序列3h+1', end - start);
start = end;
shellSort2(arr2);
console.log('更优的递增序列', Date.now() - start);

// 递增序列3h + 1 49
// 更优的递增序列 43