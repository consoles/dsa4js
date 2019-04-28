// 2.1.19希尔排序的最坏情况。用 1 到 100 构造一个含有 100 个元素的数组并用希尔排序和递增序列 1 4 13 40 对其排序，使比较次数尽可能多。

const assert = require('assert')

const _ = require('lodash')

const swap = require('../../swap')
const { isSorted } = require('../../util')

const isVisible = (i, j) => {
    let k = 0;
    while (k <= 100) {
        if (j - i >= k * 40 && j - i <= k * 41) {
            return true;
        }
        k++;
    }
    return false;
};

// 这个只是稳定得到比较差的序列的一个方法，不一定是最差的序列
const getShellSortWorstCase = n => {
    let l = 0;
    const arr = new Array(n + 1);
    let p = 40;
    let pAddition = p;
    for (let i = 0; l < 100; i++) {
        for (let j = 1; j <= n; j++) {
            if (!Number.isInteger(arr[j]) && isVisible(j, p)) {
                l++;
                arr[j] = l;
            }
        }
        p += pAddition;
    }
    return arr.slice(1);
};

const shellSort = arr => {

    let count = 0;

    const seq = [1, 4, 13, 40];
    const n = arr.length;
    for (let i = seq.length - 1; i >= 0; i--) {
        const h = seq[i];
        for (let j = h; j < n; j++) {
            count++;
            for (let k = j; k >= h && arr[k] < arr[k - h]; k -= h) {
                swap(arr, k, k - h);
                count++;
            }
        }
    }

    assert(isSorted(arr));

    return count;
};

const arr1 = getShellSortWorstCase(100);
const arr2 = _.shuffle(_.range(1, 101));
console.log('最坏', shellSort(arr1));
console.log(JSON.stringify(arr2));
console.log('随机', shellSort(arr2));
