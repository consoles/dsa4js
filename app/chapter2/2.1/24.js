// 2.1.24 插入排序的哨兵。在插入排序的实现中先找出最小的元素并将其置于数组的最左边，这样就能去掉内循环的判断条件 j>0。使用 SortCompare 来评估这种做法的效果。这是一种常见的规避边界测试的方法，能够省略判断条件的元素通常称为哨兵。

const swap = require('../../swap');

const { randomDoubleArray } = require('../../util');

const insertSort = arr => {

    const n = arr.length;

    // 找到数组最小元素放在最左边
    let minIndex = 0;
    for (let i = 1; i < n; i++) {
        if (arr[i] < arr[minIndex]) {
            minIndex = i;
        }
    }
    if (minIndex !== 0) {
        swap(arr, 0, minIndex);
    }

    for (let i = 1; i < n; i++) {
        for (let j = i; arr[j] < arr[j - 1]; j--) {
            swap(arr, j, j - 1);
        }
    }
};

const insertSort2 = require('../../sort').insertSort;
const arr = randomDoubleArray(100000);

let start = Date.now();
insertSort(arr);
console.log('插入排序(哨兵)', Date.now() - start);

start = Date.now();
insertSort2(arr);
console.log('插入排序', Date.now() - start);

// 插入排序(哨兵) 8185
// 插入排序 3