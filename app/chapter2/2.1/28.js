// 2.1.28
// 相等的主键。
// 对于主键仅可能取两种值的数组，
// 评估和验证插入排序和选择排序的性能，
// 假设两种主键值出现的概率相同。

const { selectionSort, insertSort } = require('../../sort');

const n = 1e5;
const arr = [];

for (let i = 0; i < n; i++) {
    arr.push(Math.random() > .5 ? 1 : 0);
}

const arr1 = arr.slice();
const arr2 = arr.slice();

let start = Date.now();
selectionSort(arr1);
let end = Date.now();
console.log('选择', end - start);
start = end;
insertSort(arr2);
console.log('插入', Date.now() - start);

// 选择 6836
// 插入 3123