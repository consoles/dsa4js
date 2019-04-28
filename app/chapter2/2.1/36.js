// 不均匀的数据
// 编写一个测试用例，生成不均匀的测试数据，包括：
// 一半是0，一半是1
// 一半是0,1/4是1，1/4是2
// 一半是0，一半是随机int值

const _ = require('lodash');

const n = 1e5;
let arr1 = [];
let arr2 = [];
let arr3 = [];
for (let i = 0; i < n; i++) {
    if (i <= n / 2) {
        arr1.push(0);
    } else {
        arr1.push(1);
    }
    if (i <= n / 2) {
        arr2.push(0);
    } else if (i <= 0.75 * n) {
        arr2.push(1);
    } else {
        arr2.push(2);
    }
    if (i <= n / 2) {
        arr3.push(0);
    } else {
        arr3.push(parseInt(Math.random() * 1e7));
    }
}

arr1 = _.shuffle(arr1);
arr2 = _.shuffle(arr2);
arr3 = _.shuffle(arr3);

const { insertSort } = require('../../sort');

console.time('一半是0，一半是1');
insertSort(arr1);
console.timeEnd('一半是0，一半是1');

console.time('一半是0,1/4是1，1/4是2');
insertSort(arr2);
console.timeEnd('一半是0,1/4是1，1/4是2');

console.time('一半是0，一半是随机int值');
insertSort(arr3);
console.timeEnd('一半是0，一半是随机int值');

// 一半是0，一半是1: 3824.825ms
// 一半是0, 1 / 4是1，1 / 4是2: 3251.932ms
// 一半是0，一半是随机int值: 4140.001ms