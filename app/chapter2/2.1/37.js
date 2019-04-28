// 部分有序
// 编写一个测试用例，生成部分有序的数组，包括：
// 95% 有序，其他部分为随机值
// 所有元素和他们正确位置的距离都不超过10
// 5%的元素随机分布在整个数组中，剩下的数据都是有序的

const _ = require('lodash');

const n = 1e5;
let arr1 = [];
let arr2 = [];
let arr3 = [];
for (let i = 0; i < n; i++) {
    arr1.push(i);
    arr2.push(i);
    arr3.push(i);
}

const index = parseInt(n * 0.95);
const subArr1 = arr1.slice(0, index);
const randomArr = _.shuffle(arr1.slice(index));
arr1 = subArr1.concat(randomArr);

const used = new Set();
for (let i = 0; i < n; i++) {
    const index = Math.floor(Math.random()) * 10 + 1;
    if (index < n && !used.has(i) && !used.has(index)) {
        used.add(i);
        used.add(index);
        [arr2[i], arr2[index]] = [arr2[index], arr2[i]];
    }
}
used.clear();

const swapCount = Math.floor(n * 0.05);
for (let i = 0; i < swapCount;) {
    const index1 = Math.floor(Math.random() * n);
    const index2 = Math.floor(Math.random() * n);
    if (index1 !== index2 && !used.has(index1) && !used.has(index2)) {
        used.add(index1);
        used.add(index2);
        [arr3[i], arr3[index]] = [arr3[index], arr3[i]];
        i++;
    }
}

const { insertSort } = require('../../sort');

console.time('95% 有序，其他部分为随机值');
insertSort(arr1);
console.timeEnd('95% 有序，其他部分为随机值');

console.time('所有元素和他们正确位置的距离都不超过10');
insertSort(arr2);
console.timeEnd('所有元素和他们正确位置的距离都不超过10');

console.time('5%的元素随机分布在整个数组中，剩下的数据都是有序的');
insertSort(arr3);
console.timeEnd('5%的元素随机分布在整个数组中，剩下的数据都是有序的');

// 95% 有序，其他部分为随机值: 21.968ms
// 所有元素和他们正确位置的距离都不超过10: 2.869ms
// 5%的元素随机分布在整个数组中，剩下的数据都是有序的: 1.634ms