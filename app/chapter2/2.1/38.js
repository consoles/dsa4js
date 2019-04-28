// 不同类型的元素

// 编写一个测试用例，编写一个测试用例，生成多种数据类型元素组成的数组，数组的主键值随机包括：
// 每个元素的主键均为string类型（至少10个字符），并且含有一个double值
// 每个元素的主键均为double类型，并至少含有10个string值（至少10个字符）
// 每个元素的主键均为int类型，并含有一个int[20]的值

const _ = require('lodash');

const n = 1e4;
let arr1 = [];
let arr2 = [];
let arr3 = [];

for (let i = 0; i < n; i++) {
    arr1.push(Math.random().toString(16));
    arr2.push(Math.random());
    arr3.push(i);
}

arr1 = _.shuffle(arr1);
arr2 = _.shuffle(arr2);
arr3 = _.shuffle(arr3);

const { insertSort } = require('../../sort');

console.time('主键值为string');
insertSort(arr1);
console.timeEnd('主键值为string');

console.time('主键值为double');
insertSort(arr2);
console.timeEnd('主键值为double');

console.time('主键值为int');
insertSort(arr3);
console.timeEnd('主键值为int');

// 主键值为string: 293.549ms
// 主键值为double: 183.963ms
// 主键值为int: 147.954ms