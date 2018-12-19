// 二分查找与暴力查找。根据1.1.10.4节给出的暴力查找法编写一个程序 BruteForceSearch，在你的计算机上比较它和 BinarySearch处理largeW.txt 和 largeT.txt所需的时间。
// 数据文件 链接: https://pan.baidu.com/s/1OtvDSHm87s7W4uQVdQuslg 提取码: yy2j 

const { readInts } = require('../util');

console.time('read numbers1000000');
const numbers1000000 = readInts('/Users/yiihua-013/Downloads/1.1.38/largeW.txt');
console.timeEnd('read numbers1000000');
console.time('read numbers10000000');
const numbers10000000 = readInts('/Users/yiihua-013/Downloads/1.1.38/largeT.txt');
console.timeEnd('read numbers10000000');

const find = (arr, key) => {
    for (let i = 0; i < arr.length; i++) {
        if (arr[i] === key) {
            return i;
        }
    }
    return -1;
}

const { binarySearch } = require('../binarySearch');

console.time('普通查找 numbers1000000');
find(numbers1000000, 122922297);
console.timeEnd('普通查找 numbers1000000');
console.time('普通查找 numbers10000000');
find(numbers10000000, 122922297);
console.timeEnd('普通查找 numbers10000000');

console.time('二分查找 numbers1000000');
binarySearch(numbers1000000, 122922297);
console.timeEnd('二分查找 numbers1000000');
console.time('二分查找 numbers10000000');
binarySearch(numbers10000000, 122922297);
console.timeEnd('二分查找 numbers10000000');

// read numbers1000000: 506.979ms
// read numbers10000000: 4944.900ms
// 普通查找 numbers1000000: 2.163ms
// 普通查找 numbers10000000: 14.031ms
// 二分查找 numbers1000000: 0.098ms
// 二分查找 numbers10000000: 0.012ms