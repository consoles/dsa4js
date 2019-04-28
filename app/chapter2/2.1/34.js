// 2.1.34罕见情况
// 编写一个测试用例，调用sort方法对实际应用中可能出现的困难或者极端情况的数组进行排序。
// 例如：有序数组、逆序数组、数组中所有主键均相同、数组的主键只有两种的数组

const { sortedArray, sortedDescArray, sameArray, twoElementsArray } = require('../../util');

const { selectionSort, insertSort, shellSort } = require('../../sort');

const time = (arr, fn, T) => {
    let total = 0;
    for (let i = 0; i < T; i++) {
        const start = Date.now();
        fn.call(null, arr);
        total += (Date.now() - start);
    }
    return total / T;
}

let n = 128;
const T = 10;

console.log('T = ', T);
while (true) {
    console.log('n = ', n);
    for (const arrFn of [sortedArray, sortedDescArray, sameArray, twoElementsArray]) {
        const arr = arrFn.call(null, n);
        for (const sortFn of [selectionSort, insertSort, shellSort]) {
            const t = time(arr, sortFn, T);
            console.log(arrFn.name, sortFn.name, t);
        }
    }
    console.log();
    n += n;
}

// 无论是怎样的数组，选择排序时间复杂度不变
// 有序数组（主键相同）中，插入排序更优
// 逆序数组中插入排序也更优

// T =  10
// n =  128
// sortedArray selectionSort 0.5
// sortedArray insertSort 0.1
// sortedArray shellSort 0
// sortedDescArray selectionSort 0
// sortedDescArray insertSort 0
// sortedDescArray shellSort 0
// sameArray selectionSort 0.3
// sameArray insertSort 0
// sameArray shellSort 0.1
// twoElementsArray selectionSort 0.5
// twoElementsArray insertSort 0
// twoElementsArray shellSort 0.1

// n =  256
// sortedArray selectionSort 0.1
// sortedArray insertSort 0
// sortedArray shellSort 0
// sortedDescArray selectionSort 0.1
// sortedDescArray insertSort 0.1
// sortedDescArray shellSort 0
// sameArray selectionSort 0.1
// sameArray insertSort 0
// sameArray shellSort 0
// twoElementsArray selectionSort 0.1
// twoElementsArray insertSort 0
// twoElementsArray shellSort 0

// n =  512
// sortedArray selectionSort 0.4
// sortedArray insertSort 0
// sortedArray shellSort 0
// sortedDescArray selectionSort 0.5
// sortedDescArray insertSort 0
// sortedDescArray shellSort 0
// sameArray selectionSort 0.4
// sameArray insertSort 0
// sameArray shellSort 0
// twoElementsArray selectionSort 0.4
// twoElementsArray insertSort 0
// twoElementsArray shellSort 0.1

// n =  1024
// sortedArray selectionSort 1.4
// sortedArray insertSort 0
// sortedArray shellSort 0
// sortedDescArray selectionSort 1.2
// sortedDescArray insertSort 0
// sortedDescArray shellSort 0
// sameArray selectionSort 1.1
// sameArray insertSort 0
// sameArray shellSort 0.1
// twoElementsArray selectionSort 1.2
// twoElementsArray insertSort 0
// twoElementsArray shellSort 0

// n =  2048
// sortedArray selectionSort 4.2
// sortedArray insertSort 0
// sortedArray shellSort 0.1
// sortedDescArray selectionSort 3.9
// sortedDescArray insertSort 0
// sortedDescArray shellSort 0
// sameArray selectionSort 3.9
// sameArray insertSort 0
// sameArray shellSort 0.1
// twoElementsArray selectionSort 3.9
// twoElementsArray insertSort 0
// twoElementsArray shellSort 0.1

// n =  4096
// sortedArray selectionSort 15.6
// sortedArray insertSort 0
// sortedArray shellSort 0.1
// sortedDescArray selectionSort 15.4
// sortedDescArray insertSort 0
// sortedDescArray shellSort 0.1
// sameArray selectionSort 15.1
// sameArray insertSort 0
// sameArray shellSort 0.1
// twoElementsArray selectionSort 15.2
// twoElementsArray insertSort 0
// twoElementsArray shellSort 0.1

// n =  8192
// sortedArray selectionSort 59.3
// sortedArray insertSort 0
// sortedArray shellSort 0.2
// sortedDescArray selectionSort 61.4
// sortedDescArray insertSort 0
// sortedDescArray shellSort 0.1
// sameArray selectionSort 61.1
// sameArray insertSort 0
// sameArray shellSort 0.1
// twoElementsArray selectionSort 62.5
// twoElementsArray insertSort 0
// twoElementsArray shellSort 0.2

// n =  16384
// sortedArray selectionSort 245.1
// sortedArray insertSort 0.1
// sortedArray shellSort 0.4
// sortedDescArray selectionSort 249.9
// sortedDescArray insertSort 0
// sortedDescArray shellSort 0.4
// sameArray selectionSort 245.7
// sameArray insertSort 0
// sameArray shellSort 0.4
// twoElementsArray selectionSort 238.7
// twoElementsArray insertSort 0
// twoElementsArray shellSort 0.4

// n =  32768
// sortedArray selectionSort 966.5
// sortedArray insertSort 0.1
// sortedArray shellSort 0.8
// sortedDescArray selectionSort 967.1
// sortedDescArray insertSort 0.1
// sortedDescArray shellSort 0.8
// sameArray selectionSort 978
// sameArray insertSort 0.1
// sameArray shellSort 0.8
// twoElementsArray selectionSort 985
// twoElementsArray insertSort 0.1
// twoElementsArray shellSort 0.8