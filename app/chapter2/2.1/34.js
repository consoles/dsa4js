// 2.1.34罕见情况
// 编写一个测试用例，调用sort方法对实际应用中可能出现的困难或者极端情况的数组进行排序。
// 例如：有序数组、逆序数组、数组中所有主键均相同、数组的主键只有两种的数组

const { genArrAsc, genArrDesc, genArrSame, genTwoElementsArr } = require('../../util');

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
    for (const arrFn of [genArrAsc, genArrDesc, genArrSame, genTwoElementsArr]) {
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
// genArrAsc selectionSort 0.5
// genArrAsc insertSort 0.1
// genArrAsc shellSort 0
// genArrDesc selectionSort 0
// genArrDesc insertSort 0
// genArrDesc shellSort 0
// genArrSame selectionSort 0.3
// genArrSame insertSort 0
// genArrSame shellSort 0.1
// genTwoElementsArr selectionSort 0.5
// genTwoElementsArr insertSort 0
// genTwoElementsArr shellSort 0.1

// n =  256
// genArrAsc selectionSort 0.1
// genArrAsc insertSort 0
// genArrAsc shellSort 0
// genArrDesc selectionSort 0.1
// genArrDesc insertSort 0.1
// genArrDesc shellSort 0
// genArrSame selectionSort 0.1
// genArrSame insertSort 0
// genArrSame shellSort 0
// genTwoElementsArr selectionSort 0.1
// genTwoElementsArr insertSort 0
// genTwoElementsArr shellSort 0

// n =  512
// genArrAsc selectionSort 0.4
// genArrAsc insertSort 0
// genArrAsc shellSort 0
// genArrDesc selectionSort 0.5
// genArrDesc insertSort 0
// genArrDesc shellSort 0
// genArrSame selectionSort 0.4
// genArrSame insertSort 0
// genArrSame shellSort 0
// genTwoElementsArr selectionSort 0.4
// genTwoElementsArr insertSort 0
// genTwoElementsArr shellSort 0.1

// n =  1024
// genArrAsc selectionSort 1.4
// genArrAsc insertSort 0
// genArrAsc shellSort 0
// genArrDesc selectionSort 1.2
// genArrDesc insertSort 0
// genArrDesc shellSort 0
// genArrSame selectionSort 1.1
// genArrSame insertSort 0
// genArrSame shellSort 0.1
// genTwoElementsArr selectionSort 1.2
// genTwoElementsArr insertSort 0
// genTwoElementsArr shellSort 0

// n =  2048
// genArrAsc selectionSort 4.2
// genArrAsc insertSort 0
// genArrAsc shellSort 0.1
// genArrDesc selectionSort 3.9
// genArrDesc insertSort 0
// genArrDesc shellSort 0
// genArrSame selectionSort 3.9
// genArrSame insertSort 0
// genArrSame shellSort 0.1
// genTwoElementsArr selectionSort 3.9
// genTwoElementsArr insertSort 0
// genTwoElementsArr shellSort 0.1

// n =  4096
// genArrAsc selectionSort 15.6
// genArrAsc insertSort 0
// genArrAsc shellSort 0.1
// genArrDesc selectionSort 15.4
// genArrDesc insertSort 0
// genArrDesc shellSort 0.1
// genArrSame selectionSort 15.1
// genArrSame insertSort 0
// genArrSame shellSort 0.1
// genTwoElementsArr selectionSort 15.2
// genTwoElementsArr insertSort 0
// genTwoElementsArr shellSort 0.1

// n =  8192
// genArrAsc selectionSort 59.3
// genArrAsc insertSort 0
// genArrAsc shellSort 0.2
// genArrDesc selectionSort 61.4
// genArrDesc insertSort 0
// genArrDesc shellSort 0.1
// genArrSame selectionSort 61.1
// genArrSame insertSort 0
// genArrSame shellSort 0.1
// genTwoElementsArr selectionSort 62.5
// genTwoElementsArr insertSort 0
// genTwoElementsArr shellSort 0.2

// n =  16384
// genArrAsc selectionSort 245.1
// genArrAsc insertSort 0.1
// genArrAsc shellSort 0.4
// genArrDesc selectionSort 249.9
// genArrDesc insertSort 0
// genArrDesc shellSort 0.4
// genArrSame selectionSort 245.7
// genArrSame insertSort 0
// genArrSame shellSort 0.4
// genTwoElementsArr selectionSort 238.7
// genTwoElementsArr insertSort 0
// genTwoElementsArr shellSort 0.4

// n =  32768
// genArrAsc selectionSort 966.5
// genArrAsc insertSort 0.1
// genArrAsc shellSort 0.8
// genArrDesc selectionSort 967.1
// genArrDesc insertSort 0.1
// genArrDesc shellSort 0.8
// genArrSame selectionSort 978
// genArrSame insertSort 0.1
// genArrSame shellSort 0.8
// genTwoElementsArr selectionSort 985
// genTwoElementsArr insertSort 0.1
// genTwoElementsArr shellSort 0.8
