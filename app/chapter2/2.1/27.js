// 2.1.27
// 希尔排序的用时是次平方级的。
// 在你的计算机上用 SortCompare 比较希尔排序和插入排序以及选择排序。
// 测试数组的大小按照 2 的幂次递增，从 128 开始。

let n = 128;
const T = 5;

const { selectionSort, insertSort, shellSort, sortCompare } = require('../../sort');

while (true) {
    console.log(`n = ${n},T = ${T}`);
    const ret = sortCompare(n, T, selectionSort, insertSort, shellSort);
    console.log(ret);
    console.log();
    n += n;
}

// n = 128, T = 5
// { selectionSort: 1.6, insertSort: 2, shellSort: 0.2 }

// n = 256, T = 5
// { selectionSort: 0.2, insertSort: 0, shellSort: 1 }

// n = 512, T = 5
// { selectionSort: 0.6, insertSort: 0.6, shellSort: 0.2 }

// n = 1024, T = 5
// { selectionSort: 2, insertSort: 1.2, shellSort: 0.2 }

// n = 2048, T = 5
// { selectionSort: 8.8, insertSort: 5.8, shellSort: 0.4 }

// n = 4096, T = 5
// { selectionSort: 34, insertSort: 23.6, shellSort: 1.6 }

// n = 8192, T = 5
// { selectionSort: 135, insertSort: 95, shellSort: 3.8 }

// n = 16384, T = 5
// { selectionSort: 553, insertSort: 485.6, shellSort: 8.6 }

// n = 32768, T = 5
// { selectionSort: 2223.2, insertSort: 1529.8, shellSort: 26 }

// n = 65536, T = 5
// { selectionSort: 8714.8, insertSort: 6058, shellSort: 68 }

// n = 131072, T = 5
// { selectionSort: 35049.6, insertSort: 25286.8, shellSort: 170 }

// n = 262144, T = 5
// { selectionSort: 141979.6, insertSort: 98717.2, shellSort: 508.6 }