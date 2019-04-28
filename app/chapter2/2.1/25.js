// 2.1.25 不需要交换的插入排序。
// 在插入排序的实现中使较大元素右移一位只需要访问一次数组（而不用使用 exch()）。使用 SortCompare 来评估这种做法的效果。

const swap = require('../../swap');

// https://algs4.cs.princeton.edu/21elementary/InsertionX.java.html
const insertSortNoSwap = arr => {
    const n = arr.length;

    // 从后向前扫描，参考2.1.24中的哨兵，依次将最小的元素放在最开始的位置
    let exchanges = 0;
    for (let i = n - 1; i > 0; i--) {
        if (arr[i] < arr[i - 1]) {
            swap(arr, i, i - 1);
            exchanges++;
        }
    }
    // 经过上面的操作后，最小的元素就放在了开头（哨兵）
    if (exchanges === 0) return;

    for (let i = 2; i < n; i++) {
        const v = arr[i];
        let j = i;
        while (v < arr[j - 1]) {
            arr[j] = arr[j - 1];
            j--;
        }
        if (j !== i) {
            arr[j] = v;
        }
    }
};

const insertSortNoSwap2 = arr => {
    const n = arr.length;
    for (let i = 1; i < n; i++) {
        let v = arr[i];
        let j = i - 1;
        while (j >= 0 && v < arr[j]) {
            arr[j + 1] = arr[j];
            j--;
        }
        if (j !== i - 1) {
            arr[j + 1] = v;
        }
    }
};

const { insertSort, sortCompare } = require('../../sort');

const n = 1e5;
const t = 10;
const ret = sortCompare(n, t, insertSort, insertSortNoSwap, insertSortNoSwap2);
console.log(`n = ${n},t = ${t}`);
console.log(ret);

// n = 100000, t = 10
// {
//     insertSort: 7530.2,
//     insertSortNoSwap: 3328.8,
//     insertSortNoSwap2: 4112.5
// }