// 2.1.11
// 将希尔排序中实时计算递增序列改为预先计算并存储在一个数组中。

const swap = require('../../swap');

const shellSort = arr => {

    const n = arr.length;

    let h = 1;
    const seq = [];
    while (h < n / 3) {
        seq.push(h);
        h = 3 * h + 1;
    }

    // seq 1,4,13,40

    for (let i = seq.length - 1; i >= 0; i--) {
        let h = seq[i];
        for (let j = h; j < n; j++) {
            for (let k = j; k >= h && arr[k] < arr[k - h]; k -= h) {
                swap(arr, k, k - h);
            }
        }
    }
};

const arr = [5, 4, 1, 3, 2];
shellSort(arr);
debugger