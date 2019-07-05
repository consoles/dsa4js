// 2.1.30
// 几何级数递增序列。
// 通过实验找到一个 t，使得对于大小为 N = 10 ^ 6 的任意随机数组，
// 使用递增序列 1, [t], [t ^ 2], [t ^ 3], [t ^ 4], … 的希尔排序的运行时间最短。
// 给出你能找到的三个最佳 t 值以及相应的递增序列。
// 以下练习描述的是各种用于评估排序算法的测试用例。
// 它们的作用是用随机数据帮助你增进对性能特性的理解。
// 随着命令行指定的实验测试的增大，
// 可以和 SortCompare 一样在它们中使用 time() 函数来得到更精确的结果。
// 在以后的几节中我们会使用这些练习来评估更为复杂的算法。

const assert = require('assert');

const swap = require('../../swap');
const { isSorted, genRandomDoubleArray } = require('../../util');

// 可以传入递增序列的希尔排序
const shellSort = (arr, seq) => {
    const n = arr.length;
    for (let i = seq.length - 1; i >= 0; i--) {

        const h = seq[i];
        for (let j = h; j < n; j++) {
            for (let k = j; k >= h && arr[k] < arr[k - h]; k -= h) {
                swap(arr, k, k - h);
            }
        }

    }
    assert(isSorted(arr));
};

const n = 10e6;

const arr = genRandomDoubleArray(n);

for (let i = 2; i <= n; i++) {
    const seq = [1];
    const t = i;
    let tmp = i;
    while (tmp < n) {
        seq.push(tmp);
        tmp *= tmp;
    }
    let array = arr.slice();
    const start = Date.now();
    shellSort(array, seq);
    const time = Date.now() - start;
    console.log(`t = ${t},seq = ${JSON.stringify(seq)},time = ${time}`);
}

// t = 4, seq = [1, 4, 16, 256, 65536], time = 237660
// t = 11, seq = [1, 11, 121, 14641], time = 224691
// t = 13, seq = [1, 13, 169, 28561], time = 214937

// t = 2, seq = [1, 2, 4, 16, 256, 65536], time = 269258
// t = 3, seq = [1, 3, 9, 81, 6561], time = 332667
// t = 4, seq = [1, 4, 16, 256, 65536], time = 237660
// t = 5, seq = [1, 5, 25, 625, 390625], time = 344083
// t = 6, seq = [1, 6, 36, 1296, 1679616], time = 405689
// t = 7, seq = [1, 7, 49, 2401, 5764801], time = 406692
// t = 8, seq = [1, 8, 64, 4096], time = 422612
// t = 9, seq = [1, 9, 81, 6561], time = 362210
// t = 10, seq = [1, 10, 100, 10000], time = 254088
// t = 11, seq = [1, 11, 121, 14641], time = 224691
// t = 12, seq = [1, 12, 144, 20736], time = 259813
// t = 13, seq = [1, 13, 169, 28561], time = 214937
// t = 14, seq = [1, 14, 196, 38416], time = 253654
// t = 15, seq = [1, 15, 225, 50625], time = 283326
// t = 16, seq = [1, 16, 256, 65536], time = 299295
// t = 17, seq = [1, 17, 289, 83521], time = 276032
// t = 18, seq = [1, 18, 324, 104976], time = 347239
// t = 19, seq = [1, 19, 361, 130321], time = 333728
// t = 20, seq = [1, 20, 400, 160000], time = 356615
// t = 21, seq = [1, 21, 441, 194481], time = 360961
// t = 22, seq = [1, 22, 484, 234256], time = 386153
// t = 23, seq = [1, 23, 529, 279841], time = 444000
// t = 24, seq = [1, 24, 576, 331776], time = 448556
// t = 25, seq = [1, 25, 625, 390625], time = 448132
// t = 26, seq = [1, 26, 676, 456976], time = 400998
// t = 27, seq = [1, 27, 729, 531441], time = 420321
// t = 28, seq = [1, 28, 784, 614656], time = 464849
// t = 29, seq = [1, 29, 841, 707281], time = 443931
// t = 30, seq = [1, 30, 900, 810000], time = 487036
// t = 31, seq = [1, 31, 961, 923521], time = 415800
// t = 32, seq = [1, 32, 1024, 1048576], time = 504522
// t = 33, seq = [1, 33, 1089, 1185921], time = 482779
// t = 34, seq = [1, 34, 1156, 1336336], time = 473467
// t = 35, seq = [1, 35, 1225, 1500625], time = 473481
// t = 36, seq = [1, 36, 1296, 1679616], time = 506586
// t = 37, seq = [1, 37, 1369, 1874161], time = 514607
// t = 38, seq = [1, 38, 1444, 2085136], time = 503782
// t = 39, seq = [1, 39, 1521, 2313441], time = 518553
// t = 40, seq = [1, 40, 1600, 2560000], time = 530701
// t = 41, seq = [1, 41, 1681, 2825761], time = 596207
// t = 42, seq = [1, 42, 1764, 3111696], time = 568508
