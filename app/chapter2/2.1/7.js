// 2.1.7
// 对于逆序数组，选择排序和插入排序谁更快？

// 假设比较的开销小于等于交换的开销，此时选择排序更快.

// 插入排序：比较和交换均是N^2 / 2
// 选择排序：比较N^2 / 2，交换 N

// 如果数组完全逆序，则选择排序的性能高于插入排序

const { selectionSort, insertSort } = require('../../sort');

const generateReverseArr = N => {
    const arr = [];
    for (let i = 0; i < N; i++) {
        arr.push(Math.random());
    }
    arr.sort((a, b) => b - a);
    return arr;
}

/**
 * @param {*} T 重复试验的次数，多次试验取平均值可以消除偶然误差
 * @param {*} N 数组长度
 */
const time = (fn, T, N) => {
    let total = 0;
    for (let i = 0; i < T; i++) {
        const arr = generateReverseArr(N);
        const start = Date.now();
        fn.call(null, arr);
        total += (Date.now() - start);
    }
    return total / T;
}

(() => {
    const T = 10;

    for (let n = 1000; true; n += n) {

        const t1 = time(insertSort, T, n);
        const t2 = time(selectionSort, T, n);

        console.log(`T = ${T},N = ${n}`);
        console.log('插入排序', t1);
        console.log('选择排序', t2);
        console.log('插入排序 / 选择排序', t1 / t2);
        console.log();
    }
})();

// T = 10, N = 1000
// 插入排序 1.8
// 选择排序 0.9
// 插入排序 / 选择排序 2

// T = 10, N = 2000
// 插入排序 5.3
// 选择排序 2.7
// 插入排序 / 选择排序 1.9629629629629628

// T = 10, N = 4000
// 插入排序 21.3
// 选择排序 11.8
// 插入排序 / 选择排序 1.8050847457627117

// T = 10, N = 8000
// 插入排序 88.8
// 选择排序 46.3
// 插入排序 / 选择排序 1.91792656587473

// T = 10, N = 16000
// 插入排序 354.9
// 选择排序 182.9
// 插入排序 / 选择排序 1.940404592673592

// T = 10, N = 32000
// 插入排序 1427.4
// 选择排序 741.2
// 插入排序 / 选择排序 1.925796006475985
