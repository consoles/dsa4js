// 排序算法的性能比较

const { selectionSort, insertSort, shellSort2 } = require('../sort');

const genRandomArr = N => {
    const arr = [];
    for (let i = 0; i < N; i++) {
        arr.push(Math.random());
    }
    return arr;
}

/**
 * @param {*} T 重复试验的次数，多次试验取平均值可以消除偶然误差
 * @param {*} N 数组长度
 */
const time = (fn, T, N) => {
    let total = 0;
    for (let i = 0; i < T; i++) {
        const arr = genRandomArr(N);
        const start = Date.now();
        fn.call(null, arr);
        total += (Date.now() - start);
    }
    return total / T;
}

(() => {
    const T = 100;

    for (let n = 1000; true; n += n) {
        // const t1 = time(selectionSort, T, n);
        // const t2 = time(insertSort, T, n);

        // console.log(`T = ${T},N = ${n}`);
        // console.log('选择排序', t1);
        // console.log('插入排序', t2);
        // console.log('选择排序 / 插入排序', t1 / t2);
        // console.log();

        const t1 = time(insertSort, T, n);
        const t2 = time(shellSort2, T, n);

        console.log(`T = ${T},N = ${n}`);
        console.log('插入排序', t1);
        console.log('希尔排序', t2);
        console.log('插入排序 / 希尔排序', t1 / t2);
        console.log();
    }
})();

// T = 100,N = 1000
// 选择排序 2.2
// 插入排序 1.51
// 选择排序 / 插入排序 1.456953642384106

// T = 100,N = 2000
// 选择排序 8.65
// 插入排序 5.67
// 选择排序 / 插入排序 1.525573192239859

// T = 100,N = 4000
// 选择排序 32.58
// 插入排序 22.62
// 选择排序 / 插入排序 1.4403183023872677

// T = 100,N = 8000
// 选择排序 130.51
// 插入排序 90.19
// 选择排序 / 插入排序 1.4470562146579442

// T = 100,N = 16000
// 选择排序 515.63
// 插入排序 389.83
// 选择排序 / 插入排序 1.3227047687453506

// T = 100,N = 32000
// 选择排序 2072.64
// 插入排序 1459.52
// 选择排序 / 插入排序 1.4200833150624863

// T = 100,N = 64000
// 选择排序 8323.47
// 插入排序 5792.89
// 选择排序 / 插入排序 1.4368424050862347

// T = 100,N = 128000
// 选择排序 33262.09
// 插入排序 23382.37
// 选择排序 / 插入排序 1.42252859740052

// T = 100,N = 256000
// 选择排序 132762.07
// 插入排序 93106.08
// 选择排序 / 插入排序 1.4259226679933255

// T = 100,N = 512000
// 选择排序 535260.12
// 插入排序 372356.65
// 选择排序 / 插入排序 1.4374931131215192

// T = 100,N = 1000
// 插入排序 1.49
// 希尔排序 0.24
// 插入排序 / 希尔排序 6.208333333333334

// T = 100,N = 2000
// 插入排序 5.61
// 希尔排序 0.42
// 插入排序 / 希尔排序 13.357142857142858

// T = 100,N = 4000
// 插入排序 22.72
// 希尔排序 0.95
// 插入排序 / 希尔排序 23.91578947368421

// T = 100,N = 8000
// 插入排序 90.87
// 希尔排序 2.05
// 插入排序 / 希尔排序 44.32682926829269

// T = 100,N = 16000
// 插入排序 358.45
// 希尔排序 4.78
// 插入排序 / 希尔排序 74.98953974895397

// T = 100,N = 32000
// 插入排序 1466.29
// 希尔排序 10.36
// 插入排序 / 希尔排序 141.53378378378378

// T = 100,N = 64000
// 插入排序 5783.29
// 希尔排序 23.4
// 插入排序 / 希尔排序 247.14914529914532

// T = 100,N = 128000
// 插入排序 23188.54
// 希尔排序 53.04
// 插入排序 / 希尔排序 437.1896681749623

// T = 100,N = 256000
// 插入排序 92853
// 希尔排序 118.98
// 插入排序 / 希尔排序 780.4084720121028