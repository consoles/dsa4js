// 修改binarySearch类中的测试用例来删除排序之后白名单中的重复元素

const sortedArr = [-2, 1, 3, 3, 5, 6, 6, 7, 9, 9];

const { binarySearch } = require('../binarySearch');

const uniq = arr => {
    const ret = [];
    arr.forEach(item => {
        if (binarySearch(ret, item) === -1) {
            ret.push(item);
        }
    });
    return ret;
};

// const ret = uniq(sortedArr);
// debugger

// 已经排序的数组中重复元素一定是相邻的元素

const uniq2 = arr => {
    // const ret = [];
    // for (let item of arr) {
    //     if (ret[ret.length - 1] !== item) {
    //         ret.push(item);
    //     }
    // }
    // return ret;

    let j = 0;
    for (let i = 0; i < arr.length; i++) {
        if (arr[j] !== arr[i]) {
            arr[++j] = arr[i];
        }
    }

    return arr.slice(0, j + 1);
}

// 以上2种算法本质上还需要遍历整个数组，没有用到所谓的二分查找

const uniq3 = arr => {
    let lo = 0;
    let hi = arr.length - 1;

    const duplates = [];

    while (lo <= hi) {
        let mid = lo + parseInt((hi - lo) / 2);
        if (key < arr[mid]) {
            hi = mid - 1;
        } else if (key > arr[mid]) {
            lo = mid + 1;
        } else {
            // 如果找到这个元素，则遍历lo到hi所有的元素
            for (let i = lo; i <= hi; i++) {
                if (i !== mid && arr[i] === key) {
                    duplates.push(arr[i]);
                }
            }
        }
    }
};

const ret2 = uniq2(sortedArr);
debugger