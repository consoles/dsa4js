// 设计一个算法，使用对数级别的比较次数找出有理数 p / q，其中 0 < p < q < N， 比较形式为给定的数是否小于 x ?
// 提示：两个分母均小于N的有理数之差不小于1/N^2

const binarySearch = (arr, key) => {
    let lo = 0;
    let hi = arr.length - 1;
    const e = 1 / (arr.length * arr.length);
    while (lo <= hi) {
        const mid = lo + Math.floor((hi - lo) / 2);
        if (Math.abs(arr[mid] - key) <= e) {
            return mid;
        }
        if (arr[mid] < key) {
            lo = mid + 1;
        } else {
            hi = mid - 1;
        }
    }
    return -1;
};

const arr = [1.0 / 2.0, 2.0 / 3.0, 3.0 / 4.0, 4.0 / 5.0, 5.0 / 6.0];
arr.sort((a, b) => a - b);
const index = binarySearch(arr, 4 / 5);
debugger