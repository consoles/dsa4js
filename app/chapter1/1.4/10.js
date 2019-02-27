// 修改二分查找算法，使之总是返回和被查找的键匹配的索引最小的元素。
// （但仍能够保证对数级别的运行时间）

const binarySearch = (arr, key) => {
    let start = 0;
    let end = arr.length - 1;
    while (start <= end) {
        let mid = Math.floor((start + end) / 2);
        if (arr[mid] === key) {
            while (arr[mid - 1] === key) {
                mid--;
            }
            return mid;
        }
        if (key > arr[mid]) {
            start = mid + 1;
        } else {
            end = mid - 1;
        }
    }
    return -1;
};

const arr = [1, 1, 1, 2, 3, 3, 3, 4, 5, 5, 6, 7, 8, 9];
let ret = binarySearch(arr, 1);
debugger