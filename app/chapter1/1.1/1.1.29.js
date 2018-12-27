// 1.1.29 等值键。为 BinarySearch 类添加一个静态方法 rank() ，它接受一个键和一个整型有序数组(可能存在重复键)作为参数并返回数组中小于该键的元素数量，以及一个类似的方法 count() 来返回数组中等于该键的元素的数量。注意: 如果 i 和 j 分别是 rank(key, a) 和 count(key, a)
// 的返回值，那么 a[i..i + j - 1] 就是数组中所有和 key 相等的元素。

const { binarySearch } = require('../../binarySearch');

/**
 * 数组中小于key的元素的数量
 * @param {*} key 
 * @param {*} sortedArr 
 */
const rank = (key, sortedArr) => {
    let start = 0;
    let end = sortedArr.length - 1;

    while (start <= end) {
        let mid = start + parseInt((end - start) / 2);
        let item = arr[mid];
        if (item === key) {
            // 二分查找主要解决的问题是高效判定数组中是否有指定元素，如果数组中没有重复元素还能顺便解决第一个出现此元素的索引
            // 即：不能很好解决indexOf的问题
            for (let i = start; i <= end; i++) {
                if (sortedArr[i] === key) {
                    return i;
                }
            }
        }
        if (item >= key) {
            end = mid - 1;
        } else if (item < key) {
            start = mid + 1;
        }
    }

    return -1;
};

/**
 * 数组中等于key的元素的数量
 * @param {*} key 
 * @param {*} sortedArr 
 */
const count = (key, sortedArr) => {

    let start = 0;
    let end = sortedArr.length - 1;
    let count = 0;

    while (start <= end) {
        let mid = start + parseInt((end - start) / 2);
        if (arr[mid] === key) {
            for (let i = start; i <= end; i++) {
                if (sortedArr[i] === key) {
                    count++;
                }
            }
            return count;
        } else if (arr[mid] > key) {
            end = mid - 1;
        } else {
            start = mid + 1;
        }
    }

    return count;
};

const arr = [-2, 1, 3, 3, 5, 6, 6, 7, 9, 9];

const lessCount = rank(6, arr);
const eqCount = count(6, arr);
debugger