// 双调查找。
// 如果一个数组中的所有元素是先递增后递减的，则称这个数组为双调的。
// 编写一个程序，给定一个含有 N 个不同 int 值的双调数组，判断它是否含有给定的整数。
// 程序在最坏情况下所需的比较次数为 ~3lgN。

const localMax = nums => {
    if (nums.length < 3) throw new Error('检查输入');
    let lo = 0, hi = nums.length - 1;
    while (lo <= hi) {
        const mid = lo + Math.floor((hi - lo) / 2);
        if (nums[mid] > nums[mid - 1] && nums[mid] > nums[mid + 1]) {
            return mid;
        }
        if (nums[mid - 1] > nums[mid + 1]) {
            hi = mid - 1;
        } else {
            lo = mid + 1;
        }
    }
    return -1;
}

/**
 * 在3lgN时间内判断双调数组中是否有指定元素
 */
const find = (nums, num) => {

    const binarySearch = require('../../binarySearch').binarySearch

    const maxIndex = localMax(nums); // lgN得到最大元素
    const max = nums[maxIndex];
    if (num > max) return false;
    if (num === max) return true;
    const left = nums.slice(0, maxIndex);
    const right = nums.slice(maxIndex + 1);
    // 分别向左和向右进行二分搜索
    const index = binarySearch(left, num, (a, b) => a - b);
    if (index !== -1) return true;
    return binarySearch(right, num, (a, b) => b - a) !== -1;
}

const arr = [1, 2, 3, 4, 5, 6, 100, 89, 9, 8, 7];
const ret = find(arr, 8);
debugger