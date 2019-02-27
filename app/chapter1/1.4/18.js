// 数组的局部最小元素。
// 编写一个程序，给定一个含有 N 个不同整数的数组，找到一个局部最小元素：
// 满足 a[i] < a[i - 1]，且 a[i] < a[i + 1] 的索引 i。
// 程序在最坏情况下所需的比较次数为 ~2lgN。

// 暴力法 O(N)
const localMin1 = nums => {
    for (let i = 1; i < nums.length - 1; i++) {
        if (nums[i] < nums[i - 1] && nums[i] < nums[i + 1]) {
            return i;
        }
    }
    return -1;
}

// 检查数组中间值a[N/2]以及它相邻的元素a[N/2-1]和a[N/2+1]。如果a[N/2]是一个局部最小值则算法终止；否则在较小的相邻元素的那一侧查找
// 和二分查找的方式类似，先确认中间的值是否是局部最小，如果不是，则向较小的一侧二分查找。O(lgN)
const localMin2 = nums => {
    let lo = 0, hi = nums.length - 1;
    while (lo <= hi) {
        const mid = lo + Math.floor((hi - lo) / 2);
        if (nums[mid] < nums[mid - 1] && nums[mid] < nums[mid + 1]) {
            return mid;
        }
        if (nums[mid - 1] < nums[mid + 1]) {
            hi = mid - 1;
        } else {
            lo = mid + 1;
        }
    }
    return -1;
};

const arr = [1, 2, 1, -2, 3, 5, 6, 5, 3, 7];
let index = localMin2(arr);
debugger