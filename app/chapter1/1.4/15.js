// 快速 3 - sum。
// 作为热身，使用一个线性级别的算法
// （而非基于二分查找的线性对数级别的算法）
// 实现 TwoSumFaster 来计算已排序的数组中和为 0 的整数对的数量。
// 用相同的思想为 3 - sum 问题给出一个平方级别的算法。

// 需要保证输入为有序数组！！
const twoSumFaster = arr => {
    // 已排序数组的左侧为负数，右侧为正数
    let i = 0, j = arr.length - 1;
    let cnt = 0;
    while (i < j) {
        const sum = arr[i] + arr[j];
        if (sum > 0) {
            j--;
        } else if (sum < 0) {
            i++;
        } else {
            i++;
            j--;
            cnt++;
        }
    }
    return cnt;
};

const threeSumFaster = arr => {
    let count = 0;
    for (let i = 0; i < arr.length; i++) {
        let lo = i + 1;
        let hi = arr.length - 1;
        while (lo < hi) {
            const sum = arr[i] + arr[lo] + arr[hi];
            if (sum === 0) {
                lo++;
                hi--;
                count++;
            } else if (sum > 0) {
                lo++;
            } else {
                hi--;
            }
        }
    }
    return count;
};

let arr = [-4, -2, 1, 2, 3];
let ret = twoSumFaster(arr);
arr = [-4, -2, -1, 1, 2, 2, 3];
ret = threeSumFaster(arr);
debugger