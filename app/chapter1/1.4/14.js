// 为 4 - sum 设计一个算法

// 暴力法
const fourSum = nums => {
    let cnt = 0;
    const len = nums.length;
    for (let i = 0; i < len; i++)
        for (let j = i + 1; j < len; j++)
            for (let k = j + 1; k < len; k++)
                for (let l = k + 1; l < len; l++)
                    if (nums[i] + nums[j] + nums[k] + nums[l] === 0) cnt++;
    return cnt;
};

// 基于二分搜索，参加three-sum
const fourSum2 = nums => {
    let cnt = 0;
    const binarySearch = require('../../binarySearch').binarySearchByRecursion;
    const len = nums.length;
    nums.sort((a, b) => a - b);
    for (let i = 0; i < len; i++)
        for (let j = i + 1; j < len; j++)
            for (let k = j + 1; k < len; k++) {
                const sum = nums[i] + nums[j] + nums[k];
                // 将最里面的循环替换为二分搜索N^4 => N^3lgN
                const index = binarySearch(nums, -sum, k + 1, len - 1);
                if (index !== -1) cnt++;
            }
    return cnt;
};

const arr = [1, 1, -1, -1, 6, 7];
const ret = fourSum2(arr);
debugger