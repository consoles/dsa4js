/**
 * @param {number[]} nums
 * @return {number}
 */
var removeDuplicates = function (nums) {
    // 方法1：利用set去重
    // const set = new Set();
    // let len = 0;
    // for (const num of nums) {
    //     if (!set.has(num)) {
    //         nums[len++] = num;
    //         set.add(num);
    //     }
    // }
    // return len;

    // 方法2，单指针维护当前有效元素的个数
    const n = nums.length;
    if (n === 0) return 0;
    let index = 1;
    for (let i = 1; i < n; i++) {
        const num = nums[i];
        if (num !== nums[i - 1]) {
            nums[index++] = num;
        }
    }
    return index;
};

nums = [1, 1, 2]
nums = [0, 0, 1, 1, 1, 2, 2, 3, 3, 4]
ret = removeDuplicates(nums);
debugger;