/**
 * @param {number[]} nums
 * @param {number} k
 * @return {void} Do not return anything, modify nums in-place instead.
 */
var rotate = function (nums, k) {
    // 按照字面意思来就是干
    const len = nums.length;
    k %= len; // 搞一个小小的优化
    // while (k--) {
    //     const num = nums.pop();
    //     nums.unshift(num);
    // }
    // 不用系统提供的方法，手工一个个搬砖
    // while (k--) {
    //     const last = nums[len - 1];
    //     for (let i = nums.length - 1; i > 0; i--) {
    //         nums[i] = nums[i - 1];
    //     }
    //     nums[0] = last;
    // }
    // 原来下标为i的元素正确的位置为 (i+k)%n，用个辅助数组来解决
    // const aux = new Array(len);
    // for (let i = 0; i < len; i++) {
    //     aux[(i + k) % len] = nums[i];
    // }
    // // aux已经是正确的了，接下来进行数组覆盖
    // for (let i = 0; i < len; i++) {
    //     nums[i] = aux[i];
    // }

    // 大佬的想法：3次翻转
    function reverse(l, r) {
        while (l < r) {
            [nums[l], nums[r]] = [nums[r], nums[l]];
            l++;
            r--;
        }
    }
    reverse(0, len - 1);
    reverse(0, k - 1);
    reverse(k, len - 1);
};

// const nums = [1, 2, 3, 4, 5, 6, 7], k = 3;
const nums = [-1, -100, 3, 99], k = 2;
rotate(nums, k);
debugger;
