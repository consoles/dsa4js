/**
 * @param {number[]} nums
 * @return {void} Do not return anything, modify nums in-place instead.
 */
var moveZeroes = function (nums) {
    let index = 0;
    const len = nums.length;
    for (let i = 0; i < len; i++) {
        const num = nums[i];
        if (num !== 0) {
            nums[index++] = num;
        }
    }
    for (let i = index; i < len; i++) {
        nums[i] = 0;
    }
};

const nums = [0, 1, 0, 3, 12];
moveZeroes(nums);
debugger

// 这个思想类似于标记整理方法
// JVM时垃圾回收的标记整理法，首先将存活的对象进行标记，然后把标记好的对象向一端进行移动，然后将端界限之外的非标记对象全部清理掉。
// 再看这道题，是不是很像，把非0的数字往前移动，最终将0和非0的数组化为两段。题目源于工程开发