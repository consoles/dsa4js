/**
 * @param {number[]} nums
 * @return {string[]}
 */
var summaryRanges = function (nums) {
    // 求区间，按照字面意思一把梭哈
    const ranges = [];
    const len = nums.length;
    if (len <= 0) return ranges;
    let l = 0;
    while (l < len) {
        const start = l;
        while (l + 1 < len && nums[l + 1] === nums[l] + 1) {
            l++;
        }
        const item = l === start ? `${nums[l]}` : `${nums[start]}->${nums[l]}`;
        ranges.push(item);
        l++;
    }
    return ranges;
};

// const nums = [0, 1, 2, 4, 5, 7];
// const nums = [0, 2, 3, 4, 6, 8, 9];
// const nums = [];
// const nums = [-1];
const nums = [0];
const res = summaryRanges(nums);
debugger;
