/**
 * @param {number[]} nums
 * @return {string[]}
 */
var summaryRanges = function (nums) {
    // 求区间，按照字面意思一把梭哈
    const ranges = [];
    const len = nums.length;
    if (len <= 0) return ranges;
    let leftIndex = 0
    while(leftIndex < len) {
        const leftValue = nums[leftIndex]
        let rightIndex = leftIndex
        while(rightIndex < len && nums[rightIndex + 1] === nums[rightIndex] + 1) {
            rightIndex++
        }
        if (leftIndex === rightIndex) {
            ranges.push(leftValue + '')
        } else {
            ranges.push(`${leftValue}->${nums[rightIndex]}`)
        }
        leftIndex = rightIndex + 1
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
