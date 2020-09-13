/**
 * @param {number[]} nums
 * @return {number[]}
 */
var runningSum = function(nums) {
    if (nums.length <= 0) return []
    const res = new Array(nums.length)
    res[0] = nums[0]
    for(let i = 1;i < nums.length;i++) {
        res[i] = res[i-1] + nums[i]
    }
    return res
};

nums = [1,2,3,4]
console.log(runningSum(nums));
nums = [1,1,1,1,1]
console.log(runningSum(nums));