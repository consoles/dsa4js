/**
 * @param {number[]} nums
 * @param {number} target
 * @return {number[][]}
 */
var fourSum = function (nums, target) {
    nums.sort((a, b) => a - b)
    const res = []
    const n = nums.length
    // 排序、去重 + 双指针
    for (let i = 0; i < n - 3; i++) {
        // 去重
        if (i > 0 && nums[i] === nums[i - 1]) continue
        for (let j = i + 1; j < n - 2; j++) {
            if (j > i + 1 && nums[j] === nums[j - 1]) continue
            // 双指针,start从j+1开始，因为有2个数(i和j)确定了
            let start = j + 1, end = n - 1
            const s = target - nums[i] - nums[j]
            while (start < end) {
                const sum = nums[start] + nums[end]
                if (sum === s) {
                    res.push([nums[i], nums[j], nums[start], nums[end]])
                    while (start < end && nums[start + 1] === nums[start]) {
                        start++
                    }
                    while (start < end && nums[end - 1] === nums[end]) {
                        end--
                    }
                    start++
                    end--
                } else if (sum < s) {
                    start++
                } else {
                    end--
                }
            }
        }
    }
    return res
};

nums = [1, 0, -1, 0, -2, 2], target = 0;
console.log(fourSum(nums, target));