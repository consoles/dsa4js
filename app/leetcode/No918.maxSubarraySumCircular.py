from typing import List

class Solution:
    def maxSubarraySumCircular(self, nums: List[int]) -> int:
        # 可能出现下面 2 种情况：
        # 1. 普通子数组（不跨越环形边界）：退化到 leetcode53
        # 2. 跨越边界：此时的最大子序和 = 数组总和 - 最小子序和

        # 计算普通子数组的最大字序和
        cur_sum = max_sum = nums[0]
        for num in nums[1:]:
            if cur_sum < 0:
                cur_sum = num
            else:
                cur_sum += num
            if cur_sum > max_sum:
                max_sum = cur_sum

        # 计算普通子数组的最小字序和
        cur_sum = min_sum = nums[0]
        for num in nums[1:]:
            # 前面的和是负数，可以让当前和减小
            if cur_sum < 0:
                cur_sum += num
            else:
                cur_sum = num
            if cur_sum < min_sum:
                min_sum = cur_sum

        total_sum = sum(nums)
        
        # 所有数都是负数
        if max_sum < 0:
            return max_sum

        return max(max_sum, total_sum - min_sum)                

# nums = [1,-2,3,-2] # 3
nums = [5,-3,5] # 10
# nums = [3,-2,2,-3] # 3
r = Solution().maxSubarraySumCircular(nums)
print(r)
