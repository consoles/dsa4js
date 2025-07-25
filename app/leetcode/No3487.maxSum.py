from typing import List

class Solution:
    def maxSum(self, nums: List[int]) -> int:
        """
        为了让元素和尽可能大，负数不能留。但是如果数组中的数全是负数，此时答案为 nums 的最大值。
        0 对和没有贡献，而正数能让和增加，选择正数就行
        """
        positive_nums_set = set(num for num in nums if num > 0)
        if len(positive_nums_set) == 0:
            return max(nums)
        return sum(positive_nums_set)

# 15
# nums = [1,2,3,4,5]

# 1
# nums = [1,1,0,1,1]

# 3
nums = [1,2,-1,-2,1,0,-1]
r = Solution().maxSum(nums) 
print(r)   