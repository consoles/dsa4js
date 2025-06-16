from math import inf
from typing import List

class Solution:
    def maximumDifference(self, nums: List[int]) -> int:
        n = len(nums)
        max_diff = 0
        for i in range(n):
            for j in range(i + 1, n):
                if nums[j] > nums[i]:
                    max_diff = max(max_diff, nums[j] - nums[i])
        return max_diff

    def maximumDifference2(self, nums: List[int]) -> int:  
        """
        前缀最小值：类似 No121.买卖股票的最佳时机
        从右到左枚举卖出价格 nums[j]，要想获得最大利润，我们需要知道 price[0..j-1] 的最小值(前缀最小值)，把这个最小值作为买入价。可以使用变量 preMin 来维护，用 nums[j] - preMin 更新最大利润
        """
        ans = -1
        preMin = nums[0]
        for i in range(1, len(nums)):
            if nums[i] < preMin:
                preMin = nums[i]
            if nums[i] > preMin:
                ans = max(ans, nums[i] - preMin)
        return ans

# 4
nums = [7,1,5,4]
r = Solution().maximumDifference2(nums)
print(r)        