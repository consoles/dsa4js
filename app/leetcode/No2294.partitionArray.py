from typing import List

class Solution:
    def partitionArray(self, nums: List[int], k: int) -> int:
        """
        排序 + 贪心：
        我们只需要关心子序列的最小值和最大值，这和子序列的元素顺序无关。为了方便计算可以先对数组进行排序。
        例如: nums = [3, 6, 1, 2, 5] ，排序后为 nums = [1, 2, 3, 5, 6], 分成 [3,1,2] 和 [6,5] 两个子序列。
        排序后，对于数组最小值 1 来讲，所有与最小值 1 相差 <=k = 2 的数都可以和 1 放在同一组。由于我们已经将数组排好序了，所以与 1 在同一组的数是连续的，即连续子数组 [1,2,3]

        从左到右遍历数组，记录当前这一段的最小值 min_val，如果 nums[i] - min_val > k，说明 nums[i] 应该放在下一段
        """
        nums.sort()
        ans = 0
        min_val = float('-inf')
        for num in nums:
            if num - min_val > k:
                min_val = num
                ans += 1
        return ans

# 2
nums = [3,6,1,2,5]
k = 2
r = Solution().partitionArray(nums, k)
print(r)
