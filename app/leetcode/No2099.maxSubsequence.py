from typing import List

class Solution:
    def maxSubsequence(self, nums: List[int], k: int) -> List[int]:
        """
        贪心
        1. 创建下标数组，然后根据元素值对下标进行排序。排序后下标数组末尾的 k 个元素对应原数组中最大的 k 个元素（无论它们在原数组中的位置如何）
        2. 提取最大 k 个元素的下标并排序。对这些下标进行升序排列，以恢复它们在原数组中的相对位置
        3. 根据排序后的下标还原出子序列

        - 第一次排序：找出最大的 k 个元素的下标（不考虑它们在原数组中的顺序）
        - 第二次排序：恢复这些下标的相对顺序，确保子序列的元素顺序与原数组一致
        """
        idx = sorted(range(len(nums)), key=lambda i: nums[i])
        idx = sorted(idx[-k:])
        return [nums[i] for i in idx]
    
# nums = [2,1,3,3]
# k = 2

# nums = [-1,-2,3,4]
# k = 3

nums = [3,4,3,3]
k = 2
r = Solution().maxSubsequence(nums, k)
print(r)    