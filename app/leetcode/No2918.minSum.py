from typing import List

class Solution:
    def minSum(self, nums1: List[int], nums2: List[int]) -> int:
        """
        题目要求将两个数组中所有的 0 替换为正整数，且使它们的和相等。不难想象将两个数组中和较大的那个变成 1，另一个按照需要进行变换就行了

        1. 如果两个数组中 0 的个数都不为 0 一定存在答案，答案为 max(sum1 + zero1_count, sum2 + zero2_count)
        2. 如果某个数组中 0 的数量为 0，并且该数组中的和小于另一个数组中的 sum + zero_count 则不存在解
        """
        sum1 = sum2 = 0
        zero1_count = zero2_count = 0
        for num in nums1:
            sum1 += num
            if num == 0:
                zero1_count += 1
        for num in nums2:
            sum2 += num
            if num == 0:
                zero2_count += 1
        if zero1_count == 0 and sum2 + zero2_count > sum1:
            return -1
        if zero2_count == 0 and sum1 + zero1_count > sum2:
            return -1
        return max(sum1 + zero1_count, sum2 + zero2_count)

# 12
# nums1 = [3,2,0,1,0]
# nums2 = [6,5,0]

# -1
nums1 = [2,0,2,0]
nums2 = [1,4]
r = Solution().minSum(nums1, nums2)
print(r)