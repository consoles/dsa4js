from typing import List
from itertools import pairwise

class Solution:
    def maxAdjacentDistance(self, nums: List[int]) -> int:
        n = len(nums)
        max_distance = 0
        for i in range(1, n):
            dis = abs(nums[i] - nums[i - 1])
            max_distance = max(max_distance, dis)
        max_distance = max(max_distance, abs(nums[0] - nums[n - 1]))
        return max_distance

    def maxAdjacentDistance2(self, nums: List[int]) -> int:
        # 将第一个元素放在数组的末尾就不用特殊处理最后一个元素和第一个元素的比较问题了
        # max_distance = 0
        # nums2 = nums + [nums[0]]
        # for i in range(1, len(nums2)):
        #     dis = abs(nums2[i] - nums2[i - 1])
        #     max_distance = max(max_distance, dis)
        # return max_distance

        # itertools.pairwise 是 python3.10 新增的,用于将序列转化为连续的相邻对
        return max(abs(x - y) for x, y in pairwise(nums + [nums[0]]))



# 5
# nums = [-5,-10,-5]

# 3
# nums = [1,2,4]

nums = [-2,1,-5]
r = Solution().maxAdjacentDistance2(nums)
print(r)
