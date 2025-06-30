from typing import List
from collections import defaultdict

class Solution:
    def findLHS(self, nums: List[int]) -> int:
        """滑动窗口"""
        nums.sort()
        res = 0
        begin = 0
        for end in range(len(nums)):
            while nums[end] - nums[begin] > 1:
                begin += 1
            if nums[end] - nums[begin] == 1:
                res = max(res, end - begin + 1)
        return res
    
    def findLHS2(self, nums: List[int]) -> int:
        """
        哈希表：对于 x，计算出 count(x) + count(x+1) 就是针对 x 的和谐子序列的长度
        """
        counter = defaultdict(int)
        for num in nums:
            counter[num] += 1
        res = 0
        for num in counter:
            if num + 1 in counter:
                res = max(res, counter[num] + counter[num + 1])
        return res

# 5
nums = [1,3,2,2,5,2,3,7]
r = Solution().findLHS2(nums)
print(r)    