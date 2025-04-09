from typing import List

class Solution:
    def minOperations(self, nums: List[int], k: int) -> int:
        """
        如果数组中存在比 k 小的元素，则无解
        否则数组中所有大于 k 的数字种类个数就是操作次数
        """
        s = set()
        for num in nums:
            if num < k:
                return -1
            if num > k:
                s.add(num)
        return len(s)        

# 2
# nums = [5,2,5,4,5]
# k = 2

# -1
# nums = [2,1,2]
# k = 2

# 4
nums = [9,7,5,3]
k = 1
r = Solution().minOperations(nums, k)
print(r)
