from typing import List
from collections import defaultdict

class Solution:
    def numIdenticalPairs(self, nums: List[int]) -> int:
        n = len(nums)
        c = 0
        for i in range(n - 1):
            for j in range(i + 1, n):
                if nums[i] == nums[j]:
                    c += 1
        return c
    
    def numIdenticalPairs2(self, nums: List[int]) -> int:
        """
        使用数学方法进行组合计数，假设数字 k 在序列中出现了 x 次，
        那么满足题目中所说的 nums[i] == nums[j] = k (i < j) 的数对 (i, j) 的数量为 x * (x - 1) // 2。
        O(n)
        """
        freq = defaultdict(int) # 统计数字出现的次数
        for num in nums:
            freq[num] += 1

        c = 0    
        for count in freq.values():
            c += count * (count - 1) // 2
        return c
    
    def numIdenticalPairs3(self, nums: List[int]) -> int:
        """
        解法 2 已经非常优秀了，但是可以更进一步！
        用一个哈希表 freq 来统计 j 左边每个数字的出现次数。freq[x] 表示在 j 左边的值为 x 元素的个数
        例如: nums = [1, 1, 2, 1]，遍历到 j = 3 的时候 freq[1] = 2，那么就找到 2 个数对，把 2 累加仅答案
        """
        c = 0
        freq = defaultdict(int)
        # 注意本题的细节：先累加答案，再更新 freq
        # 因为本题要求 i < j
        # 如果先更新 freq，再累加答案，就把 nums[j] 自己也统计进来了，相当于把 i=j 的情况也认为是好数对
        for num in nums:
            c += freq[num]
            freq[num] += 1
        return c    

# 4
# nums = [1,2,3,1,1,3]

# 6
# nums = [1,1,1,1]

# 0
nums = [1,2,3]
r = Solution().numIdenticalPairs2(nums)
print(r)
