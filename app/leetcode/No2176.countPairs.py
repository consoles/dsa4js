from typing import List

class Solution:
    def countPairs(self, nums: List[int], k: int) -> int:
        # 按照题目意思进行枚举 o(N^2)
        count = 0
        n = len(nums)
        for i in range(n):
            for j in range(i + 1, n):
                if nums[i] == nums[j] and (i * j) % k == 0:
                    count += 1
        return count
    
    def countPairs2(self, nums: List[int], k: int) -> int:
        # 用一个 map 保存当前数字的索引，只需要一次遍历，正常情况下 O(N)，最差 O(N^2)
        count = 0
        n = len(nums)
        num_index_map = {}
        for i in range(n):
            num = nums[i]
            if num in num_index_map:
                for index in num_index_map[num]:
                    if (i * index) % k == 0:
                        count += 1
                num_index_map[num].append(i)        
            else:
                num_index_map[num] = [i]
        return count        


# 4
nums = [3,1,2,2,2,1,3]
k = 2

# 0
# nums = [1,2,3,4]
# k = 1
r = Solution().countPairs2(nums, k)
print(r)
