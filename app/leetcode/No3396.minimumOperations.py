from typing import List
import math

class Solution:
    def minimumOperations(self, nums: List[int]) -> int:
        # 模拟
        op_count = 0
        n = len(nums)
        i = 0
        def check(index: int):
            if index >= n:
                return True
            s = set()
            for num in nums[index:]:
                if num in s:
                    return False
                s.add(num)
            return True    

        while not check(i):
            i += 3
            op_count += 1

        return op_count
    
    def minimumOperations2(self, nums: List[int]) -> int:
        """
        倒序遍历
        nums=[1,2,3,4,2,3,3,5,7]
        我们可以倒着遍历 nums，遍历到 nums[5]=3 时，发现之前遍历过相同的数 nums[6]=3，这意味着 nums[0..5]=[1,2,3,4,2,3] 都要移除，操作 2 次。
        """ 
        seen = set()
        for i in range(len(nums) - 1, -1, -1):
            x = nums[i]
            if x in seen:
                return math.ceil((i + 1) / 3)
            seen.add(x)
        return 0    

# 2 
nums = [1,2,3,4,2,3,3,5,7]    

# 2
# nums = [4,5,6,4,4]

# 0
# nums = [6,7,8,9]
r = Solution().minimumOperations2(nums)
print(r)
