from typing import List
from collections import defaultdict

class Solution:
    def findLucky(self, arr: List[int]) -> int:
        """
        计数
        """
        counter = defaultdict(int)
        for num in arr:
            counter[num] += 1
        res = -1
        for num in counter:
            if num == counter[num]:
                res = max(res, num)
        return res

# 2
# arr = [2,2,3,4]

# 3
arr = [1,2,2,3,3,3]
r = Solution().findLucky(arr)
print(r)    