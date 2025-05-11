from typing import List

class Solution:
    def threeConsecutiveOdds(self, arr: List[int]) -> bool:
        """
        最朴素的想法:保持一个长度为 3 的滑动窗口
        """
        n = len(arr)
        for i in range(n - 2):
            if arr[i] % 2 == 0:
                continue
            j = i + 1
            k = i + 2
            if j < n and k < n:
                if arr[j] % 2 == 1 and  arr[k] % 2 == 1:
                    return True
            else:
                break    

        return False

# False
# arr = [2,6,4,1]

# True
# arr = [1,2,34,3,4,5,7,23,12]

# False
# arr = [1,2,1,1]

# True
arr = [1,1,1]
r = Solution().threeConsecutiveOdds(arr)
print(r)                
