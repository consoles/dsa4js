from typing import List

class Solution:
    def countGoodTriplets(self, arr: List[int], a: int, b: int, c: int) -> int:
        # 模拟 O(N^3)
        n = len(arr)
        count = 0
        for i in range(n):
            for j in range(i + 1, n):
                for k in range(j + 1, n):
                    if abs(arr[i] - arr[j]) <= a and abs(arr[j] - arr[k]) <= b and abs(arr[i] - arr[k]) <= c:
                        count += 1
        return count
    
    def countGoodTriplets2(self, arr: List[int], a: int, b: int, c: int) -> int:
        # TODO: 还有前缀和的解法
        pass


arr = [3,0,1,1,9,7]
a = 7
b = 2
c = 3
r = Solution().countGoodTriplets2(arr, a, b, c)
print(r)
