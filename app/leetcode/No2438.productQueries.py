from typing import List

class Solution:
    def productQueries(self, n: int, queries: List[List[int]]) -> List[int]:
        """
        二进制分解 + 直接计算
        例如: n = 11，二进制表示为 1011，从低到高的第 0,1,3 个二进制位是 1，因此分解为 [2^0, 2^1, 2^3] = [1, 2, 8]
        题目保证 n <= 10^9, 10^9 = 10^3 * 10^3 * 10^3，10^3 < 2^10，因此 10^9 < 2^10 * 2^10 * 2^10， n < 2^30
        所以分解之后的数组元素个数最大就是 29。对于每一个询问 [left,right] 可以直接遍历分解数组中的所有元素，计算答案。 
        """

        mod = 10 ** 9 + 7

        bins = []
        rep = 1
        while n > 0:
            if n % 2 == 1:
                bins.append(rep)
            n //= 2
            rep *= 2

        ans = []
        for left, right in queries:
            cur = 1
            for i in range(left, right + 1):
                cur = cur * bins[i] % mod
            ans.append(cur)   
        return ans
    
    def productQueries2(self, n: int, queries: List[List[int]]) -> List[int]:
        """
        二进制分解 + 预处理
        """
        mod = 10 ** 9 + 7
        bins = []
        rep = 1
        while n > 0:
            if n % 2 == 1:
                bins.append(rep)
            n //= 2
            rep *= 2

        m = len(bins)
        results = [[0] * m for _ in range(m)]
        for i in range(m):
            cur = 1
            for j in range(i, m):
                cur = cur * bins[j] % mod
                results[i][j] = cur

        ans = []
        for left, right in queries:   
            ans.append(results[left][right])
        return ans         

# [2,4,64]
n = 15
queries = [[0,1],[2,2],[0,3]]

# [2]
# n = 2
# queries = [[0,0]]
r = Solution().productQueries2(n, queries)
print(r)