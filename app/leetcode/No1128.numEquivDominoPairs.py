from typing import List
from collections import defaultdict

class Solution:
    def numEquivDominoPairs(self, dominoes: List[List[int]]) -> int:
        # 暴力法 O(N^2)
        n = len(dominoes)
        ans = 0
        for i in range(n):
            a, b = dominoes[i]
            for j in range(i + 1, n):
                c, d = dominoes[j]
                if (a == c and b == d) or (a == d and b == c):
                    ans += 1
        return ans
    
    def numEquivDominoPairs2(self, dominoes: List[List[int]]) -> int:
        """
        保证每个多米诺骨牌中第一个元素小于第二个元素
        [1, 2] 和 [2, 1] 都变成 [1, 2]
        统计相同数对的数量就可以了
        """
        ans = 0
        cnt = defaultdict(int)
        for a, b in dominoes:
            if a > b:
                a, b = b, a
            d = (a, b)
            # 本题要求 i<j，如果先更新 cnt，再更新答案，就把 dominoes[j] 自己也统计进来了
            # 相当于把 i=j 的情况也认为是好数对
            ans += cnt[d]    
            cnt[d] += 1
        return ans

# 1
dominoes = [[1,2],[2,1],[3,4],[5,6]]

# 3
# dominoes = [[1,2],[1,2],[1,1],[1,2],[2,2]]
r = Solution().numEquivDominoPairs2(dominoes)
print(r)    