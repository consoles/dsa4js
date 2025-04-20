from typing import List
from collections import defaultdict

class Solution:
    def numRabbits(self, answers: List[int]) -> int:
        """
        1. 同一种颜色的兔子，答案必然相同
        2. 回答相同答案的兔子却不一定是相同颜色

        例如：
        有 3 只白兔，每只白兔的答案必然是 2，对应结论 1
        有 3 只白兔，3 只黑兔，则分别问白兔和黑兔中的其中一只，它们的答案也是 2，对应结论 2

        当有兔子回答 x 的时候，一定存在最多容纳 x + 1 只兔子的阵营，且同一阵营中的兔子回答都是一样的，均为 x。因此我们需要记录有多少只兔子回答了 x，mp[x] = y,解释为一个最多容纳 x + 1 只兔子的阵营，其中 y 个兔子回答了 x。y == 1 表示该阵营第一次出现；当 y == x + 1 时，表示该阵营已经满了，后续再有兔子回答 x 的时候，已经是另一阵营的兔子了！！！只需要在 y == 1（出现新阵营的时候）收集答案，这样就就能避免漏算和重复计算
        """
        mp = defaultdict(int)
        ans = 0
        for x in answers:
            mp[x] += 1
            if mp[x] > x + 1:
                mp[x] = 1
            if mp[x] == 1:
                # 新的阵营，收集答案
                ans += x + 1
            
        return ans

# 5
# answers = [1,1,2]

# 11
# answers = [10,10,10]

# 5
answers = [1,0,1,0,0]
r = Solution().numRabbits(answers)
print(r)
