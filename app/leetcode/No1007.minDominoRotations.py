from typing import List
import math

class Solution:
    def minDominoRotations(self, tops: List[int], bottoms: List[int]) -> int:
        """
        目标是让第一排或者第二排的所有数相同。因此第一个骨牌中的数要么是 tops[0] 要么是 bottoms[0]，计算这两种情况，取最小值即可
        """
        def minRotate(target: int) -> int:
            to_top = to_bottom = 0
            for x, y in zip(tops, bottoms):
                if x != target and y != target:
                    return math.inf
                if x != target:
                    to_top += 1 # 旋转第二排到第一排
                if y != target:
                    to_bottom += 1 # 旋转第一排到第二排
            return min(to_top, to_bottom)
        ans = min(minRotate(tops[0]), minRotate(bottoms[0]))
        if ans == math.inf:
            return -1
        return ans

# 2
# tops = [2,1,2,4,2,2]
# bottoms = [5,2,6,2,3,2]    

# -1
tops = [3,5,1,2,3]
bottoms = [3,6,3,3,4]          
r = Solution().minDominoRotations(tops, bottoms)
print(r)