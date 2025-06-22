from typing import List

class Solution:
    def divideString(self, s: str, k: int, fill: str) -> List[str]:
        """
        按照题目按部就班进行
        """
        ans = []
        for i in range(0, len(s), k):
            ans.append(s[i:i+k])
        # 判断最后一个元素是否需要填充
        fill_count = k - len(ans[-1])
        if fill_count > 0:
            ans[-1] += fill * fill_count 
        return ans

s = "abcdefghi"
k = 3
fill = "x"
r = Solution().divideString(s, k, fill)
print(r)
