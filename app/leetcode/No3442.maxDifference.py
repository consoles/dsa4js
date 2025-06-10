from  collections import defaultdict

class Solution:
    def maxDifference(self, s: str) -> int:
        # 记录每个字符出现的次数
        counter = defaultdict(int)
        for c in s:
            counter[c] += 1
        odd_count_max = 0
        even_count_min = float('inf')
        for c in counter:
            count = counter[c]
            if count % 2 != 0:
                if count > odd_count_max:
                    odd_count_max = count
            else:
                if count < even_count_min:
                    even_count_min = count
        return odd_count_max - even_count_min

# 3
# s = "aaaaabbc"

# 1
s = "abcabcab"
r = Solution().maxDifference(s)
print(r)
