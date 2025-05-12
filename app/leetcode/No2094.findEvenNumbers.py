from typing import List
from collections import Counter

class Solution:
    def findEvenNumbers(self, digits: List[int]) -> List[int]:
        """
        因为只有 3 个数字，因此我们可以直接 3 重循环枚举出所有的可能
        复杂度 O(n^3)
        """
        nums = set()
        n = len(digits)
        for i in range(n):
            for j in range(n):
                for k in range(n):
                    if i == j or j == k or i == k:
                        continue
                    num = digits[i] * 100 + digits[j] * 10 + digits[k]
                    # 如果含有前导 0，那么一定小于 100
                    if num >= 100 and num % 2 == 0:
                        nums.add(num)
        return sorted(list(nums))
    
    def findEvenNumbers2(self, digits: List[int]) -> List[int]:
        """
        通过分析我们可以知道：3 位偶数的个数十分有限，我们只需要判断 [100,999] 范围内的偶数能否由 digits 构成即可

        针对任意偶数，我们统计每个数字出现的次数 freq
        针对 digits 数组，我们也统计每个数字出现的次数 freq_digits

        判断 freq 中的每个元素出现的次数都能在 freq_digits 中找到即可
        """
        res = []
        freq_digits = Counter(digits)
        for num in range(100, 1000, 2):
            freq = Counter([int(d) for d in str(num)])
            if all(freq[d] <= freq_digits[d] for d in freq):
                res.append(num)
        return res        
    
    def findEvenNumbers3(self, digits: List[int]) -> List[int]:
        """
        回溯：百位不为 0，十位随便填，个位是偶数即可
        """
        cnt = [0] * 10
        for d in digits:
            cnt[d] += 1

        ans = []
        def dfs(i: int, x: int):
            """
            i = 0, 百位；i = 1, 十位；i = 2, 个位
            x 表示正在构造的数字
            """
            if i == 3:
                ans.append(x)
                return
            for d, c in enumerate(cnt):
                if c > 0 and (i == 0 and d > 0 or i == 1 or i == 2 and d % 2 == 0):
                    cnt[d] -= 1 # 消耗一个数字 d
                    dfs(i + 1, x * 10 + d)
                    cnt[d] += 1 # 复原
        dfs(0, 0)
        return ans            

# [102,120,130,132,210,230,302,310,312,320]
digits = [2,1,3,0]
r = Solution().findEvenNumbers(digits)    
print(r)
