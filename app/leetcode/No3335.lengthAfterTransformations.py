class Solution:
    def lengthAfterTransformations(self, s: str, t: int) -> int:
        """
        首先想到的就是按部就班模拟
        """
        def transform(s: str) -> str:
            new_s = ''
            for c in s:
                if c == 'z':
                    new_s += 'ab'
                else:
                    # ord 函数，返回一个字符的 ASCII 码
                    new_s += chr(ord(c) + 1)
            return new_s

        while t != 0:
            s = transform(s)
            t -= 1
        return len(s) % (10 ** 9 + 7)
    
    def lengthAfterTransformations2(self, s: str, t: int) -> int:
        """
        题目说答案可能非常大，可能返回的字符串的长度需要对 10^9 + 7 取余，所以计算出字符串的长度几乎就是不可能的

        dp[i][j] 表示经过 i 次转换过后，字母表中第 j 个字母的个数。
        初始时 dp[0][j] 为字符串 s 中第 j 个字母的个数

        每次转换后，字母表中的第 j 个字母的个数可以通过以下的方式计算：
        z -> ab, a 只能由 z 转化，b 则由 a 和 z 共同转化

        dp[i][0] = dp[i-1][25]
        dp[i][1] = dp[i-1][0] + dp[i-1][25]
        dp[i][2] = dp[i-1][1]
        dp[i][3] = dp[i-1][2]
        ...
        dp[i][25] = dp[i-1][24]

        最后的答案为 dp[t][0] + dp[t][1] + ... + dp[t][25]
        """
        mod = 10 ** 9 + 7
        dp = [[0] * 26 for _ in range(t + 1)]
        for c in s:
            dp[0][ord(c) - ord('a')] += 1
        for i in range(1, t + 1):
            dp[i][0] = dp[i - 1][25]  
            dp[i][1] = dp[i - 1][0] + dp[i - 1][25]
            for j in range(2, 26):
                dp[i][j] = dp[i - 1][j - 1]
        return sum(dp[t]) % mod            

# 7
# s = "abcyy"
# t = 2

# 5
s = "azbk"
t = 1
r = Solution().lengthAfterTransformations(s, t)
print(r)         