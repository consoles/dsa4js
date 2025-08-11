class Solution:
    def numberOfWays(self, n: int, x: int) -> int:
        """
        DP: 典型的 0-1 背包问题，将 n 看成背包容量, [1^x, 2^x, ...] 看成物品。
        我们设:dp[i][j] 表示前 i 个数字中选择不同数字的 x 次幂之和为 j 的方案数。
        我们从 1 开始枚举所有整数，假设枚举当前的数字为 i ，幂次和为 j，此时有以下推论：
        1. 不选择当前数字 i。等价于从前 i - 1 个数字中选择不同数字使得所选数字的 x 次幂和为 j。dp[i][j] = dp[i - 1][j]
        2. 选择当前数字 i，则 i 的 x 次幂为 i^x，此时需要满足 i^x <=j,需要前 i-1 个数字中选择不同的数字使得 x 次幂之和为 j - i^x。dp[i][j] = dp[i - 1][j - i^x] 
        可以得到状态转移方程：
        1. dp[i][j] = dp[i - 1][j], j < i^x
        2. dp[i][j] = dp[i - 1][j - i^x], j >= i^x
        """
        mod = 10 ** 9 + 7
        dp = [[0] * (n + 1) for _ in range(n + 1)]
        dp[0][0] = 1
        for i in range(1, n + 1):
            val = i ** x
            for j in range(n + 1):
                dp[i][j] = dp[i - 1][j]
                if j >= val:
                    dp[i][j] = (dp[i][j] + dp[i - 1][j - val]) % mod
        return dp[n][n]  

# 1: 3^2 + 1^2
# n = 10
# x = 2 

# 2: 3^1 + 2^1, 4^1
n = 4
x = 1             
r = Solution().numberOfWays(n, x)
print(r)
