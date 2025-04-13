class Solution:
    def countGoodNumbers(self, n: int) -> int:
        """
        题目说数据规模大，并且解的个数非常多，暴力法肯定过不了

        长度为 n 的数字字符串有 a = ceil(n / 2) 个偶数下标，每个下标可能有 0,2,4,6,8 5 种偶数, 即 5^a
        有 b = floor(n / 2) 个奇数下标，每个下标可能有 2,3,5,7 一共 4 种质数，即 4^b
        奇数下标和偶数下标互相独立，根据乘法原理，有总方案数 5^a * 4^b

        python 自带的 pow(base, exp, mod) 可以快速计算 base^exp % mod，它的优点是使用了快速幂算法，能高效计算大数的幂模
        (n+1) // 2 和 ceil(n / 2) 是等价的
        """
        MOD = 1_000_000_007
        return pow(5, (n + 1) // 2, MOD) * pow(4, n // 2, MOD) % MOD
           
# 5
n = 1
r = Solution().countGoodNumbers(n)
print(r)
