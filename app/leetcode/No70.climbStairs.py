class Solution:
    def climbStairs(self, n: int) -> int:
        if n == 1:
            return 1
        dp = [0] * (n + 1)
        dp[1] = 1
        dp[2] = 2
        # 要爬到 n ，要么从 n-1 爬 1 步上来，要么从 n-2 爬 2 步上来
        # f(n) = f(n-1) + f(n-2)
        for i in range(3, n + 1):
            dp[i] = dp[i-1] + dp[i-2]
        return dp[n]
    
    def climbStairs2(self, n: int) -> int:
        if n == 1:
            return 1
        last = 1
        cur = 2
        for i in range(3, n + 1):
            old_cur = cur
            cur = last + cur
            last = old_cur
        return cur    

n = 45
r = Solution().climbStairs(n)
print(r)
