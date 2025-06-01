class Solution:
    def distributeCandies(self, n: int, limit: int) -> int:
        """
        枚举：
        总共 3 个小朋友
        如果第一个小朋友分了 x 个糖果，那么剩余的糖果数量为 n - x。
        此时存在 2 种情况：
        1. n - x > 2 * limit, 则至少有一个小朋友分到的糖果数量超过了 limit。不存在解
        2. 对于第二个小朋友最少需要分 max(0, n - x - limit)，才能保证第三个小朋友分的
           糖果数量不超过 limit。同时最多能拿到 min(limit, n - x) 个糖果。

        第一个小朋友本身获得的糖果数量 x 的范围是 [0, min(n, limit)]。   
        """
        ans = 0
        for x in range(min(n, limit) + 1):
            if n - x > 2 * limit:
                continue
            ans += min(limit, n - x) - max(0, n - x - limit) + 1
        return ans

# 3 
# n = 5 
# limit = 2

# 10
n = 3
limit = 3
r = Solution().distributeCandies(n, limit)
print(r)