class Solution:
    def isPowerOfFour(self, n: int) -> bool:
        """
        不断除以 4 直到 n = 1，如果中间发现余数不是 0，则返回 False
        """
        while n > 1:
            if n % 4 != 0:
                return False
            n //= 4
        return n == 1

# True
# n = 16

# False
# n = 5

# True
n = 1
r = Solution().isPowerOfFour(n)
print(r)
