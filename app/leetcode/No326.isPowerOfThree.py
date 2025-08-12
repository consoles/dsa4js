class Solution:
    def isPowerOfThree(self, n: int) -> bool:
        """
        不断除以 3，如果过程中不能整除，则返回 False，否则判断最后能否除到 1
        """
        if n <= 0: return False
        while n >= 3:
            if n % 3 != 0:
                return False
            n //= 3
        return n == 1

    def isPowerOfThree2(self, n: int) -> bool:
        """
        在 32 位有符号整数的范围内，最大的 3 的幂为 3^19 = 1162261467
        只需要判断 n 是 3^19 的约数即可
        """
        return n > 0 and 1162261467 % n == 0

# True
n = 27

# False
# n = 0

# True
# n = 9

# False
# n = 45
r = Solution().isPowerOfThree(n)
print(r)
