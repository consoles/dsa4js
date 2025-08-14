class Solution:
    def checkPowersOfThree(self, n: int) -> bool:
        """
        进制转换：
        三进制数可能出现的数字为 0,1,2
        如果出现了 2 就表示 2*3^x 不满足题目的要求，转化成二进制智能出现 0 和 1
        0 * 3^x = 0, 1 * 3^x = 3^x
        """
        while n != 0:
            if n % 3 == 2:
                return False
            n //= 3
        return True

# True
# n = 12

# True
# n = 91

# False
n = 21
r = Solution().checkPowersOfThree(n)
print(r)
