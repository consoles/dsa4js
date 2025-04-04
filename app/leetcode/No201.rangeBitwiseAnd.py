class Solution:
    def rangeBitwiseAnd(self, left: int, right: int) -> int:
        # 算法的正确性没问题，但是遇到 [600000000, 2147483647] 这种极端情况会超时
        res = left
        i = left + 1
        while i <= right:
            res &= i
            if res == 0:
                break
            i += 1
        return res

    def rangeBitwiseAnd2(self, left: int, right: int) -> int: 
        # 公共前缀法
        # 9  -> 1001
        # 10 -> 1010
        # 11 -> 1011
        # 12 -> 1100
        # 9 & 10 & 11 & 10 == 1 000
        # 9,10,11,12 的公共前缀是 1，后面补 3 个 0
        # 所有数字执行按位与运算的结果是所有对应二进制字符串的公共前缀再用零补上后面的剩余位

        # 1. 将两个数字不断向右移动，直到数字相等，即数字被缩减为它们的公共前缀
        # 2. 通过将公共前缀向左移动，将零添加到公共前缀的右边以获得最终结果
        shift = 0
        while left != right:
            left >>= 1
            right >>= 1
            shift += 1
        # 左移补 0
        return left << shift 

left = 600000000
right = 2147483647
r = Solution().rangeBitwiseAnd2(left, right)
print(r)
