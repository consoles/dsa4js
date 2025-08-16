class Solution:
    def maximum69Number (self, num: int) -> int:
        """
        从左到右扫描，把第一个碰到的 6 变为 9 即可。
        """
        # 1234 -> [4, 3, 2, 1]
        nums_reversed = []
        while num > 0:
            nums_reversed.append(num % 10)
            num //= 10
        # 可以发现最高位在最后的位置，因此我们从数组的最后一个位置找到开头，如果发现 6 就变为 9
        for i in range(len(nums_reversed) - 1, -1, -1):
            if nums_reversed[i] == 6:
                nums_reversed[i] = 9
                break
        # 通过数组还原
        # [4, 3, 2, 1] -> 1234: 4*10^0 + 3*10^1 + 2*10^2 + 1*10^3
        res = 0
        for i in range(len(nums_reversed)):
            res += nums_reversed[i] * (10 ** i)
        return res
    
    def maximum69Number2 (self, num: int) -> int:
        """
        把字符串转化为字符数组，判断字符数组
        """
        s = list(str(num))
        for i in range(len(s)):
            if s[i] == '6':
                s[i] = '9'
                break
        return int(''.join(s))    

# 9969
# num = 9669

# 9999
# num = 9996

# 9999
num = 9999
r = Solution().maximum69Number(num) 
print(r)    