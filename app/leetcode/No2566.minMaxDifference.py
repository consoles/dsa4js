class Solution:
    def minMaxDifference(self, num: int) -> int:
        """
        生成最大值的时候尽量将高位数字替换为 9，生成最小值的时候尽量将高位数字替换为 9
        从左向右，找到第一个不是 9 的数字，将改数字替换为 9；找到第一个不是 0 的数字，将该数字替换为 0, 由于第一个数字一定不是 0，因此最小数字可以直接将第一位替换为 0   
        """
        s = str(num) # 计算最大值字符串
        t = s # 拷贝一份，用于计算最小值字符串
        pos = 0
        while pos < len(s) and s[pos] == '9':
            pos += 1
        if pos < len(s):
            s = s.replace(s[pos], '9')
        t = t.replace(t[0], '0')
        return int(s) - int(t)    

# 99009
num = 11891

# 99
# num = 90
r = Solution().minMaxDifference(num)
print(r)
        