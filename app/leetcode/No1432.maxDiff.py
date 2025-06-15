class Solution:
    def maxDiff(self, num: int) -> int:
        """
        x 的取值范围是 [0, 9]
        y 的取值范围是 [0, 9]
        使用二重循环完成所有的替换
        """

        def change(x, y: int) -> str:
            """
            将 num 中所有的 x 替换为 y，返回替换之后的字符串
            """
            return str(num).replace(str(x), str(y))

        min_num = max_num = num
        for x in range(10):
            for y in range(10):
                s = change(x, y)
                # 不能以 0 开头
                if s[0] == '0':
                    continue
                new_num = int(s)
                min_num = min(min_num, new_num)
                max_num = max(max_num, new_num)
        return max_num - min_num

    def maxDiff2(self, num: int) -> int:    
        """
        贪心：最大值就是找到一个最高位，将其修改为 9，最小值就是找到一个最高位，将其修改为 0
        最大数：从高到低依次枚举，找到第一个不是 9 的数，将其修改为 9
        最小数：从高到低依次枚举，找到第一个不是 0 的数，将其修改为 0，但是这种思路会出现前导 0 的情况，需要特殊处理
            1. 如果枚举的是最高位，那么只能将其替换为 1，否则就会有前导 0 了
            2. 如果当前的数字和最高位的数字相等，那么直接跳过这个数位，因为在我们枚举最高位的时候已经处理过这个数字了。既然在枚举最高位的时候没有进行替换，那么这个数字一定是 1，由于前导 0 的限制我们也不能将其替换为 0，因此可以直接跳过
        """
        min_num_str = max_num_str = num_str = str(num)
        # 计算最大值：找到最高位，将其替换为 9
        for digit in num_str:
            if digit != '9':
                max_num_str = max_num_str.replace(digit, '9')
                break
        # 计算最小值：将最高位替换为 1，或者找到第一个和最高位不想等的数字替换为 0
        for i, digit in enumerate(num_str):
            if i == 0:
                if digit != '1':
                    min_num_str = num_str.replace(digit, '1')
                    break
            else:
                if digit != '0' and digit != min_num_str[0]:
                    min_num_str = num_str.replace(digit, '0')
                    break
        return int(max_num_str) - int(min_num_str)
        
# 888
# num = 555

# 8
# num = 9

# 820000
# num = 123456

# 80000
num = 10000
r = Solution().maxDiff2(num)
print(r)            