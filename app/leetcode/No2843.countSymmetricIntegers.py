class Solution:
    def countSymmetricIntegers(self, low: int, high: int) -> int:
        def check(x: int) -> bool:
            # 每次取个位数
            nums = []
            while x != 0:
                n = x % 10
                x //= 10
                nums.append(n)
            if len(nums) % 2 != 0:
                return False
            mid_index = len(nums) // 2
            sum_l = sum(nums[0:mid_index])
            sum_r = sum(nums[mid_index:])
            return sum_l == sum_r
        
        # 枚举 [low, high] 区间的数字
        c = 0
        for x in range(low, high + 1):
            if check(x):
                c += 1
        return c

    def countSymmetricIntegers2(self, low: int, high: int) -> int:    
        """
        题目规定了数据规模是在 [1,10000]，有 2 种合法情况：
        1. 2 位数中，例如 11, 22, 33, ... 这种很明显是 11 的倍数
        2. 4 位数则需要取千位，百位和 与 个位，十位和 比较
        """
        c = 0
        for x in range(low, high + 1):
            if x < 100 and x % 11 == 0:
                c += 1
            elif 1000 <= x < 10000:
                l_sum = x // 1000 + x % 1000 // 100
                r_sum = x % 100 // 10 + x % 10    
                if l_sum == r_sum:
                    c += 1
        return c

# 9
# low = 1
# high = 100

# 4
low = 1200
high = 1230
r = Solution().countSymmetricIntegers2(low, high)
print(r)
