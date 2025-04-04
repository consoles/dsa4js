class Solution:
    def reverseBits(self, n: int) -> int:
        # 模拟
        # 1. 数字 -> 二进制字符串
        # 2. 反转二进制字符串
        # 3. 计算反转之后的二进制字符串的十进制
        def int2bin_str(n: int) -> str:
            s = ''
            while n != 0:
                s = str(n % 2) + s
                n //= 2
            return s

        s = int2bin_str(n)
        if len(s) < 32:
            # 需要在前面补充 0
            pad_0_count = 32 - len(s)
            while pad_0_count > 0:
                s = '0' + s
                pad_0_count -= 1
        s_reverse = s[::-1]
        # 将二进制字符串转为 10 进制
        r = 0
        i = 31
        c = 0
        while i >= 0:
            x = int(s_reverse[i])
            r += x * 2 ** c
            c += 1
            i -= 1
        return r

    def reverseBits2(self, n: int) -> int:
        # 上面的方式还有优化的空间，使用 list<int> 保存二进制数，去掉 int <-> str 的转换开销
        # 这种方式去除了 int - string 的互相转换反而更慢了！！！
        nums = []
        while n != 0:
            v = n % 2
            n //= 2
            nums.append(v)

        nums = nums[::-1]    
        # 4 -> 100 每次我们将当前位是 append 的，存储的是 [0, 0, 1]
        if len(nums) < 32:
            pad_0_count = 32 - len(nums)
            while pad_0_count != 0:
                nums.insert(0,0)
                pad_0_count -= 1

        reversed_bin_nums = nums[::-1]
        i = 0
        r = 0
        while i <= 31:
            r += reversed_bin_nums[i] * 2 ** (31 - i)
            i += 1
        return r

    def reverseBits3(self, n: int) -> int:             
        res = 0
        for i in range(32):
            # n & 1 取最低位
            # res << 1 将已经反转的部分左移一位，腾出最低位给新位
            # | (n & 1) 将新位添加到 res 的最低位
            res = (res << 1) | (n & 1)
            # 丢弃已处理的最低位
            n >>= 1
        return res    
    
# n = 43261596 # 964176192
n = 4294967293 # 3221225471
r = Solution().reverseBits3(n)            
print(r)
