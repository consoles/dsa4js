class Solution:
    def hammingWeight(self, n: int) -> int:
        # 转化成二进制，看看有多少 1
        c = 0
        while n != 0:
            v = n % 2
            if v == 1:
                c += 1
            n //= 2
        return c
    
    def hammingWeight2(self, n: int) -> int:
        c = 0
        while n != 0:
            # v = n & 1
            # if v == 1:
            #     c += 1

            # 优化
            c += (n & 1)
            n >>= 1 
        return c 

    def hammingWeight3(self, n: int) -> int:
        # 利用 n & (n - 1) 快速去掉最低位的 1，直到 n 变为 0
        c = 0
        while n != 0:
            n &= (n - 1)
            c += 1
        return c  

    def hammingWeight4(self, n: int) -> int: 
        return bin(n).count('1') 
    
    def hammingWeight5(self, n: int) -> int: 
        c = 0
        mask = 1
        for i in range(32):
            # 如果 n 的当前位是 1，则 n & mask 不为 0
            if n & mask != 0:
                c += 1
            mask <<= 1
        return c    

# 3
# n = 11 

# 1
# n = 128

# 30
n = 2147483645
r = Solution().hammingWeight5(n)
print(r)
