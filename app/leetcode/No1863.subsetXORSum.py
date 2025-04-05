from typing import List

class Solution:
    def subsetXORSum(self, nums: List[int]) -> int:
        # 求出所有的子集，然后求和
        # n 个元素的全部子集从 空集到全集的 总共的可能数是 2^N
        n = len(nums)
        sub_sets = []
        def dfs(index: int, res: List[int]):
            if index == n:
                sub_sets.append(res[::])
                return
            # 针对每个元素可能取当前元素，也有可能不取
            dfs(index + 1, res)
            res.append(nums[index])
            dfs(index + 1, res)
            res.pop()
        dfs(0, [])
        xor_sum = 0
        for res in sub_sets:
            r = 0
            for num in res:
                r ^= num
            xor_sum += r    
        return xor_sum
    
    def subsetXORSum2(self, nums: List[int]) -> int:
        # 方法 1 先是枚举了全部的子集，然后遍历每个子集求 xor 的和
        # 可以在求解子集的过程中完成 xor sum 的计算
        n = len(nums)
        r = 0
        def dfs(index: int, val: int):
            nonlocal r # 注意不声明这一句的话会产生新的变量，无法引用外部的 r
            if index == n:
                r += val
                return
            # 考虑当前数字
            dfs(index + 1, val ^ nums[index])
            # 不考虑当前数字
            dfs(index + 1, val)
        dfs(0, 0)    
        return r

    def subsetXORSum3(self, nums: List[int]) -> int:
        total = 0
        n = len(nums)
        # 所有子集的数量 0 - 2^n-1 一共 2^n 个
        # 例如 n = 2 时,mask 的取值为 0b00(0),0b01(1),0b10(2),0b11(3)
        # 0b01 表示选中第 0 个元素(nums[0])
        # 0b11 表示选中第 0 个和第 1 个元素(nums[0] 和 nums[1]) 
        for mask in range(1 << n):
            # 0 ^ x = x
            xor = 0
            for i in range(n):
                # 1 << i 生成一个只有第 i 位是 1 的二进制数
                # 检查 mask 的第 i 位是否是 1
                # 如果是 1 表示选中 nums[i]
                if mask & (1 << i):
                    xor ^= nums[i]
            total += xor
        return total            


# 6
# nums = [1,3]

# 28
# nums = [5,1,6]

# 480
nums = [3,4,5,6,7,8]
r = Solution().subsetXORSum3(nums)
print(r)
