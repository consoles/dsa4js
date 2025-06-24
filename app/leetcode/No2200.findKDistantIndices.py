from typing import List

class Solution:
    def findKDistantIndices(self, nums: List[int], key: int, k: int) -> List[int]:
        # 先找出所以等于 key 的索引，该索引前后 k 的步长是其有效范围
        # 这个方法超时了
        index_list = [i for i, num in enumerate(nums) if num == key]
        res = []
        for index in index_list:
            for i in range(max(0, index - k), min(len(nums), index + k + 1)):
                if i not in res:
                    res.append(i)
        return res

    def findKDistantIndices2(self, nums: List[int], key: int, k: int) -> List[int]:
        """
        枚举
        """
        res = []
        n = len(nums)
        for i in range(n):
            for j in range(n):
                if nums[j] == key and abs(i - j) <= k:
                    res.append(i)
                    break # 提前结束内层循环，防止重复添加
        return res

    def findKDistantIndices3(self, nums: List[int], key: int, k: int) -> List[int]:
        """
        方法1的优化版本
        """
        res = []
        r = 0 # 未被判断过的最小下标
        n = len(nums)
        for j in range(n):
            if nums[j] == key:
                l = max(r, j - k)
                r = min(n - 1, j + k) + 1
                for i in range(l, r):
                    res.append(i)
        return res

# [1,2,3,4,5,6]
# nums = [3,4,9,1,3,9,5]
# key = 9
# k = 1
# r = Solution().findKDistantIndices(nums, key, k)

# [0, 1, 2, 3, 4]
nums = [2,2,2,2,2]
key = 2
k = 2
r = Solution().findKDistantIndices(nums, key, k)
print(r)
