from typing import List

class Solution:
    def search(self, nums: List[int], target: int) -> int:
        # 方法1：寻找数组中的最小值
        # 最小值如果是第一个元素，则整个数组严格递增
        # 否则，可以以最小值为边界划分为 2 个有序数组
        n = len(nums)
        min_v = float('inf')
        min_index = -1
        for i in range(n):
            num = nums[i]
            if num < min_v:
                min_v = num
                min_index = i

        def bst(lo: int,hi: int) -> int:
            if lo > hi:
                return -1
            mid = (lo + hi) // 2
            num = nums[mid]
            if num == target:
                return mid
            if target < num:
                return bst(lo, mid - 1)
            return bst(mid + 1, hi)        

        if min_index == 0:
            return bst(0, n - 1)

        # 两个有序区间 [0, min_index - 1], [min_index, n - 1]
        idx = bst(0, min_index - 1)
        if idx != -1:
            return idx
        return bst(min_index, n - 1)

    def search2(self, nums: List[int], target: int) -> int:
        # 局部有序数组的二分查找问题
        if not nums:
            return -1
        n = len(nums)
        l, r = 0, n - 1
        while l <= r:
            # 从中间位置进行划分，左右2部分一定有一部分是有序的
            mid = (l + r) // 2
            if nums[mid] == target:
                return mid
            # [0, mid] 有序
            if nums[0] <= nums[mid]:
                if nums[0] <= target < nums[mid]:
                    r = mid - 1
                else:
                    l = mid + 1
            # [mid+1, n-1] 有序 
            else:
                if nums[mid] < target <= nums[n - 1]:
                    l = mid + 1
                else:
                    r = mid - 1
        return -1                 

# nums = [4,5,6,7,0,1,2]
# target = 0

# nums = [4,5,6,7,0,1,2]
# target = 3

# nums = [1]
# target = 0

nums = [1,3,5]
target = 1
r = Solution().search(nums, target)
print(r)
