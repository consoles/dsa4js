from typing import List

class Solution:
    def searchRange(self, nums: List[int], target: int) -> List[int]:
        # 类似 lfind 和 rfind
        # 暴力法，从前向后查找，再从后向前查找
        n = len(nums)
        res = [-1, -1]
        if n == 0:
            return res

        for i in range(n):
            if nums[i] == target:
                res[0] = i
                break
        for i in range(n-1, -1, -1):
            if nums[i] == target:
                res[1] = i
                break
        return res
    
    def searchRange2(self, nums: List[int], target: int) -> List[int]:
        # 解法 1 没有利用排序数组的特性
        res = [-1, -1]
        n = len(nums)
        if n == 0:
            return res
        
        # 先进行二分查找，找到目标值的索引，然后向左和向右进行线性扩散
        lo, hi = 0, n - 1
        index = -1
        while lo <= hi:
            mid = lo + (hi - lo) // 2
            v = nums[mid]
            if v == target:
                index = mid
                break
            if target > v:
                lo = mid + 1
            else:
                hi = mid - 1
        if index == -1:
            return res

        l = index
        while l >= 0 and nums[l] == v:
            res[0] = l
            l -= 1
        r = index
        while r < n and nums[r] == v:
            res[1] = r
            r += 1
        return res

    def searchRange3(self, nums: List[int], target: int) -> List[int]:                
        # 方法 2 改进了一些，但是在极端测试用例下会退化成 O(N)
        # 一个大数组所有元素都相同，需要找的元素刚好就在数组中
        # 可不可以一直使用二分呢？

        n = len(nums)
        res = [-1, -1]
        if n == 0:
            return res

        def findFirstIndex():
            lo, hi = 0, n - 1
            index = -1
            while lo <= hi:
                mid = lo + (hi - lo) // 2
                v = nums[mid]
                # 相等的时候继续尝试向左边探测
                if v == target:
                    index = mid
                    hi = mid - 1
                elif target > v:
                    lo = mid + 1
                else:
                    hi = mid - 1
            return index        

        def findLastIndex():
            lo, hi = 0, n - 1
            index = -1
            while lo <= hi:
                mid = lo + (hi - lo) // 2
                v = nums[mid]
                # 相等的时候继续尝试向右边探测
                if v == target:
                    index = mid
                    lo = mid + 1
                elif target > v:
                    lo = mid + 1
                else:
                    hi = mid - 1
            return index
        
        res[0] = findFirstIndex()
        res[1] = findLastIndex()
        return res


# [3,4]
# nums = [5,7,7,8,8,10]
# target = 8

# [-1,-1]
# nums = [5,7,7,8,8,10]
# target = 6

# [-1, -1]
# nums = []
# target = 0

nums = [2, 2]
target = 3

r = Solution().searchRange3(nums, target)
print(r)
