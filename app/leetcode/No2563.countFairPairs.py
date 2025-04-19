from typing import List
import bisect

class Solution:
    def countFairPairs(self, nums: List[int], lower: int, upper: int) -> int:
        # 暴力法,很容易想到的 O(n^2)
        n = len(nums)
        c = 0
        for i in range(n - 1):
            for j in range(i + 1, n):
                if lower <= nums[i] + nums[j] <= upper:
                    c += 1
        return c
    
    def countFairPairs2(self, nums: List[int], lower: int, upper: int) -> int:
        """
        二分查找
        由于排序并不影响答案，可以先进行排序，这样就可以使用二分查找了
        排序后，枚举右边的 nums[j]，那么左边的 nums[i] 需要满足 0 <= i < j 以及 lower - nums[j] <= nums[i] <= upper - nums[j]

        计算 <= upper - nums[j] 的个数，减去 < lower - nums[j] 的个数，就是满足上述的元素的个数（此处可以联系前缀和来理解）
        由于 nums 是有序的，我们可以在 [0, j-1] 中进行二分查找：
        1. 找到 > upper - nums[j] 的第一个数，设其下标为 r，那么下标在 [0, r-1] 中的数都是 <= upper - nums[j] 的，这有 r 个。如果 [0,j-1] 中没有找到这样的数，那么二分查找的结果为 j。这以为着 [0,j-1] 中的数都是 <= upper - nums[j] 的，这有 j 个。
        2. 找到 >= lower - nums[j] 的第一个数，设其下标为 l，那么下标在 [0, l - 1] 中的数都是 < lower - nums[j] 的，这有 l 个。如果 [0, j-1] 中没有找到这样的数，那么二分结果就是 j，这意味着 [0, j - 1] 中的数都是 < lower - nums[j] 的，这有 j 个。
        3. 满足 lower - nums[j] <= nums[j] <= upper - nums[j] 的元素的个数是 r - l，加入答案。
        """
        nums.sort()
        count = 0
        for j, x in enumerate(nums):
            # 注意：这两个函数都是返回元素的插入位置左开右闭区间
            # 注意要在 [0, j-1] 中二分，因为题目要求 2 个下标 i < j
            r = bisect.bisect_right(nums, upper - x, 0, j)
            l = bisect.bisect_left(nums, lower - x, 0, j)
            count += r - l
        return count   

    def countFairPairs3(self, nums: List[int], lower: int, upper: int) -> int: 
        """
        相向三指针：
        随着 nums[j] 的变大，upper - nums[j] 都在变小，具有单调性，可以使用相向三指针 j,l,r 代替上面的二分查找

        1. 初始化 l = r = n
        2. 从左向右遍历排序后的 nums
        3. 找 > upper - nums[j] 的第一个数：如果 nums[r-1] > upper - nums[j],说明 r 太大了，可以继续减小。循环结束的时候 r 与 j 取最小值后，就是上述二分查找计算出来的 r
        4. 找 >= lower - nums[j] 的第一个数：如果 nums[l - 1] >= lower - nums[j] ，说明 l 太大了，可以继续减小。循环结束的时候 l 与 j 取最小值后，就是二分查找中计算出的 l
        """
        nums.sort()
        count = 0
        l = r = len(nums)
        for j, x in enumerate(nums):
            while r and nums[r - 1] > upper - x:
                r -= 1
            while l and nums[l - 1] >= lower - x:
                l -= 1
            count += min(r, j) - min(l, j)
        return count    

# 6
# nums = [0,1,7,4,4,5]
# lower = 3
# upper = 6                

# 1
nums = [1,7,9,2,5]
lower = 11
upper = 11
r = Solution().countFairPairs2(nums, lower, upper)
print(r)
