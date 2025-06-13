from typing import List

class Solution:
    def minimizeMax(self, nums: List[int], p: int) -> int:
        def check(diff: int) -> bool:
            """
            给定一个最大允许的差值 diff，能否在 nums 中找到 p 对相邻的数，使得它们的差 <= diff
            """
            cnt = i = 0
            while i < len(nums) - 1:
                if nums[i + 1] - nums[i] <= diff:
                    cnt += 1
                    i += 2
                else:
                    i += 1
            return cnt >= p

        # 将数组排序，这样相邻元素之间的差值最小，更容易构造合法的配对
        nums.sort()
        # 最小差值是 0，最大差值就是 数组中的最大数减去最小数
        left = 0
        right = nums[-1] - nums[0]
        while left < right:
            mid = (left + right) // 2
            if check(mid):
                right = mid
            else:
                left = mid + 1
        return left

# 1
# nums = [10,1,2,7,1,3]
# p = 2

# 0
nums = [4,2,1,2]
p = 1
r = Solution().minimizeMax(nums, p)
print(r)
