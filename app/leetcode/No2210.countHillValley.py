from typing import List

class Solution:
    def countHillValley(self, nums: List[int]) -> int:
        """
        去重： nums=[1,1,2,2,1,1] -> nums=[1,2,1]
        连续相同的元素只保留 1 个
        去重后只需要考虑 2 种情况 
        峰: nums[i] > nums[i-1] and nums[i] > nums[i+1]
        谷: nums[i] < nums[i-1] and nums[i] < nums[i+1]
        """
        k = 1
        for i in range(1, len(nums)):
            if nums[i] != nums[i-1]:
                nums[k] = nums[i]
                k += 1
        # [0, k) 就是原来数组中去重后的结果 
        # 波峰波谷元素是 [1, k-2]
        cnt = 0
        for i in range(1, k-1):
            # if nums[i] > nums[i-1] and nums[i] > nums[i+1]:
            #     cnt += 1
            # elif nums[i] < nums[i-1] and nums[i] < nums[i+1]:
            #     cnt += 1

            # 小技巧: 峰和谷 2 种情况可以合并
            # 要么 nums[i] > nums[i-1] 和 nums[i] > nums[i+1] 都是 true(峰)
            # 要么 nums[i] > nums[i-1] 和 nums[i] > nums[i+1] 都是 false(谷)
            # 即: nums[i] > nums[i-1] 和 nums[i] > nums[i+1] 的布尔值相同
            if (nums[i] > nums[i-1]) == (nums[i] > nums[i+1]):
                cnt += 1
        return cnt
    
    def countHillValley2(self, nums: List[int]) -> int:
        """
        上面的做法是 2 次遍历，可以把 2 个循环合并成 1 个循环，做到 1 次遍历
        定义 prev 为上一个连续相同元素段（段）中的元素，cur 为当前段的元素，next 为下一个段中的元素，上面的条件:
        (nums[i] > nums[i-1]) == (nums[i] > nums[i+1]) 可以写成: (cur > prev) == (cur > next)
        """
        ans = 0
        prev = nums[0]
        for i in range(1, len(nums) - 1):
            cur = nums[i]
            next = nums[i + 1]
            if cur == next:
                continue
            if prev == cur:
                continue
            if (cur > prev) == (cur > next):
                ans += 1
            prev = cur
        return ans    

# 3
nums = [2,4,1,1,6,5]

# 0
# nums = [6,6,5,5,4,1]
r = Solution().countHillValley2(nums)
print(r)            
