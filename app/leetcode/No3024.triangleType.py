from typing import List

class Solution:
    def triangleType(self, nums: List[int]) -> str:
        '''
        排序，任意两边之和大于第三边，是三角形
        '''
        nums.sort()
        if nums[0] + nums[1] > nums[2] and nums[0] + nums[2] > nums[1] and nums[1] + nums[2] > nums[0]:
            if nums[0] == nums[1] and nums[1] == nums[2]:
                return 'equilateral'
            if nums[0] == nums[1] or nums[1] == nums[2]:
                return 'isosceles'
            return 'scalene'
        return 'none'
    
    def triangleType2(self, nums: List[int]) -> str:
        '''
        排序之后事情变得非常简单，天然有以下约束
        nums[0] <= nums[1] <= nums[2]
        '''
        nums.sort()
        if nums[0] + nums[1] <= nums[2]:
            return 'none'
        if nums[0] == nums[2]:
            return 'equilateral'
        if nums[0] == nums[1] or nums[1] == nums[2]:
            return 'isosceles'
        return 'scalene'

# equilateral   
# nums = [3,3,3]

# scalene
nums = [3,4,5]
r = Solution().triangleType(nums)
print(r)    