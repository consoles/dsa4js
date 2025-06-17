from typing import List

class Solution:
    def divideArray(self, nums: List[int], k: int) -> List[List[int]]:
        """
        贪心：排序，相邻的 3 个数放在一组, nums[i], nums[i + 1], nums[i + 2]，只需要判断 nums[i + 2] 和 nums[i] 的差值就行了
        """
        ans = []
        nums.sort()
        for i in range(0, len(nums), 3):
            if nums[i+2] - nums[i] > k:
                return []
            ans.append(nums[i:i+3])
        return ans

nums = [1,3,4,8,7,9,3,5,1]
k = 2
r = Solution().divideArray(nums, k)
print(r)
