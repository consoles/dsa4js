from typing import List

class Solution:
    def sortColors(self, nums: List[int]) -> None:
        """
        Do not return anything, modify nums in-place instead.
        """
        # 计数排序
        # counter[0] 记录0的个数
        # counter[1] 记录1的个数
        # counter[2] 记录2的个数
        counter = [0] * 3
        for num in nums:
            counter[num] += 1
        i = 0
        for j in range(3):
            for _ in range(counter[j]):
                nums[i] = j
                i += 1

    def sortColors2(self, nums: List[int]) -> None:
        """
        三指针：
        i 元素为 0 的右边界，初始为 -1
        j 元素为 2 的左边界，初始为 n
        k 当前遍历元素，初始为 0

        当 k < j 时：
        1. nums[k] == 0, nums[k] 和 nums[i+1] 交换，i += 1，k += 1
        2. nums[k] = 2, nums[k] 和 nums[j-1] 交换，j -= 1
        3. nums[k] == 1, k += 1
        
        遍历结束后数组元素分为了 3 个部分 [0, i], [i+1,j-1], [j,n-1] 3 个部分的元素恰好都是 0，1，2
        """
        i, j, k = -1, len(nums), 0
        while k < j:
            if nums[k] == 0:
                i += 1
                nums[i], nums[k] = nums[k], nums[i]
                k += 1
            elif nums[k] == 2:
                j -= 1
                nums[j], nums[k] = nums[k], nums[j]
            else:
                k += 1

nums = [2,0,2,1,1,0]
Solution().sortColors(nums) 
print(nums)