from typing import List

class Solution:
    def maxSubArray(self, nums: List[int]) -> int:
        # 暴力法超时
        max_sum = -float('inf')
        l = len(nums)
        for i in range(0, l):
            for j in range(i, l):
                arr = nums[i:j+1]
                s = sum(arr)
                if (s > max_sum):
                    max_sum = s
        return max_sum
    
    def maxSubArray2(self, nums: List[int]) -> int:
        # dp[i] 为以 nums[i] 结尾的最大子数组和
        # 如果 dp[i-1] > 0，则 dp[i] = dp[i - 1] + nums[i]，因为前面的和是正的，可以增大当前和
        # 否则 dp[i] = nums[i]，前面的和是负的，不如从当前元素重新开始
        n = len(nums)
        dp = [0] * n
        dp[0] = nums[0]
        max_sum = dp[0]
        for i in range(1, n):
            if dp[i-1] > 0:
                dp[i] = dp[i-1] + nums[i]
            else:
                dp[i] = nums[i]

            if dp[i] > max_sum:
                max_sum = dp[i]
        return max_sum
    
    def maxSubArray3(self, nums: List[int]) -> int:
        # 方法 2 的 dp 数组可以进行压缩
        max_sum = cur_sum = nums[0]
        for num in nums[1:]:
            if cur_sum > 0:
                cur_sum += num
            else:
                cur_sum = num
            if cur_sum > max_sum:
                max_sum = cur_sum
        return max_sum

if __name__ == '__main__':
    nums = [-2,1,-3,4,-1,2,1,-5,4]
    print(Solution().maxSubArray3(nums))
