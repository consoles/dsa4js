from typing import List

class Solution:
    def countSubarrays(self, nums: List[int], k: int) -> int:
        # 滑动窗口
        # O(N^2)
        max_num = max(nums)
        n = len(nums)
        ans = 0
        for left in range(n):
            max_num_in_window = 0
            for right in range(left, n):
                num = nums[right]
                if num == max_num:
                    max_num_in_window += 1
                if max_num_in_window == k:
                    # [left, right] 满足条件则
                    # [left, right + 1], [left, right + 2] ... [left, n - 1] 都满足
                    # 因为更长的数组不会对现有结果产生相反共享
                    ans += (n - right)
                    break
        return ans
    
    def countSubarrays2(self, nums: List[int], k: int) -> int:
        """
        方法 1 在数据规模较大的时候会超时，通常在 leetcode 中，数据规模比较大(10^5)的时候，要求算法在 O(N) 或者 (N*logN) 完成
        """
        max_num = max(nums)
        n = len(nums)
        left = max_num_cnt = ans = 0

        for right in range(n):
            if nums[right] == max_num:
                max_num_cnt += 1
            # 当窗口至少包含 k 个最大值的时候，left 尝试向右移动(收缩窗口)，直到窗口中不满足 k 个最大值
            while max_num_cnt == k:
                if nums[left] == max_num:
                    max_num_cnt -= 1
                left += 1
                # 最后 left 跳出循环的时候刚好 [left, right] 不满足 k 个最大值
                # 左端点可以取到 [0, left - 1] 一共 left 个元素
            ans += left 
        return ans

# 6
nums = [1,3,2,3,3]
k = 2     

# 0
# nums = [1,4,2,1]
# k = 3       
r = Solution().countSubarrays2(nums, k)
print(r)
