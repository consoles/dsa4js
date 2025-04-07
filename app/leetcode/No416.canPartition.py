from typing import List
from functools import cache

class Solution:
    def canPartition(self, nums: List[int]) -> bool:
        # 暴力法
        # 尝试将 nums 分隔成 2 个子集，比较两个子集的和
        n = len(nums)
        def make_partition(index: int, nums1: List[int], nums2: List[int]):
            """
            nums1 和 nums2 分别表示 子集1 和 子集2
            当前元素要么属于 子集1 要么属于子集 2 
            """
            if index == n:
                yield nums1.copy(), nums2.copy()
                return
            
            v = nums[index]
            index += 1
            # 当前元素属于 nums1 ?
            nums1.append(v)
            yield from make_partition(index, nums1, nums2)
            nums1.pop()
            # 当前元素属于 nums2 ?
            nums2.append(v)
            yield from make_partition(index, nums1, nums2)
            nums2.pop()

        g = make_partition(0, [], [])
        for p in g:
            nums1, nums2 = p
            if sum(nums1) == sum(nums2):
                return True
        return False    

    def canPartition2(self, nums: List[int]) -> bool:
        """
        涉及到子集的暴力解法都是 O(2^N)

        这个问题可以转化为：给定一个只包含正整数的非空数组，判断能否从数组中选出一些数字，使得这些数字的和等于 sum / 2

        对所有元素进行求和，计算其中一个子集的和，如果能等于 sum / 2 则一定有解，只需要尝试生成左边部分的子集和即可
        """
        n = len(nums)
        total_sum = sum(nums)
        if total_sum % 2 != 0:
            return False
        
        target_sum = total_sum / 2

        @cache
        def can_partition(cur_sum: int, index: int) -> bool:
            if cur_sum == target_sum:
                return True
            if cur_sum > target_sum:
                return False
            if index >= n:
                return False
            # 选当前元素
            ok = can_partition(cur_sum + nums[index], index + 1)
            if ok:
                return True
            # 不选当前元素
            return can_partition(cur_sum, index + 1)
        return can_partition(0, 0)
    
    def canPartition3(self, nums: List[int]) -> bool:
        """
        0-1 背包
        选取的数字的和恰好等于整个数组的元素和的一半

        dp[i][j] 表示从前 i 个元素中选取若干个数，其和是否等于 j
        i 的范围 1-n
        j 的范围 0-target (target = sum / 2)

        初始化：
        dp[0][0] = true: 前 0 个元素的和为 0
        dp[0][j] = false (j > 0): 前 0 个元素无法组成任何正数的和
        dp[i][0] = true: 和为 0 的子集总是存在的（不选取任何元素）
        """
        total_sum = sum(nums)
        if total_sum % 2 != 0:
            return False
        
        target = total_sum // 2
        n = len(nums)
        dp = [[False] * (target + 1) for _ in range(n + 1)]
        dp[0][0] = False
        for i in range(n):
            dp[i][0] = True

        for i in range(1, n + 1):
            num = nums[i-1]
            for j in range(target + 1):
                if num > j:
                    # 当前元素比目标大，不能选当前元素（选了会超出）
                    # 此时 dp[i][j] 取决于不选 num 的状态
                    dp[i][j] = dp[i-1][j]
                else:
                    # 当前元素 num 可以不选，也可以选
                    dp[i][j] = dp[i-1][j] or dp[i-1][j - num]
        return dp[n][target]

# True
# nums = [1,5,11,5]

# False
# nums = [1,2,3,5]

nums = [100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,99,97]
r = Solution().canPartition3(nums)
print(r)
