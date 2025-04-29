from typing import List
from collections import defaultdict

class Solution:
    def countCompleteSubarrays(self, nums: List[int]) -> int:
        # 统计不同元素的个数
        distinct_count = len(set(nums))
        n = len(nums)
        ans = 0
        # 滑动窗口
        for left in range(n):
            window_counts = defaultdict(int)
            distinct_in_window = 0
            for right in range(left, n):
                num = nums[right]
                # 窗口内首次出现该数字
                if window_counts[num] == 0:
                    distinct_in_window += 1
                window_counts[num] += 1

                # 如果窗口内包含不同的元素，则统计答案
                if distinct_in_window == distinct_count:
                    # 如果当前窗口 [left, right] 已经包含了所有不同元素
                    # 则 [left, right + 1], [left, right + 2] ... [left, n - 1] 都满足条件
                    # 因为右边界扩大不会减少已有的数字种类
                    # 特别注意：break 掉，让 left 继续向右移动
                    ans += (n - right)
                    break
        return ans            

# 4
nums = [1,3,1,2,2]
r = Solution().countCompleteSubarrays(nums)
print(r)
