from typing import List
from collections import defaultdict

class Solution:
    def countGood(self, nums: List[int], k: int) -> int:
        def check(i, j: int) -> bool:
            if i >= j: 
                return False
            end = j + 1
            cnt = 0
            for x in range(i, end):
                for y in range(x + 1, end):
                    if nums[x] == nums[y]:
                        cnt += 1
            return cnt >= k

        # 暴力法
        # 枚举子数组的起点和终点 O(N^2)
        # 判断子数组是否是“好”的 O(N^2)
        # 复杂度 O(N^4) 基本是超时
        n = len(nums)
        count = 0
        for i in range(n):
            for j in range(i + 1, n):
                if check(i, j): 
                    count += 1
        return count
    
    def countGood2(self, nums: List[int], k: int) -> int:
        """
        滑动窗口:O(N)
        核心思路：
        1. 如果窗口中有 c 个元素 x，再进来一个 x，会新增 c 个相等数对
        2. 如果窗口中有 c 个元素 x，去掉一个 x，会减少 c - 1 个相等数对

        用一个哈希表 count_map 维护子数组（窗口）中每个元素的出现次数，使用 cur_pair_count 维护窗口中有几个相等数对
        """
        n = len(nums)
        count_map = defaultdict(int) # 统计每个数字出现的次数
        left = 0
        good_count = 0
        cur_pair_count = 0 # 当前窗口中有几个相等数对

        # 从小到大枚举子数组的右端点 right。把 num = nums[right] 加入窗口，那么窗口中有 count_map[num] 个数和 num 相等
        # 所以 cur_pair_count += count_map[num]，然后再把 num 的出现次数加 1
        for right in range(n):
            num = nums[right]
            # 注意这两句话的顺序不能变
            cur_pair_count += count_map[num] # 前数字与已有的相同数字形成的对数
            count_map[num] += 1

            # 说明子数组是符合要求的，右移左端点 left，先把 count_map[nums[left]] 减 1，再把 cur_pair_count 减少 count_map[nums[left]]
            # 循环结束后，[left, right] 这个子数组是不符合要求的，但是在 left 指针右移之前（[left - 1, right]）这个子数组是符合要求的
            # 因为子数组长度越长，越能满足要求，因此 [left-2,right],[left-3,right] ... [0, right] 都是满足要求的
            # 即当右端点为 right 的时候, 左端点为 0, 1, 2, ... left - 1 的子数组都是满足要求的，一共有 left 个满足要求的子数组
            while cur_pair_count >= k:
                count_map[nums[left]] -= 1
                cur_pair_count -= count_map[nums[left]]
                left += 1
            good_count += left # 以当前右指针为终点的所有子数组都是好子数组
        return good_count

# 1
# nums = [1,1,1,1,1]
# k = 10

# 4
# nums = [3,1,4,3,2,2,4]
# k = 2

# 7044 暴力法超时的测试用例
nums = [4,9,1,6,4,6,4,3,6,7,9,3,6,5,8,6,1,2,2,8,7,10,8,2,10,4,1,9,1,6,1,9,7,10,5,10,7,6,1,4,6,9,7,6,2,5,8,10,6,7,9,8,3,6,9,7,6,10,3,6,8,2,6,4,9,1,6,8,9,9,8,6,2,5,1,10,2,10,9,1,5,6,10,2,2,1,6,3,2,8,3,1,9,10,3,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]
k = 1442
print(Solution().countGood2(nums, k))
