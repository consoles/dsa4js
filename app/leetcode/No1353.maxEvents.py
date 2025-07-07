from typing import List
import heapq

class Solution:
    def maxEvents(self, events: List[List[int]]) -> int:
        """
        贪心：
        第一天参加结束时间最早的会议，随后的每一天就是解决 n-1 天的子问题
        """
        mx = max(e[1] for e in events)
        # 按照开始时间进行分组
        # [1,1], [1,2], [1,3] => 1: [1,2,3]
        # 开始时间 -> 结束时间列表
        groups = [ [] for _ in range(mx + 1) ]
        for s, e in events:
            groups[s].append(e)

        ans = 0
        h = []
        for i, g in enumerate(groups):
            # 删除过期会议
            while h and h[0] < i:
                heapq.heappop(h)
            # 新增可以参加的会议
            for end_day in g:
                heapq.heappush(h, end_day)
            # 参加一个结束时间最早的会议
            if h:
                ans += 1
                heapq.heappop(h)
        return ans 

# 3
# events = [[1,2],[2,3],[3,4]]

# 4
events= [[1,2],[2,3],[3,4],[1,2]]
r = Solution().maxEvents(events)
print(r)