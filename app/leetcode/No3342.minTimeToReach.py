from typing import List
import heapq

class Solution:
    def minTimeToReach(self, moveTime: List[List[int]]) -> int:
        """
        使用 Dijkstra 算法计算从矩阵左上角到右下角的最小时间
        设从起点 (0,0) 到达 (i,j) 的最短时间为 dis[i][j]
        那么从 (i,j) 走到相邻的格子 (x,y) 到达 (x,y) 的时间为 max(dis[i][j], moveTime[x][y]) + time
        其中 time 会在 [1,2] 之间交替，类似国际象棋棋盘，(i+j) 的奇偶性决定了 time 的大小
        time = (i + j) % 2 + 1
        """
        n, m = len(moveTime), len(moveTime[0])
        # 记录从 (0, 0) 到 (i, j) 的最小时间
        dis = [[float('inf')] * m for _ in range(n)]
        dis[0][0] = 0
        # (当前时间, i, j)
        h = [(0, 0, 0)]
        while True:
            d, i, j = heapq.heappop(h)
            # 判断是否到达终点
            if i == n - 1 and j == m - 1:
                return d
            # 该记录已经过时了，跳过
            if d > dis[i][j]:
                continue
            # 当前位置和的奇偶性决定移动时间 1,2,1,2,1,2
            time = (i + j) % 2 + 1
            for x, y in (i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1):
                if 0 <= x < n and 0 <= y < m:
                    new_dis = max(d, moveTime[x][y]) + time
                    # 如果 new_dis 更优，则更新 dis[x][y] 并加入堆
                    if new_dis < dis[x][y]:
                        dis[x][y] = new_dis
                        heapq.heappush(h, (new_dis, x, y))

# 7
# moveTime = [[0,4],[4,4]]

# 6
# moveTime = [[0,0,0,0],[0,0,0,0]]

# 4
moveTime = [[0,1],[1,2]]
r = Solution().minTimeToReach(moveTime)              
print(r)        