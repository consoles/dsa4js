from typing import List
# Definition for a QuadTree node.
class Node:
    def __init__(self, val, isLeaf, topLeft, topRight, bottomLeft, bottomRight):
        self.val = val
        self.isLeaf = isLeaf
        self.topLeft = topLeft
        self.topRight = topRight
        self.bottomLeft = bottomLeft
        self.bottomRight = bottomRight


class Solution:
    def construct(self, grid: List[List[int]]) -> 'Node':
        # 以 [start_row, start_col] 为起点, (end_row, end_col) 为终点的矩形区域
        # 这里不用 (x, y) 这种命名主要是 x 表示水平方向， y 表示竖直方向 和我们常规的矩阵的行列坐标有直观上的冲突
        def dfs(start_row, start_col, end_row, end_col):
            same = True
            val = grid[start_row][start_col]
            for i in range(start_row, end_row):
                for j in range(start_col, end_col):
                    if grid[i][j] != val:
                        same = False
                        break
                if not same:
                    break

            if same:
                return Node(val == 1, True, None, None, None, None)

            mid_row = (start_row + end_row) // 2
            mid_col = (start_col + end_col) // 2
            node = Node(True, False, None, None, None, None)
            node.topLeft     = dfs(start_row, start_col, mid_row, mid_col)
            node.topRight    = dfs(start_row, mid_col, mid_row, end_col)
            node.bottomLeft  = dfs(mid_row, start_col, end_row, mid_col)
            node.bottomRight = dfs(mid_row, mid_col, end_row, end_col)
            return node

        return dfs(0, 0, len(grid), len(grid[0]))

    def construct2(self, grid: List[List[int]]) -> 'Node':
        # 对方法 1 中暴力判定矩阵式元素是够全 1 和 全 0 的优化：
        # 当某一部分均为 0 时，它的和为 0；某一部分均为 1 时，它的和为这一部分的面积大小
        # 可以使用前缀和进行优化
        n = len(grid) # 可以保证 grid 是方阵
        # pre[i][j] 表示从 grid[0][0] 到 grid[i-1][j-1] 的子矩阵的和
        # pre[i-1][j]：上方矩阵和
        # pre[i][j-1]：左方矩阵和
        # pre[i-1][j-1]：上方和左方矩阵和重叠的部分
        # grid[i-1][j-1]: 当前位置的值
        # 这是典型的 DP 思想
        pre_sum = [[0] * (n + 1) for _ in range(n + 1)]
        for i in range(1, n + 1):
            for j in range(1, n + 1):
                # (i,j) 位置的前缀和等于相邻的 3 个位置
                pre_sum[i][j] = pre_sum[i - 1][j] + pre_sum[i][j - 1] - pre_sum[i - 1][j - 1] + grid[i - 1][j - 1]


        def get_sum(row1, col1, row2, col2): # 获取子矩阵的和
            # sum = pre_sum[row2][col2](整个大矩形)
            # - pre_sum[row1-1][col2](上方矩形)
            # - pre_sum[row2][col1-1](左方矩形)
            # + pre_sum[row1-1][col1-1](上方和左方矩形重叠的部分,被减去 2 次)
            return pre_sum[row2][col2] - pre_sum[row2][col1] - pre_sum[row1][col2] + pre_sum[row1][col1]      

        def dfs(start_row, start_col, end_row, end_col):
            total_sum = get_sum(start_row, start_col, end_row, end_col)
            # 全 0 和 全 1 都是叶子节点
            if total_sum == 0:
                return Node(False, True, None, None, None, None) # 全 0
            if total_sum == (end_row - start_row) * (end_col - start_col):
                return Node(True, True, None, None, None, None) # 全 1
            mid_row = (start_row + end_row) // 2
            mid_col = (start_col + end_col) // 2
            node = Node(True, False, None, None, None, None)
            node.topLeft     = dfs(start_row, start_col, mid_row, mid_col)
            node.topRight    = dfs(start_row, mid_col, mid_row, end_col)
            node.bottomLeft  = dfs(mid_row, start_col, end_row, mid_col)
            node.bottomRight = dfs(mid_row, mid_col, end_row, end_col)
            return node
        
        return dfs(0, 0, n, n)
