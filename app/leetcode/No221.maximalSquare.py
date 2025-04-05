from typing import List

class Solution:
    def maximalSquare(self, matrix: List[List[str]]) -> int:
        # 思路1：从左向右，从上向下遍历矩阵，第一个遇到 1 的位置是左上角，从左上角向右下角拓展的过程中检查包围的区域是否合法，更新最大正方形

        def check(x1,y1,x2,y2:int) -> bool:
            for i in range(x1, x2 + 1):
                for j in range(y1, y2 + 1):
                    if matrix[i][j] != '1':
                        return False
            return True        

        def calc(i, j: int) -> int:
            x1, y1 = i, j
            # 正方形边长
            a = 0
            while i < m and j < n:
                if not check(x1, y1, i, j):
                   break
                a = (i - x1 + 1)
                i += 1
                j += 1
            return a ** 2       
                
        max_area = 0
        m, n = len(matrix), len(matrix[0])
        for i in range(m):
            for j in range(n):
                if matrix[i][j] == '1':
                    area = calc(i, j)
                    if area > max_area:
                        max_area = area
        return max_area
    
    def maximalSquare2(self, matrix: List[List[str]]) -> int:
        # 方法 1 的第 75 个用例会超时

        # 二维 DP
        # dp[i + 1][j + 1] 表示以第 i 行，第 j 列为右下角的正方形的最大边长，使用这个主要是假设第一行，第一列的边界外都有虚拟的一行 0 或者一列 0，这样就不用在填充 dp 的时候进行 if 判断了
        # 任何一个正方形都依赖当前格的 左，上，左上 3 个方格的情况
        # 正方形的边长受限于其周围 3 个方向的最小值
        if not matrix:
            return 0
        rows, cols = len(matrix), len(matrix[0])
        max_side = 0
        # 已经预处第一行，第一列为 0
        dp = [[0] * (cols + 1) for _ in range(rows + 1)]

        for row in range(rows):
            for col in range(cols):
                if matrix[row][col] == '1':
                    dp[row + 1][col + 1] = min(
                        dp[row + 1][col], # 左
                        dp[row][col + 1], # 上
                        dp[row][col] # 左上
                        ) + 1
                    max_side = max(max_side, dp[row + 1][col + 1])
        return max_side ** 2            

    
matrix = [["1","0","1","0","0"],["1","0","1","1","1"],["1","1","1","1","1"],["1","0","0","1","0"]]

# matrix = [["0","1"],["1","0"]]

r = Solution().maximalSquare2(matrix)
print(r)    
