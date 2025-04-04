from typing import List

class NumMatrix:

    def __init__(self, matrix: List[List[int]]):
        if not matrix or not matrix[0]:
            self.pre_sum = [[0]]
            return
        
        m, n = len(matrix), len(matrix[0])
        self.pre_sum = [[0] * (n + 1) for _ in range(m + 1)]
        
        for i in range(1, m + 1):
            for j in range(1, n + 1):
                self.pre_sum[i][j] = (
                    self.pre_sum[i - 1][j] 
                    + self.pre_sum[i][j - 1] 
                    - self.pre_sum[i - 1][j - 1] 
                    + matrix[i - 1][j - 1]
                )

    def sumRegion(self, row1: int, col1: int, row2: int, col2: int) -> int:
        return (
            self.pre_sum[row2 + 1][col2 + 1] 
            - self.pre_sum[row2 + 1][col1] 
            - self.pre_sum[row1][col2 + 1] 
            + self.pre_sum[row1][col1]
        )
