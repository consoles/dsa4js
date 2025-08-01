from typing import List

class Solution:
    def generate(self, numRows: int) -> List[List[int]]:
        """
        第一行 1 个数字，第 2 行 2 个
        """
        res = [[1]]
        for i in range(1, numRows):
            row_res = [] * (i + 1)
            for j in range(i + 1):
                if j == 0 or j == i:
                    row_res.append(1)
                else:
                    row_res.append(res[i - 1][j - 1] + res[i - 1][j])
            res.append(row_res)
        return res

numRows = 5
r = Solution().generate(numRows)
print(r)
