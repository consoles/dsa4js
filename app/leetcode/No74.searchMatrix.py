from typing import List

class Solution:
    def searchMatrix(self, matrix: List[List[int]], target: int) -> bool:
        m,n = len(matrix),len(matrix[0])
        # 由于每行的第一个元素大于前一行的最后一个元素，且每行元素是升序的，
        # 所以每行的第一个元素大于前一行的第一个元素
        # 因此矩阵第一列的元素是升序的
        # 首先使用二分查找找到该元素对应的行
        # 然后在该行中进行二分查找找到精确位置

        def findRowIndex():
            """扫描矩阵的第一列，找到行"""
            start, end = 0, m - 1
            while start <= end:
                mid = (start + end) // 2
                num = matrix[mid][0]
                if num == target:
                    return mid
                if target < num:
                    # 目标元素在上一行
                    end = mid - 1
                else:
                    start = mid + 1
            return end

        row_index = findRowIndex()
        if row_index < 0:
            return False
        
        start, end = 0, n - 1
        while start <= end:
            mid = (start + end) // 2
            num = matrix[row_index][mid]
            if num == target:
                return True
            if target > num:
                start = mid + 1
            else:
                end = mid - 1
        return False   

    def searchMatrix2(self, matrix: List[List[int]], target: int) -> bool: 
        # 将矩阵降维成数组，则数组是非严格升序的
        # m * n 的矩阵可以降维成 [0, m*n - 1] 的一维数组
        # 将 1 维坐标映射到二维
        # i = x // col_count
        # j = x % col_count
        m, n = len(matrix), len(matrix[0])
        l, r = 0, m * n - 1
        while l <= r:
            mid = (l + r) // 2
            i = mid // n
            j = mid % n
            num = matrix[i][j]
            if target == num:
                return True
            if target < num:
                r = mid - 1
            else:
                l = mid + 1
        return False         

matrix = [[1,3,5,7],[10,11,16,20],[23,30,34,60]]
target = 3
r = Solution().searchMatrix2(matrix, target)
print(r)
