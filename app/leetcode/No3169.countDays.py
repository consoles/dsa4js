from typing import List

class Solution:
    def countDays(self, days: int, meetings: List[List[int]]) -> int:
        """
        正难则反
        区间合并：将每个会议看作一个闭区间，合并区间后用总天数减去每一段区间的长度就是答案
        """
        # 按照开始时间升序，如果开始时间相同，则按照结束时间升序
        # Python 对于多维数据会依次排序每一个维度，直到找出差异。如果最终找不出差异就保持数据原有的相对顺序（稳定性）
        meetings.sort()
        l, r = 1, 0
        for start, end in meetings:
            # 产生新的区间
            if start > r:
                days -= r - l + 1
                l = start
            r = max(r, end)
        days -= r - l + 1
        return days