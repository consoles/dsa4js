from typing import List

class Solution:
    def numberOfArrays(self, differences: List[int], lower: int, upper: int) -> int:
        """
        长度为 n 的 differences 数组可以产生长度为 n + 1 的 hidden 数组。

        给定 hidden[0]，可以唯一确定整个 hidden 数组：
        hidden[1] = hidden[0] + differences[0]
        hidden[2] = hidden[1] + differences[1] = hidden[0] + differences[0] + differences[1]
        ...
        hidden[i] = hidden[0] + sum(differences[0...i-1])

        对于所有的 hidden[i] 必须满足 lower <= hidden[i] <= upper
        设 prefix_sum[i] = sum(differences[0...i-1]) 表示 differences 的前 i 项的和。因此对于所有的 i(0 <= i <= n)
        满足 lower <= hidden[0] + prefix_sum[i] <= upper 
        lower - prefix_sum[i] <= hidden[0] <= upper - prefix_sum[i]
        
        我们还需要进一步精确找到 hidden[0] 的有效范围：
        找到 hidden[0] 的最小可能值和最大可能值，使得所有的 hidden[i] 位于 [lower, upper]，即：

        hidden[0] = max(lower - prefix_sum[i] for all i) 确定左区间
        hidden[0] = min(upper - prefix_sum[i] for all i) 确定右区间
        注意上述 i 的取值范围是 [0, n]    
        """
        n = len(differences)
        prefix_sum = [0] * (n + 1)
        for i in range(1, n + 1):
            prefix_sum[i] = prefix_sum[i - 1] + differences[i - 1]
        
        left, right = lower, upper
        for s in prefix_sum:
            left = max(left, lower - s)
            right = min(right, upper - s)

        if left > right:
            return 0
        return right - left + 1   

    def numberOfArrays2(self, differences: List[int], lower: int, upper: int) -> int: 
        """
        并不需要显式存储 prefix_sum 数组，直接一次遍历中完成 prefix_sum 的计算和左右边界的计算
        """
        left, right = lower, upper
        prefix_sum = 0
        for d in differences:
            prefix_sum += d
            left = max(left, lower - prefix_sum)
            right = min(right, upper - prefix_sum)
            if left > right:
                return 0
        return right - left + 1

# 2
differences = [1,-3,4]
lower = 1
upper = 6

# 4
# differences = [3,-4,5,1,-2]
# lower = -4
# upper = 5

# 0
# differences = [4,-7,2]
# lower = 3
# upper = 6
r = Solution().numberOfArrays2(differences, lower, upper)
print(r)
