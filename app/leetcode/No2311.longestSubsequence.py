class Solution:
    def longestSubsequence(self, s: str, k: int) -> int:
        """
        贪心
        构造最长子序列
        最长子序列包含原二进制字符串中所有 0，然后再从右向左（低位到高位）添加 1，使得子序列对应的二进制数字小于等于 k
        """
        n = len(s)
        res = s.count("0")
        num = 0
        for i in range(n - 1, -1, -1):
            if s[i] == '1':
                bit_num = 1 << (n - i - 1)
                if num + bit_num > k:
                    break
                res += 1
                num += bit_num
        return res

s = "1001010"
k = 5
r = Solution().longestSubsequence(s, k)
print(r)
