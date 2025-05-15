from typing import List

class Solution:
    def getLongestSubsequence(self, words: List[str], groups: List[int]) -> List[str]:
        """
        求最长的 0,1 交替串
        先不管 words 数组，只看 groups 数组。在 groups 数组中选一些元素使得挑选结果为 0101... 或者 1010...

        贪心：
        由于group对应的下标只有可能是010101...或者101010...，假设最优解对应的group是010101...且不是从头开始的，那么说其group的前面必定存在0和/或1。如果前面只有1，即肯定有[1]010101...比我们现在的【最优解】更长；如果前面只有0，那么说其长度也只是和【最优解】等长。因此从头开始查找必定能是最优解。
        """
        ans = []
        for i, g in enumerate(groups):
            if i == 0 or g != groups[i-1]:
                ans.append(words[i])
        return ans        

# [a,b]
words = ["e","a","b"]
groups = [0,0,1]
r = Solution().getLongestSubsequence(words, groups)
print(r)