from typing import List 

class Solution:
    def getWordsInLongestSubsequence(self, words: List[str], groups: List[int]) -> List[str]:
        """
        dp[i] 表示以下标 i 为结尾的最长子序列长度。设 hammingDistance(s, t) 表示字符串 s 和 t 的汉明距离。子序列中如果下标 i 可以添加在下标 j 之后，则此时一定满足 groups[i] != groups[j], j < i 且 hammingDistance(words[i], words[j]) == 1。此时以下标 i 为结尾的最长子序列长度为 dp[i] = max(dp[i], dp[j] + 1)。

        dp[i] = max(dp[i], dp[j] + 1) if groups[i] != groups[j] and j < i and hammingDistance(words[i], words[j]) == 1

        对于下标 i，我们可以枚举 i 之前的下标，就可以求得以 i 为结尾的最长子序列长度。依次求出以每个下标结尾的最长子序列的长度就可以找到 [0,1,...,n-1]中的最长子序列长度。为了计算方便，我们用 prev[i] 记录以下标 i 为结尾的最长子序列中 i 的上一个下标。当我们找到最长子序列的结果下标 i 时，沿着 i 往前找即可找到整个序列的下标，并将每个下标对应的字符串加入到数组中，对整个数组进行反转就能得到答案。
        """
        n = len(words)
        dp = [1] * n
        prev = [-1] * n
        max_index = 0

        def check(s1, s2: str) -> bool:
            """
            检查 2 个字符串的汉明距离是否为 1
            """
            if len(s1) != len(s2):
                return False
            diff_count = 0
            for c1, c2 in zip(s1, s2):
                if c1 != c2:
                    diff_count += 1
                    if diff_count > 1:
                        return False
            return diff_count == 1        

        for i in range(1, n):
            for j in range(i):
                ok = check(words[i], words[j])
                if ok and dp[j] + 1 > dp[i] and groups[i] != groups[j]:
                    dp[i] = dp[j] + 1
                    prev[i] = j
            if dp[i] > dp[max_index]:
                max_index = i

        ans = []
        i = max_index
        while i >= 0:
            ans.append(words[i])
            i = prev[i]
        ans.reverse()
        return ans