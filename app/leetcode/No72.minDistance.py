from functools import cache

class Solution:
    def minDistance(self, word1: str, word2: str) -> int:
        # O(2^N)
        # minDis 的内部实现采用索引值而不是字符串切片可以进一步优化性能

        @cache
        def minDis(word1, word2: str) -> int:
            if len(word1) == 0:
                # word1 -> word2，len(word2) 次 insert
                return len(word2)
            if len(word2) == 0:
                # word1 -> word2, len(word1) 次 delete
                return len(word1)
            
            # 当前字符相等，无需变换，从下一个位置继续检查
            if word1[0] == word2[0]:
                return minDis(word1[1:], word2[1:])
            
            insert_count = 1 + minDis(word1, word2[1:])
            delete_count = 1 + minDis(word1[1:], word2)
            replace_count = 1 + minDis(word1[1:], word2[1:])
            return min(insert_count, delete_count, replace_count)
            
        return minDis(word1, word2)
    
    def minDistance2(self, word1: str, word2: str) -> int:
        # DP O(M*N)
        # dp[i][j] 表示 word1 中的前 i 个字符变换到 word2 中的前 j 个字符，最短需要操作的次数
        # 需要考虑 word1 或者 word2 其中一个为空串的情况，即：全增加或者全删除，dp[i][0], dp[0][j] 需要特殊初始化
        # 状态转移：
        # 增: dp[i][j] = dp[i][j-1] + 1
        # 删：dp[i][j] = dp[i-1][j] + 1
        # 改：dp[i][j] = dp[i-1][j-1] + 1
        # 当 word1[i-1] 和 word2[j-1] 相等的时候可以直接参考 dp[i-1][j-1] 不用做任何操作

        m, n = len(word1), len(word2)
        dp = [[0] * (n + 1) for _ in range(m + 1)]

        for i in range(m + 1):
            dp[i][0] = i
        for j in range(n + 1):
            dp[0][j] = j

        for i in range(1, m + 1):
            for j in range(1, n + 1):
                if word1[i-1] == word2[j-1]:
                    dp[i][j] = dp[i-1][j-1]
                else:
                    dp[i][j] = min(
                        dp[i][j-1] + 1, # ADD
                        dp[i-1][j] + 1, # DELETE
                        dp[i-1][j-1] + 1, # REPLACE
                    )
        return dp[m][n]            




# 3
word1 = "horse"
word2 = "ros"

# 5
word1 = "intention"
word2 = "execution"

# 6
word1 = "dinitrophenylhydrazine"
word2 = "acetylphenylhydrazine"

r = Solution().minDistance2(word1, word2)
print(r)
