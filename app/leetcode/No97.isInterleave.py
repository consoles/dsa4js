from functools import cache

class Solution:
    def isInterleave(self, s1: str, s2: str, s3: str) -> bool:
        # 二维 DP
        # O(M*N)
        # dp[i][j] 表示 s1 的前 i 个字符和 s2 的前 j 个字符能否交错组成 s3 的前 i + j 个字符
        # 状态转移方程：
        # 1. 如果 s1[i-1] == s3[i+j-1]，则 dp[i][j] = dp[i-1][j]
        # 2. 如果 s2[j-1] == s3[i+j-1]，则 dp[i][j] = dp[i][j-1]
        # 最终 dp[i][j] 是上面 2 种情况的逻辑或
        # 
        # 初始化：
        # dp[0][0] = True 表示空字符串可以由两个空串组成
        # dp[i][0]: 检查 s1 的前 i 个字符是否与 s3 的前 i 个字符匹配
        # dp[0][j]: 检查 s2 的前 j 个字符是否与 s3 的前 j 个字符匹配
        m, n = len(s1), len(s2)
        if m + n != len(s3):
            return False
        
        dp = [[False] * (n + 1) for _ in range(m + 1)]
        dp[0][0] = True

        # 初始化第一列(s2 为空)
        for i in range(1, m + 1):
            dp[i][0] = dp[i - 1][0] and s1[i-1] == s3[i-1]

        # 初始化第一行(s1 为空)
        for j in range(1, n + 1):
            dp[0][j] = dp[0][j-1] and s2[j-1] == s3[j-1]


        # 填充 dp
        for i in range(1, m + 1):
            for j in range(1, n + 1):
                dp[i][j] = (dp[i-1][j] and s1[i-1] == s3[i+j-1]) or \
                    (dp[i][j-1] and s2[j-1] == s3[i+j-1])

        return dp[m][n]

    def isInterleave2(self, s1: str, s2: str, s3: str) -> bool:
        # O(2^(M+N))
        l1, l2, l3 = len(s1), len(s2), len(s3)
        if l1 + l2 != l3:
            return False

        def dfs(i,j,k:int) -> bool:
            # k 越界，表示 s3 扫描完了，返回 True
            if k == l3:
                return True
            
            valid = False
            if i < l1 and s1[i] == s3[k]:
                valid = dfs(i + 1, j, k + 1)
                if valid:
                    return True

            if j < l2 and s2[j] == s3[k]:
                valid = dfs(i, j + 1, k + 1)
                if valid:
                    return True

            return valid 

        return dfs(0, 0, 0)

    def isInterleave3(self, s1: str, s2: str, s3: str) -> bool:  
        # O(2^(M+N))
        l1, l2, l3 = len(s1), len(s2), len(s3)
        if l1 + l2 != l3:
            return False

        # 方法 2 做了很多无用计算
        @cache
        def dfs(i,j,k:int) -> bool:
            # k 越界，表示 s3 扫描完了，返回 True
            if k == l3:
                return True
            
            valid = False
            if i < l1 and s1[i] == s3[k]:
                valid = dfs(i + 1, j, k + 1)
                if valid:
                    return True

            if j < l2 and s2[j] == s3[k]:
                valid = dfs(i, j + 1, k + 1)
                if valid:
                    return True

            return valid 

        return dfs(0, 0, 0)  

# s1 = "aabcc"
# s2 = "dbbca"
# s3 = "aadbbcbcac"

s1 = "bbbbbabbbbabaababaaaabbababbaaabbabbaaabaaaaababbbababbbbbabbbbababbabaabababbbaabababababbbaaababaa"
s2 = "babaaaabbababbbabbbbaabaabbaabbbbaabaaabaababaaaabaaabbaaabaaaabaabaabbbbbbbbbbbabaaabbababbabbabaab"
s3 = "babbbabbbaaabbababbbbababaabbabaabaaabbbbabbbaaabbbaaaaabbbbaabbaaabababbaaaaaabababbababaababbababbbababbbbaaaabaabbabbaaaaabbabbaaaabbbaabaaabaababaababbaaabbbbbabbbbaabbabaabbbbabaaabbababbabbabbab"

# s1 = 'aaa'
# s2 = ''
# s3 = 'aaa'
r = Solution().isInterleave(s1, s2, s3)
print(r)
