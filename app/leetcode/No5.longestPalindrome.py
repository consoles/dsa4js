class Solution:
    def longestPalindrome(self, s: str) -> str:
        # 1. 找出子串
        # 2. 判断回文
        # O(N^3)

        def checkPalindrome(l: int, r: int):
            if l > r:
                return False
            while l < r:
                if s[l] != s[r]:
                    return False
                l += 1
                r -= 1
            return True    

        max_len = 0
        max_s = ''
        n = len(s)
        for i in range(n):
            for j in range(i, n):
                flag = checkPalindrome(i, j)
                if flag:
                    l = j - i + 1
                    if l > max_len:
                        max_len = l
                        max_s = s[i:j+1]
        return max_s
    
    def longestPalindrome2(self, s: str) -> str:
        # 方法 1 实现简单，但是对于超长字符串会超时
        n = len(s)
        if n < 2:
            return s
        
        # 中心扩散法：
        # 遍历字符串，对于每个 s[i] 
        # 以 s[i] 为中心向左右进行拓展，找到最长的奇数回文长度
        # 以 s[i], s[i+1] 为中心，向左右进行拓展找到最长的偶数回文长度
        # 比较当前的回文长度，更新 start, end
        # 取 s[start:end + 1]

        def expandAroundCenter(l:int, r:int):
            while l >= 0 and r < n:
                if s[l] != s[r]:
                    break
                l -= 1
                r += 1
            return l + 1, r - 1

        start, end = 0, 0
        for i in range(n):
            l1, r1 = expandAroundCenter(i, i)
            if r1 - l1 > end - start:
                start, end = l1, r1
            if i + 1 < n and s[i] == s[i+1]:
                l2, r2 = expandAroundCenter(i, i+1)
                if r2 - l2 > end - start:
                    start, end = l2, r2
        return s[start:end+1]            

    def longestPalindrome3(self, s: str) -> str:
        # 二维 DP
        # dp[i][j] 表示 s 的子串 s[i...j] 是否是回文
        # 对于一个子串 s[i...j] 它是回文的条件是:
        # 1. s[i] == s[j]
        # 2. s[i+1...j-1] 是回文子串 -> 即去掉首尾字符后的子串也是回文
        # 状态转移方程 dp[i][j] = (s[i] == s[j]) and dp[i+1][j-1]
        # 
        # 初始条件：
        # 对于单个字符，任何单个字符都是回文子串: dp[i][i] = True
        # 对于 2 个字符，如果相邻字符相同，则它们是回文子串：dp[i][i+1] = (s[i] == s[i+1])

        n = len(s)
        if n < 2:
            return s
        
        dp = [[False] * n for _ in range(n)]

        # 所有长度为 1 的子串都是回文
        for i in range(n):
            dp[i][i] = True

        start = 0
        max_len = 1
        
        # 长度为 2 的子串
        for i in range(n - 1):
            if s[i] == s[i+1]:
                dp[i][i+1] = True
                start = i
                max_len = 2

        # 长度大于 2 的子串，子串长度从 3 - len
        for l in range(3, n + 1):
            for i in range(n - l + 1): # 起始索引
                j = i + l - 1 # 结束索引
                if s[i] == s[j] and dp[i+1][j-1]:
                    dp[i][j] = True
                    if l > max_len:
                        max_len = l
                        start = i

        return s[start: start + max_len]

    def longestPalindrome4(self, s: str) -> str:           
        # 马拉车, 时间和空间复杂度都是 O(N)
        # 这个算法比较费脑（看以后研究了）
        # 核心是利用回文的对称性和已计算的信息来避免重复计算
        # 通过维护一个回文半径数组 P[i] 和一个当前回文右边界 R 高效拓展回文

        # 预处理
        # 将原字符串插入特殊字符 # ，统一处理奇偶长度回文，预处理后长度一定是奇数

        # 回文半径数组 P[i]: 表示以 i 为中心点的最长回文长度(包含 i 本身)
        # 例如: #a#b#a# 的 P[3] = 3 对应回文 aba

        # 当前回文右边界 R: 记录当前所有回文子串能到达的最右位置

        # 当前回文中心 C: 与 R 对应的回文中心

        # 1. 预处理，首尾添加哨兵字符，避免边界检查：s = "aba" → "^#a#b#a#$"
        T = '^#' + '#'.join(s) + '#$'
        n = len(T)
        P = [0] * n # 每个位置的回文半径
        C, R = 0, 0

        for i in range(1, n - 1):
            # 利用对称性初始化 P[i]
            if i < R:
                P[i] = min(R - i, P[2 * C - i])

            # 暴力拓展
            while T[i + P[i] + 1] == T[i - P[i] - 1]:
                P[i] += 1

            if i + P[i] > R:
                C, R = i, i + P[i]

        # 找到最长回文
        max_len = max(P)
        center = P.index(max_len)
        start = (center - max_len) // 2
        end = start + max_len
        return s[start:end]  


s = "babad"
s = 'cbbd'
s = 'a'
s = 'bb'
s = "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
r = Solution().longestPalindrome4(s)                  
print(r)
