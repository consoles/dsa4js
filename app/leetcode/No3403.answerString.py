class Solution:
    def answerString(self, word: str, numFriends: int) -> str:
        """
        如果固定子串的左端点，那么字符串越长，字典序越大。所以核心思路就是枚举子串的左端点，计算最大子串。
        由于其余 k - 1 个子串必须非空（长度至少为 1），所以其余子串的长度和至少是 k - 1.因此当前子串的长度最大值是 n - (k - 1)
        当 k == 1 时，此时无法分割，子串左端点只能是 0，答案是字符串本身
        """
        if numFriends == 1:
            return word
        n = len(word)
        ans = ""
        for i in range(n):
            cur = word[i:i + n - (numFriends - 1)]
            if cur > ans:
                ans = cur
        return ans

# dbc
word = "dbca"
numFriends = 2

# g
# word = 'gggg'
# numFriends = 4
r = Solution().answerString(word, numFriends)
print(r)
