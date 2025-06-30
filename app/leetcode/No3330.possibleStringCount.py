class Solution:
    def possibleStringCount(self, word: str) -> int:
        """
        如果一个字符在 word 中连续出现了 k(k>1) 次，那么在实际的原始字符串中，这个字符可能出现了 1,2, ... k - 1 次
        对于 k = 1 的这种情况，Alice 不会犯错，有 0 种可能性。因此对于任意的字符，原始字符的可能性是 k - 1
        因此，我们可以对字符串进行一次遍历：记当前遍历到位置 l，并且 word 中位置为 [l,r] 的字符都相同，而位置为 r+1 的字符不相同（或不存在），那么我们将答案增加 r−l，并接着从位置 r+1 继续遍历。
        这进一步告诉我们，由于 [l,r] 对答案的总贡献是 r−l，那么可以看成位置 l 对答案没有贡献，而位置 [l+1,r] 对答案的贡献分别都是 1。因此，对于 word 中的任意位置 i (i>0)，只要 word[i−1]=word[i]，就将答案增加 1 即可。
        """
        ans = 1
        for i in range(1, len(word)):
            if word[i] == word[i - 1]:
                ans += 1
        return ans

# 5
word = "abbcccc"
r = Solution().possibleStringCount(word)
print(r)
