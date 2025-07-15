class Solution:
    def isValid(self, word: str) -> bool:
        """
        最朴素的方法，遍历，计数
        """
        if len(word) < 3:
            return False

        has_vowel = False
        has_consonant = False
        for c in word:
            if c.isalpha():
                if c.lower() in "aeiou":
                    has_vowel = True
                else:
                    has_consonant = True
            elif not c.isdigit():
                return False
        return has_vowel and has_consonant

# True
# word = "234Adas"

# False
word = "b3"

# False
word = "a3$e"
r = Solution().isValid(word)
print(r)
