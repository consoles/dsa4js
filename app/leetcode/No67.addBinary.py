class Solution:
    def addBinary(self, a: str, b: str) -> str:
        i = len(a) - 1
        j = len(b) - 1
        carry = 0
        res = ''
        while i >= 0 or j >= 0 or carry > 0:
            x, y = 0, 0
            if i >= 0:
                x = int(a[i])
            y = 0
            if j >= 0:
                y = int(b[j])
            s = x + y + carry
            if s >= 2:
                s -= 2
                carry = 1
            else:
                carry = 0
            res = str(s) + res
            
            i -= 1
            j -= 1
        return res

    def addBinary2(self, a: str, b: str) -> str:
        # 0b10101
        return bin(int(a, 2) + int(b, 2))[2:]



# '100'
# a = "11"
# b = "1"

# '10101'
a = "1010"
b = "1011"
r = Solution().addBinary2(a, b)
print(r)
