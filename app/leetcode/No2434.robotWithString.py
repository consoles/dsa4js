class Solution:
    def robotWithString(self, s: str) -> str:
        """
        根据题意，t 中字符是先进后出的，是一个栈符串
        问题相当于：有一个初始为空的栈，给定字符的入栈顺序，求字典序最小的出栈序列

        当一个字符入栈后，我们持续检查栈顶元素 c。假设还未入栈的字符中，字典序最小的元素是 m，那么有下面 2 种情况：
        1. c <= m，此时弹出 c 最优。如果此时按兵不动，下一个出栈的将会是大等于 c 的字符，答案不会变得更优
        2. c > m，此时不弹出 c，等待后续更小的字符入栈
        所有字符入栈之后，栈内剩余的字符按顺序弹出即可。
        """

        n = len(s)
        # 表示从当前位置 i 到字符串末尾的所有元素中，最小的那个字符【后缀最小值】
        f = [''] * (n + 1)
        f[n] = '{' # 'z' + 1
        i = n - 1
        while i >= 0:
            f[i] = min(f[i + 1], s[i])
            i -= 1

        stack = []
        ans = []
        for i in range(n):
            # 当前元素入栈
            stack.append(s[i])
            # 只要栈顶元素 <= 当前后面最小的字符，就弹出栈顶，加入答案
            while stack and stack[-1] <= f[i + 1]:
                ans.append(stack.pop())
        return "".join(ans)

# azz
# s = "zza"

# abc
# s = 'bac'

# addb
s = "bdda"
r = Solution().robotWithString(s)
print(r)
