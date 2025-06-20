from collections import defaultdict

class Solution:
    def maxDistance(self, s: str, k: int) -> int:
        """
        对于曼哈顿距离，水平和竖直方向上的移动互不影响，我们可以分别计算横纵坐标
        如果向东走了 e 步，向西走了 w 步
        1. e > w，取 e 作为正方向，w 要全部变成 e。如果 w 较少 d，那么 e 就增加 d。最终横坐标 (e + d) - (w - d) = e - w + 2d
        2. e < w，取 w 作为正方向，e 要全部变成 w。如果 e 较少 d，那么 w 就增加 d。最终横坐标 (w + d) - (e - d) = w - e + 2d
        综合上面 2 种情况，修改之后的横坐标增加值为 abs(e - w) + 2d, 其中 d 为操作次数 d = min(e, w, k)
        水平方向上的移动处理完了，还需要判断竖直方向上还有没有能继续处理的 d。如果有剩余的可移动次数，按照相同的方法进行处理即可。
        """
        counter = defaultdict(int)
        ans = 0
        for ch in s:
            counter[ch] += 1
            left = k

            def helper(a, b: int) -> int:
                nonlocal left
                d = min(a, b, left)
                left -= d
                return abs(a - b) + d * 2

            ans = max(ans, helper(counter['N'], counter['S']) + helper(counter['W'], counter['E']))
        return ans

    def maxDistance2(self, s: str, k: int) -> int:
        """
        贪心：当前位置 (x,y) 到原点的曼哈顿距离是 abs(x) + abs(y)
        再看方法1中的等式 abs(a - b) + 2d，其中 abs(a-b) 可以理解为 abs(x)，d 可以理解为操作次数
        即：每执行 1 次操作，曼哈顿距离都会增加 2，但是不会超过移动次数(i+1)
        所以执行完后 s[i] 的答案为 min(abs(x) + abs(y) + 2k, i + 1)
        """
        ans = x = y = 0
        for i, c in enumerate(s):
            if c == 'N':
                y += 1
            elif c == 'S':
                y -= 1
            elif c == 'E':
                x += 1
            elif c == 'W':
                x -= 1
            ans = max(ans, min(abs(x) + abs(y) + 2 * k, i + 1))
        return ans

s = "NWSE"
k = 1
r = Solution().maxDistance2(s, k)
print(r)
