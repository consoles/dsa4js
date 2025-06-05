class UnionFind:
    def __init__(self, n):
        self.parent = [i for i in range(n)]

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, x, y):
        rootX = self.find(x)
        rootY = self.find(y)
        if rootX == rootY:
            return
        # 总是让字典序更小的作为集合的代表字符
        if rootX < rootY:
            self.parent[rootY] = rootX
        else:
            self.parent[rootX] = rootY

class Solution:
    def smallestEquivalentString(self, s1: str, s2: str, baseStr: str) -> str:
        """
        等价字符具有自反性、对称性和传递性，因此可以将整个等价字符集看做是无向图中的连通块，在同一个连通块中的点（字符）互相等价，可以相互替换。我们要为 baseStr 中的每个字符找到它所在的连通块中字典序最小的字符。

        根据题目给定的 s1 和 s2，依次遍历每对字符 s1[i] 和 s2[i]，将 s1[i] 和 s2[i] 添加到同一个连通块中。最后可以得到完整的连通块，就可以进行字符替换了。

        维护连通块的合并，可以使用 UnionFind 数据结构
        """
        uf = UnionFind(26)
        for a, b in zip(s1, s2):
            uf.union(ord(a) - ord('a'), ord(b) - ord('a'))
        return ''.join(chr(uf.find(ord(c) - ord('a')) + ord('a')) for c in baseStr)

# makkek
# s1 = "parker"
# s2 = "morris"
# baseStr = "parser"

# hdld
s1 = "hello"
s2 = "world"
baseStr = "hold"
r = Solution().smallestEquivalentString(s1, s2, baseStr)
print(r)
