from typing import List

class SegmentTree:
    def __init__(self, a:List[int]):
        n = len(a)
        self.max = [0] * (2 << (n - 1).bit_length())
        self.build(a, 1, 0, n - 1)

    def maintain(self, o: int):
        self.max[o] = max(self.max[o * 2], self.max[o * 2 + 1])

    def build(self, a: List[int], o, l, r: int):
        if l == r:
            self.max[o] = a[l]
            return
        m = (l + r) // 2
        self.build(a, o * 2, l, m)
        self.build(a, o * 2 + 1, m + 1, r)
        self.maintain(o)

    def find_first_and_update(self,o,l,r: int, x: int) -> int:
        """
        找区间内第一个 >= x 的数，并更新为 -1，返回这个数的下标（没有则返回 -1）
        改成 -1 之后，再次查找 >=x 的数，就会找不到了（因为这个篮子已经被占用了）
        有点像最小堆，修改了叶子节点，需要 swim 更新其祖先节点
        """

        if self.max[o] < x: # 区间没有 >= x 的数
            return -1

        if l == r:
            self.max[o] = -1 # 更新为 -1，表示不能放水果
            return l

        m = (l + r) // 2
        i = self.find_first_and_update(o * 2, l, m, x) # 先递归左子树
        if i == -1: # 左子树没找到
            i = self.find_first_and_update(o * 2 + 1, m + 1, r, x)
        self.maintain(o)
        return i

class Solution:
    """
    数据规模较大：10^5，下面的二重循环模拟的方法会超时
    """
    def numOfUnplacedFruits(self, fruits: List[int], baskets: List[int]) -> int:
        ans = len(fruits)
        for fruit in fruits:
            for i, basket in enumerate(baskets):
                if basket >= fruit:
                    ans -= 1
                    baskets[i] = 0
                    break
        return ans

    def numOfUnplacedFruits2(self, fruits: List[int], baskets: List[int]) -> int:
        t = SegmentTree(baskets)
        n = len(baskets)
        ans = 0
        for fruit in fruits:
            if t.find_first_and_update(1, 0, n - 1, fruit) < 0:
                ans += 1
        return ans

# 1
fruits = [4,2,5]
baskets = [3,5,4]

# 0
# fruits = [3,6,1]
# baskets = [6,4,7]
r = Solution().numOfUnplacedFruits2(fruits, baskets)
print(r)
