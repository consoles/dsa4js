from typing import List

class Solution:
    def numOfUnplacedFruits(self, fruits: List[int], baskets: List[int]) -> int:
        """
        模拟
        """
        placed_fruit = [False] * len(fruits)
        used_basket = [False] * len(baskets)
        for i, fruit in enumerate(fruits):
            for j, basket in enumerate(baskets):
                if not used_basket[j] and fruit <= basket:
                    placed_fruit[i] = True
                    used_basket[j] = True
                    break
        return placed_fruit.count(False)

    def numOfUnplacedFruits2(self, fruits: List[int], baskets: List[int]) -> int:
        """
        模拟：不引入额外空间
        枚举水果，然后从左向右枚举每个篮子：
        1. 能找到装得下当前水果的篮子，则这个篮子被占用了，将其容量更新为 0
        2. 如果找不到装得下当前水果的篮子，则计数器加 1
        """
        ans = 0
        for fruit in fruits:
            find = False
            for j, basket in enumerate(baskets):
                if baskets[j] > 0 and fruit <= basket:
                    baskets[j] = 0
                    find = True
                    break
            if not find:
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
