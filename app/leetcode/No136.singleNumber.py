from typing import List

class Solution:
    def singleNumber(self, nums: List[int]) -> int:
        # x ^ 0 = x
        # x ^ x = 0
        r = 0
        for num in nums:
            r ^= num
        return r    

    def singleNumber2(self, nums: List[int]) -> int:
        s = set()
        for num in nums:
            if num in s:
                s.remove(num)
            else:
                s.add(num)
        # 如果一个元素出现 2 次，那么第一次会被加入，第二次会被移除，最后几个中剩余的元素就是仅仅出现 1 次的元素
        return s.pop()
