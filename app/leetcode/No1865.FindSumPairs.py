from typing import List
from collections import Counter

class FindSumPairs:

    def __init__(self, nums1: List[int], nums2: List[int]):
        self.nums1 = nums1
        self.nums2 = nums2
        self.num2_counter = Counter(nums2)

    def add(self, index: int, val: int) -> None:
        old_num2 = self.nums2[index]
        self.num2_counter[old_num2] -= 1
        new_num2 = old_num2 + val
        self.nums2[index] = new_num2
        self.num2_counter[new_num2] += 1
        
    def count(self, tot: int) -> int:
        # count = 0
        # for num1 in self.nums1:
        #     for num2 in self.nums2:
        #         if num1 + num2 == tot:
        #             count += 1
        # return count  
        count = 0
        for num1 in self.nums1:
            if tot - num1 in self.num2_counter:
                count += self.num2_counter[tot - num1]
        return count

# Your FindSumPairs object will be instantiated and called as such:
# obj = FindSumPairs(nums1, nums2)
# obj.add(index,val)
# param_2 = obj.count(tot)