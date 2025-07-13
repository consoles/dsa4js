from typing import Optional

# Definition for singly-linked list.
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

class Solution:
    def getDecimalValue(self, head: Optional[ListNode]) -> int:
        """
        二进制链表转整数
        1. 先讲链表转为字符串，再将二进制字符串转整数
        """
        string = ''
        while head:
            string += str(head.val)
            head = head.next
        return int(string, 2)
    
    def getDecimalValue2(self, head: Optional[ListNode]) -> int:
        """
        模拟: str2(101) = 1 * 2^2 + 0 * 2^1 + 1 * 2^0
        我们并不需要知道链表长度才确定指数
        """
        res = 0
        while head:
            res = res * 2 + head.val
            head = head.next
        return res
    
# 5
# head = ListNode(1, ListNode(0, ListNode(1))) 

# 0
head = ListNode(0)
r = Solution().getDecimalValue2(head)
print(r)   
