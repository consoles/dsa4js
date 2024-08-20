package main

type ListNode struct {
	Val  int
	Next *ListNode
}

/**
 * Definition for singly-linked list.
 * type ListNode struct {
 *     Val int
 *     Next *ListNode
 * }
 */
func rotateRight(head *ListNode, k int) *ListNode {
	if k == 0 || head == nil || head.Next == nil {
		return head
	}
	// 统计链表长度
	len := 0
	for cur := head; cur != nil; cur = cur.Next {
		len++
	}
	k = k % len // 旋转 len 次就回到初始状态了
	if k == 0 {
		return head
	}
	// 快指针先走 k 步
	fast := head
	for i := 0; i < k; i++ {
		fast = fast.Next
	}
	// 快慢指针同步前进，直到 fast 走到尾部节点
	slow := head
	for fast.Next != nil {
		fast = fast.Next
		slow = slow.Next
	}
	// 此时慢指针 slow 的下一个节点就是旋转之后的新头，原来尾结点 fast 需要串联到老的头 head 上
	newHead := slow.Next
	slow.Next = nil
	fast.Next = head
	return newHead
}
