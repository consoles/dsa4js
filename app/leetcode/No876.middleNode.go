package main

import "fmt"

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
func middleNode(head *ListNode) *ListNode {
	// 最朴素的解法，先计算出链表长度
	if head == nil || head.Next == nil {
		return head
	}
	length := 0
	cur := head
	for cur != nil {
		length++
		cur = cur.Next
	}
	// len: 5 -> 2
	// len: 6 -> 3
	count := 0
	cur = head
	for count < length/2 {
		cur = cur.Next
		count++
	}
	return cur
}

func middleNode2(head *ListNode) *ListNode {
	// 快慢指针
	fast, slow := head, head
	for fast != nil && fast.Next != nil {
		fast = fast.Next.Next
		slow = slow.Next
	}
	return slow
}

func MakeLinkedList(nums []int) *ListNode {
	dummy := &ListNode{}
	cur := dummy
	for _, v := range nums {
		node := &ListNode{
			Val: v,
		}
		cur.Next = node
		cur = cur.Next
	}
	return dummy.Next
}

func main() {
	head := MakeLinkedList([]int{1, 2, 3, 4, 5})
	// head := MakeLinkedList([]int{1, 2, 3, 4, 5, 6})
	r := middleNode(head)
	fmt.Println(r)
}
