package helper

import "fmt"

type ListNode struct {
	Val  int
	Next *ListNode
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

func PrintLinkedList(head *ListNode) {
	cur := head
	for cur != nil {
		fmt.Print(cur.Val, "->")
		cur = cur.Next
	}
	fmt.Println("NULL")
}
