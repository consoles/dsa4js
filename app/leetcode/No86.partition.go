package main

import (
	"fmt"
)

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

/**
 * Definition for singly-linked list.
 * type ListNode struct {
 *     Val int
 *     Next *ListNode
 * }
 */
func partition(head *ListNode, x int) *ListNode {
	dummyLeft := &ListNode{}
	dummyRight := &ListNode{}
	left := dummyLeft
	right := dummyRight
	cur := head
	for cur != nil {
		if cur.Val < x {
			left.Next = cur
			left = left.Next
		} else {
			right.Next = cur
			right = right.Next
		}
		cur = cur.Next
	}
	right.Next = nil
	left.Next = dummyRight.Next
	return dummyLeft.Next
}

func main() {
	h := MakeLinkedList([]int{1, 4, 3, 2, 5, 2})
	PrintLinkedList(h)
	h2 := partition(h, 3)
	PrintLinkedList(h2)
}
