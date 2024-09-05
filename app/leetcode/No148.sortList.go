package main

import "fmt"

type ListNode struct {
	Val  int
	Next *ListNode
}

func printList(head *ListNode) {
	cur := head
	for cur != nil {
		fmt.Print(cur.Val, "->")
		cur = cur.Next
	}
	fmt.Println()
}

func mergeList(l1, l2 *ListNode) *ListNode {
	dummy := &ListNode{}
	cur := dummy
	for l1 != nil && l2 != nil {
		if l1.Val <= l2.Val {
			cur.Next = l1
			l1 = l1.Next
		} else {
			cur.Next = l2
			l2 = l2.Next
		}
		cur = cur.Next
	}
	if l1 != nil {
		cur.Next = l1
	} else {
		cur.Next = l2
	}
	return dummy.Next
}

func mergeList2(l1, l2 *ListNode) *ListNode {
	if l1 == nil {
		return l2
	}
	if l2 == nil {
		return l1
	}
	if l1.Val <= l2.Val {
		head := l1
		head.Next = mergeList2(head.Next, l2)
		return head
	}
	head := l2
	head.Next = mergeList2(l1, l2.Next)
	return head
}

/**
 * Definition for singly-linked list.
 * type ListNode struct {
 *     Val int
 *     Next *ListNode
 * }
 */
func sortList(head *ListNode) *ListNode {
	if head == nil || head.Next == nil {
		return head
	}
	slow, fast := head, head
	var preSlow *ListNode
	for fast != nil && fast.Next != nil {
		preSlow = slow
		slow = slow.Next
		fast = fast.Next.Next
	}
	preSlow.Next = nil   // 断开，分成 2 条链
	h1 := sortList(head) // 已排序的左链
	h2 := sortList(slow) // 已排序的右链
	return mergeList2(h1, h2)
}

func main() {
	head := &ListNode{
		Val: -1,
		Next: &ListNode{
			Val: 5,
			Next: &ListNode{
				Val: 3,
				Next: &ListNode{
					Val: 4,
					Next: &ListNode{
						Val:  0,
						Next: nil,
					},
				},
			},
		},
	}
	h2 := sortList(head)
	printList(h2)
}
