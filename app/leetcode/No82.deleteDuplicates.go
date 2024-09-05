package main

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

/**
 * Definition for singly-linked list.
 * type ListNode struct {
 *     Val int
 *     Next *ListNode
 * }
 */
func deleteDuplicates(head *ListNode) *ListNode {
	dummy := &ListNode{
		Next: head,
	}
	prev := dummy
	cur := head
	for cur != nil {
		hasDup := false
		// 先把重复元素删掉，然后再删除掉用于比较的元素
		for cur.Next != nil && cur.Val == cur.Next.Val {
			cur.Next = cur.Next.Next
			hasDup = true
		}
		if hasDup {
			prev.Next = cur.Next
		} else {
			prev = cur
		}
		cur = cur.Next
	}
	return dummy.Next
}

func deleteDuplicates2(head *ListNode) *ListNode {
	// https://www.bilibili.com/video/BV12S4y1N7Ya
	dummy := &ListNode{
		Next: head,
	}
	p := dummy
	for p.Next != nil {
		q := p.Next
		for q.Next != nil && q.Next.Val == q.Val {
			q = q.Next
		}
		if q == p.Next {
			// 没有重复节点
			p = p.Next
		} else {
			// 跳过中间的重复节点
			p.Next = q.Next
		}
	}
	return dummy.Next
}

func main() {
	head := MakeLinkedList([]int{1, 2, 3, 3, 4, 4, 5})
	PrintLinkedList(head)
	h2 := deleteDuplicates2(head)
	PrintLinkedList(h2)
}
