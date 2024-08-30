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
func reverseBetween(head *ListNode, left int, right int) *ListNode {
	if head == nil || head.Next == nil {
		return head
	}
	// 使用虚拟头结点可以避免复杂的分类讨论问题
	dummy := &ListNode{Next: head}
	prev := dummy
	// 1. 从虚拟头结点走 left - 1 步，来到 left 节点的前一个节点
	for i := 0; i < left-1; i++ {
		prev = prev.Next
	}
	// 2. 从 left 节点的前一个节点再走 right - left + 1 步来到 right 节点
	rightNode := prev
	for i := 0; i < right-left+1; i++ {
		rightNode = rightNode.Next
	}
	// 3. 把 [left, right] 拆分出来形成一个新的链表
	leftNode := prev.Next
	succ := rightNode.Next
	prev.Next = nil
	rightNode.Next = nil
	// 4. 翻转链表 [left, right]
	// 新链表的头是原来的 rightNode，尾是原来的 left 节点
	reverseList(leftNode)
	// 5. 将翻转后的新链表接到原来的链表中
	prev.Next = rightNode
	leftNode.Next = succ
	return dummy.Next
}

func reverseBetween2(head *ListNode, left, right int) *ListNode {
	dummy := &ListNode{Next: head}
	prev := dummy
	for i := 0; i < right-left-1; i++ {
		prev = prev.Next
	}
	// 头插法
	// prev 保持不变，始终指向 left 节点的前一个节点

	cur := prev.Next
	for i := 0; i < right-left; i++ {
		next := cur.Next
		cur.Next = next.Next
		next.Next = prev.Next
		prev.Next = next
	}
	return dummy.Next
}

func reverseList(head *ListNode) *ListNode {
	if head == nil || head.Next == nil {
		return head
	}
	var prev *ListNode
	cur := head
	for cur != nil {
		next := cur.Next
		cur.Next = prev
		prev = cur
		cur = next
	}
	return prev
}

func PrintList(head *ListNode) {
	for head != nil {
		fmt.Printf("%d ", head.Val)
		head = head.Next
	}
	fmt.Println()
}

func main() {
	head := &ListNode{Val: 1, Next: &ListNode{Val: 2, Next: &ListNode{Val: 3, Next: &ListNode{Val: 4, Next: &ListNode{Val: 5}}}}}
	PrintList(head)
	// head = reverseBetween(head, 2, 4)
	h2 := reverseList(head)
	PrintList(h2)
}
