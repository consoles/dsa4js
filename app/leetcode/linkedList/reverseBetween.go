package main

import "fmt"

type ListNode struct {
	Val  int
	Next *ListNode
}

// 翻转链表的 [left, right] 部分
func reverseBetween(head *ListNode, left, right int) *ListNode {
	// 这种思想先将链表的局部转化为值的数组，局部将数据进行翻转，最后修改原来链表中的值
	// 其实是不规范的：应该直接修改原来链表的指向
	if left >= right {
		return head
	}
	// 朴素的想法: 将 [left, right] 反转
	arr := make([]int, 0)
	count := 1
	cur := head
	for cur != nil {
		if count >= left && count <= right {
			arr = append(arr, cur.Val)
		}
		cur = cur.Next
		count++
	}
	// 翻转数组
	for i, j := 0, len(arr)-1; i < j; i, j = i+1, j-1 {
		arr[i], arr[j] = arr[j], arr[i]
	}
	// 恢复链表
	cur = head
	count = 1
	for cur != nil {
		if count >= left && count <= right {
			cur.Val = arr[count-left]
		}
		cur = cur.Next
		count++
	}

	return head
}

func reverseBetween2(head *ListNode, left, right int) *ListNode {
	if left >= right {
		return head
	}
	// 找到 left 的前一个节点 prev

	return nil
}

func reverseList(head *ListNode) *ListNode {
	prev, cur := (*ListNode)(nil), head
	for cur != nil {
		next := cur.Next
		cur.Next = prev
		prev = cur
		cur = next
	}
	return prev
}

func reverseList2(head *ListNode) *ListNode {
	if head == nil || head.Next == nil {
		return head
	}
	next := head.Next
	newHead := reverseList2(next)
	next.Next = head
	head.Next = nil
	return newHead
}

// PrintList 打印链表
func PrintList(head *ListNode) {
	current := head
	for current != nil {
		fmt.Print(current.Val)
		if current.Next != nil {
			fmt.Print(" -> ")
		}
		current = current.Next
	}
	fmt.Println() // 换行
}

func main() {
	h := &ListNode{Val: 1, Next: &ListNode{Val: 2, Next: &ListNode{Val: 3, Next: &ListNode{Val: 4, Next: &ListNode{Val: 5, Next: nil}}}}}

	// reverseBetween(h, 2, 4)
	h2 := reverseList2(h)
	PrintList(h2)
}
