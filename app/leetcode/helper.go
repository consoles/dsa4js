package main

import (
	"fmt"
	"strings"
)

type ListNode struct {
	Val  int
	Next *ListNode
}

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
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

// printNode 打印单个节点，并根据层级调整前缀
// 左子节点 ++
// 右子节点 --
func printNode(node *TreeNode, level int, isLeft bool) {
	token := "--"
	if isLeft {
		token = "++"
	}
	padding := strings.Repeat(token, level)
	fmt.Printf("%s%d\n", padding, node.Val)
}

func printTree(root *TreeNode, level int, isLeft bool) {
	if root == nil {
		return
	}
	printNode(root, level, isLeft)
	printTree(root.Left, level+1, true)
	printTree(root.Right, level+1, false)
}

func PrintBinaryTree(root *TreeNode) {
	printTree(root, 0, false)
}

func main() {
	// root := &TreeNode{
	// 	Val: 1,
	// 	Left: &TreeNode{
	// 		Val:   2,
	// 		Left:  &TreeNode{Val: 4},
	// 		Right: &TreeNode{Val: 5},
	// 	},
	// 	Right: &TreeNode{
	// 		Val:   3,
	// 		Right: &TreeNode{Val: 6},
	// 	},
	// }
	// PrintBinaryTree(root)
}
