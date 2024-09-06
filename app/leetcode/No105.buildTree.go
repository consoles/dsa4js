package main

import (
	"fmt"
	"strings"
)

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

// 从前序与中序遍历序列构造二叉树
/**
 * Definition for a binary tree node.
 * type TreeNode struct {
 *     Val int
 *     Left *TreeNode
 *     Right *TreeNode
 * }
 */
func buildTree(preorder []int, inorder []int) *TreeNode {
	if len(preorder) == 0 {
		return nil
	}
	// preorder: [root, left, right]
	// inorder: [left, root, right]
	// 由于数组中没有重复元素，我们遍历 inorder 找到 root 就能划分出左右子树
	rootVal := preorder[0]
	root := &TreeNode{
		Val: rootVal,
	}
	index := 0
	for i := 0; i < len(inorder); i++ {
		if inorder[i] == rootVal {
			index = i
			break
		}
	}
	inOrderLeft := inorder[:index]
	inOrderRight := inorder[index+1:]
	leftCount := len(inOrderLeft)
	preOrderLeft := preorder[1 : leftCount+1]
	preOrderRight := preorder[1+leftCount:]
	root.Left = buildTree(preOrderLeft, inOrderLeft)
	root.Right = buildTree(preOrderRight, inOrderRight)
	return root
}

// [pl, pr] preorder 的有效范围
// [il, ir] inorder 的有效范围
func _buildTree(preorder []int, inorder []int, posMap map[int]int, pl, pr int, il, ir int) *TreeNode {
	if pl > pr || il > ir {
		return nil
	}
	rootVal := preorder[pl]
	pos := posMap[rootVal] // 根节点在中序序列中的位置
	k := pos - il          // 左子树节点数量

	root := &TreeNode{
		Val:   rootVal,
		Left:  _buildTree(preorder, inorder, posMap, pl+1, pl+k, il, il+k-1),
		Right: _buildTree(preorder, inorder, posMap, pl+k+1, pr, il+k+1, ir),
	}
	return root
}

func buildTree2(preorder []int, inorder []int) *TreeNode {
	// 上面的每次递归需要知道根节点在中序数组中的位置，每次需要 O(N) 时间查找，可以使用 hash map 将这个过程优化成 O(1)
	// 同时，方法 1 每次 slice 缩小原数组的过程中涉及到拷贝，可以优化
	n := len(inorder)
	posMap := make(map[int]int)
	for i := 0; i < n; i++ {
		posMap[inorder[i]] = i
	}
	return _buildTree(preorder, inorder, posMap, 0, n-1, 0, n-1)
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
	preorder := []int{3, 9, 20, 15, 7}
	inorder := []int{9, 3, 15, 20, 7}
	root := buildTree2(preorder, inorder)
	PrintBinaryTree(root)
}
