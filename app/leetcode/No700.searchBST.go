package main

type TreeNode struct {
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

/**
 * Definition for a binary tree node.
 * type TreeNode struct {
 *     Val int
 *     Left *TreeNode
 *     Right *TreeNode
 * }
 */
func searchBST(root *TreeNode, val int) *TreeNode {
	if root == nil {
		return root
	}
	rootVal := root.Val
	if rootVal == val {
		return root
	}
	if val < rootVal {
		return searchBST(root.Left, val)
	}
	return searchBST(root.Right, val)
}
