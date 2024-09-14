package main

import "fmt"

type Node struct {
	Val      int
	Children []*Node
}

/**
 * Definition for a Node.
 * type Node struct {
 *     Val int
 *     Children []*Node
 * }
 */

func dfs(root *Node, vals *[]int) {
	if root != nil {
		*vals = append(*vals, root.Val)
		for _, node := range root.Children {
			dfs(node, vals)
		}
	}
}

func dfs2(root *Node, vals []int) []int {
	if root != nil {
		vals = append(vals, root.Val)
		for _, node := range root.Children {
			vals = dfs2(node, vals)
		}
	}
	return vals
}

func preorder(root *Node) []int {
	// 注意 golang 中的数组是值传递
	res := make([]int, 0)
	dfs(root, &res)
	return res
}

func preorder2(root *Node) []int {
	return dfs2(root, make([]int, 0))
}

func preorder3(root *Node) []int {
	res := make([]int, 0)
	var dfs func(*Node)
	dfs = func(node *Node) {
		if node == nil {
			return
		}
		res = append(res, node.Val)
		for _, ch := range node.Children {
			dfs(ch)
		}
	}
	dfs(root)
	return res
}

func preorder4(root *Node) []int {
	var res []int
	if root == nil {
		return res
	}
	stack := []*Node{root}
	for len(stack) > 0 {
		node := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		res = append(res, node.Val)
		// 栈是后入先出，所以为了模拟和递归一样的效果，每个孩子节点逆序入栈
		for i := len(node.Children) - 1; i >= 0; i-- {
			stack = append(stack, node.Children[i])
		}
	}
	return res
}

func main() {
	root := &Node{
		Val: 1,
		Children: []*Node{
			&Node{
				Val: 3,
				Children: []*Node{
					&Node{
						Val: 5, Children: []*Node{},
					},
					&Node{
						Val: 6, Children: []*Node{},
					},
				},
			},
			&Node{
				Val:      2,
				Children: []*Node{},
			},
			&Node{
				Val:      4,
				Children: []*Node{},
			},
		},
	}
	r := preorder3(root)
	fmt.Println(r)
}
