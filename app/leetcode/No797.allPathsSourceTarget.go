package main

import "fmt"

func allPathsSourceTarget(graph [][]int) [][]int {
	res := make([][]int, 0)
	end := len(graph) - 1
	var dfs func(int, []int)
	dfs = func(index int, path []int) {
		if index == end {
			path = append(path, index)
			newPath := make([]int, len(path))
			copy(newPath, path)
			res = append(res, newPath)
			return
		}
		curNode := graph[index]
		for _, next := range curNode {
			oldLength := len(path)
			path = append(path, index)
			dfs(next, path)
			path = path[:oldLength]
		}
	}

	dfs(0, []int{})
	return res
}

func allPathsSourceTarget2(graph [][]int) [][]int {
	res := [][]int{}
	stack := []int{0}
	var dfs func(int)
	dfs = func(i int) {
		if i == len(graph)-1 {
			// []int(nil) 创建一个新切片
			// 并将 stack 中的所有元素追加到这个新切片中
			res = append(res, append([]int(nil), stack...))
		}
		for _, x := range graph[i] {
			stack = append(stack, x)
			dfs(x)
			stack = stack[:len(stack)-1]
		}
	}
	dfs(0)
	return res
}

func main() {
	// graph := [][]int{
	// 	[]int{1, 2},
	// 	[]int{3},
	// 	[]int{3},
	// 	[]int{},
	// }

	graph := [][]int{
		[]int{4, 3, 1},
		[]int{3, 2, 4},
		[]int{3},
		[]int{4},
		[]int{},
	}
	r := allPathsSourceTarget(graph)
	fmt.Println(r)
}
