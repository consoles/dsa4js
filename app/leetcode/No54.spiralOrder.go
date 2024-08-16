package main

import "fmt"

func spiralOrder(matrix [][]int) []int {
	// 模拟
	if len(matrix) == 0 || len(matrix[0]) == 0 {
		return []int{}
	}
	dirs := [][]int{
		{0, 1},  // right
		{1, 0},  // down
		{0, -1}, // left
		{-1, 0}, // up
	}
	m, n := len(matrix), len(matrix[0])

	arr := make([]int, m*n)
	visited := make([][]bool, m)
	for i := 0; i < m; i++ {
		visited[i] = make([]bool, n) // 默认都是false
	}
	dirIndex := 0
	x, y := 0, 0 // 起始位置
	for i := 0; i < m*n; i++ {
		arr[i] = matrix[x][y]
		visited[x][y] = true
		xNew, yNew := x+dirs[dirIndex][0], y+dirs[dirIndex][1]
		// 碰撞检测
		if xNew < 0 || xNew >= m || yNew < 0 || yNew >= n || visited[xNew][yNew] {
			dirIndex = (dirIndex + 1) % 4
		}
		x, y = x+dirs[dirIndex][0], y+dirs[dirIndex][1]
	}
	return arr
}

func spiralOrder2(matrix [][]int) []int {
	if len(matrix) == 0 || len(matrix[0]) == 0 {
		return []int{}
	}
	m, n := len(matrix), len(matrix[0])
	arr := make([]int, m*n)
	index := 0
	left, right, top, bottom := 0, n-1, 0, m-1
	for left <= right && top <= bottom {
		for col := left; col <= right; col++ {
			arr[index] = matrix[top][col]
			index++
		}
		for row := top + 1; row <= bottom; row++ {
			arr[index] = matrix[row][right]
			index++
		}
		if left < right && top < bottom {
			for col := right - 1; col > left; col-- {
				arr[index] = matrix[bottom][col]
				index++
			}
			for row := bottom; row > top; row-- {
				arr[index] = matrix[row][left]
				index++
			}
		}
		left++
		right--
		top++
		bottom--
	}
	return arr
}

func spiralOrder3(matrix [][]int) []int {
	if len(matrix) == 0 || len(matrix[0]) == 0 {
		return []int{}
	}
	m, n := len(matrix), len(matrix[0])
	arr := make([]int, m*n)
	index := 0
	// 上，右，下，左 4 个边界，由4个边界1层层向内收缩
	left, right, top, bottom := 0, n-1, 0, m-1
	for {
		// left to right
		for i := left; i <= right; i++ {
			arr[index] = matrix[top][i]
			index++
		}
		top++
		if top > bottom {
			break
		}
		// top to bottom
		for i := top; i <= bottom; i++ {
			arr[index] = matrix[i][right]
			index++
		}
		right--
		if left > right {
			break
		}
		// right to left
		for i := right; i >= left; i-- {
			arr[index] = matrix[bottom][i]
			index++
		}
		bottom--
		if top > bottom {
			break
		}
		// bottom to top
		for i := bottom; i >= top; i-- {
			arr[index] = matrix[i][left]
			index++
		}
		left++
		if left > right {
			break
		}
	}
	return arr
}

func main() {
	matrix := [][]int{
		{1, 2, 3},
		{4, 5, 6},
		{7, 8, 9},
	}
	r := spiralOrder3(matrix)
	fmt.Println(r)
}
