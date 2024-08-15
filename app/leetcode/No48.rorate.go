package main

import "fmt"

func rotate(matrix [][]int) {
	// 第一行，最后一列
	// 第二行，倒数第二列
	// 第 x 行 y 列的元素 => 倒数第 x 列 y 行的元素
	// (i,j) => (j,n-1-i)
	// 使用辅助数组解决问题
	n := len(matrix)
	m := make([][]int, n)
	for i := 0; i < n; i++ {
		m[i] = make([]int, n)
	}
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			m[j][n-1-i] = matrix[i][j]
		}
	}
	// fill back matrix
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			matrix[i][j] = m[i][j]
		}
	}
}

func rotate2(matrix [][]int) {
	// step1: 转置（对角线翻转）
	// step2: 左右翻转
	n := len(matrix)
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			matrix[i][j], matrix[j][i] = matrix[j][i], matrix[i][j]
		}
	}
	for i := 0; i < n; i++ {
		for l, r := 0, n-1; l < r; l, r = l+1, r-1 {
			matrix[i][l], matrix[i][r] = matrix[i][r], matrix[i][l]
		}
	}
}

func main() {
	matrix := [][]int{
		{1, 2, 3},
		{4, 5, 6},
		{7, 8, 9},
	}
	rotate2(matrix)
	// [[7 4 1] [8 5 2] [9 6 3]]
	fmt.Println(matrix)
}
