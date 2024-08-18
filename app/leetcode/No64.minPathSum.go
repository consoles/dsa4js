package main

import "fmt"

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func dfs(grid [][]int, curSum, i, j int) int {
	// 使用 dfs 会超时
	s := curSum + grid[i][j]
	if i == len(grid)-1 && j == len(grid[0])-1 {
		return s
	}

	if i == len(grid)-1 {
		return dfs(grid, s, i, j+1)
	}

	if j == len(grid[0])-1 {
		return dfs(grid, s, i+1, j)
	}
	// 尝试向右和向下走
	rightMin := dfs(grid, s, i, j+1)
	downMin := dfs(grid, s, i+1, j)
	return min(rightMin, downMin)
}

func minPathSum(grid [][]int) int {
	// 给定一个包含非负整数的 m x n 网格 grid ，请找出一条从左上角到右下角的路径，使得路径上的数字总和为最小
	m := len(grid)
	if m <= 0 {
		return 0
	}
	n := len(grid[0])
	if n <= 0 {
		return 0
	}
	// 从 (0,0) 到 (m-1,n-1)
	return dfs(grid, 0, 0, 0)
}

func minPathSum2(grid [][]int) int {
	// 路径的方向只能是向下或者向右
	// 网格中第一行中的每个元素只能从(0,0) 向右到达
	// 第一列中的每个元素只能从(0,0) 向下到达
	// 以上两种情况路径是唯一的，第一行和第一列中每个元素的最小路径和就是对应路径上的数字总和

	// 对于不在第一行和第一列的元素，有两种选择：
	// 从左侧元素向右一步到达
	// 从上方元素向下一步到达
	// 最小路径和等于这两种情况中较小的 + 当前元素
	// 由于每个元素对应的最小路径和与其相邻元素的最小路径和有关，可以使用 dp 来求解
	m := len(grid)
	if m <= 0 {
		return 0
	}
	n := len(grid[0])
	if n <= 0 {
		return 0
	}
	// dp[i][j] 表示从 (0,0) 出发到 (i,j) 的最小路径和
	dp := make([][]int, m)
	for i := 0; i < m; i++ {
		dp[i] = make([]int, n)
	}
	dp[0][0] = grid[0][0]
	// 第一列的最小路径和
	for i := 1; i < m; i++ {
		dp[i][0] = dp[i-1][0] + grid[i][0]
	}
	// 第一行的最小路径和
	for j := 1; j < n; j++ {
		dp[0][j] = dp[0][j-1] + grid[0][j]
	}
	for i := 1; i < m; i++ {
		for j := 1; j < n; j++ {
			dp[i][j] = min(dp[i-1][j], dp[i][j-1]) + grid[i][j]
		}
	}
	return dp[m-1][n-1]
}

func main() {
	grid := [][]int{{1, 3, 1}, {1, 5, 1}, {4, 2, 1}}
	fmt.Println(minPathSum2(grid))
	grid = [][]int{{1, 2, 3}, {4, 5, 6}}
	fmt.Println(minPathSum2(grid))
	grid = [][]int{{1, 2}, {1, 1}}
	fmt.Println(minPathSum2(grid))
}
