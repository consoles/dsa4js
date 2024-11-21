package main

import "fmt"

func uniquePathsWithObstacles(obstacleGrid [][]int) int {
	// 方法1：dfs
	// 这种方法会超时
	m, n := len(obstacleGrid), len(obstacleGrid[0])
	var dfs func([][]int, int, int, int, int) int
	dfs = func(grid [][]int, m, n, i, j int) int {
		if i >= m || j >= n || grid[i][j] == 1 {
			return 0
		}
		if i == m-1 && j == n-1 {
			return 1
		}
		return dfs(grid, m, n, i+1, j) + dfs(grid, m, n, i, j+1)
	}
	return dfs(obstacleGrid, m, n, 0, 0)
}

func uniquePathsWithObstacles2(obstacleGrid [][]int) int {
	// 方法2：dp
	// https://www.bilibili.com/video/BV1Ld4y1k7c6
	// 状态转移方程：dp[i][j] = dp[i-1][j] + dp[i][j-1]
	m, n := len(obstacleGrid), len(obstacleGrid[0])
	dp := make([][]int, m)
	for i := 0; i < m; i++ {
		dp[i] = make([]int, n)
	}
	// dp 数组初始化
	for i := 0; i < m; i++ {
		if obstacleGrid[i][0] == 1 {
			break
		}
		dp[i][0] = 1
	}
	for j := 0; j < n; j++ {
		if obstacleGrid[0][j] == 1 {
			break
		}
		dp[0][j] = 1
	}
	for i := 1; i < m; i++ {
		for j := 1; j < n; j++ {
			if obstacleGrid[i][j] == 1 {
				continue
			}
			dp[i][j] = dp[i-1][j] + dp[i][j-1]
		}
	}
	return dp[m-1][n-1]
}

func main() {
	obstacleGrid := [][]int{[]int{0, 0, 0}, []int{0, 1, 0}, []int{0, 0, 0}}
	// obstacleGrid := [][]int{[]int{0, 1}, []int{0, 0}}
	r := uniquePathsWithObstacles2(obstacleGrid)
	fmt.Println(r)
}
