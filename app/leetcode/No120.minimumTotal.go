package main

import "math"

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func minimumTotal(triangle [][]int) int {
	// 给定一个三角形 triangle ，找出自顶向下的最小路径和。
	// 每一步只能移动到下一行中相邻的结点上。相邻的结点 在这里指的是 下标 与 上一层结点下标 相同或者等于 上一层结点下标 + 1 的两个结点。也就是说，如果正位于当前行的下标 i ，那么下一步可以移动到下一行的下标 i 或 i + 1 。
	// 要想到达 (i,j) 有两种选择 (i-1,j-1), (i-1,j)
	if len(triangle) == 0 {
		return 0
	}
	// dp[i][j] 表示从 (0,0) 走到 (i,j) 的最小路径和
	dp := make([][]int, len(triangle))
	for i := 0; i < len(triangle); i++ {
		dp[i] = make([]int, len(triangle[i]))
	}
	dp[0][0] = triangle[0][0]
	for i := 1; i < len(triangle); i++ {
		// 最左侧路径
		dp[i][0] = dp[i-1][0] + triangle[i][0]
		for j := 1; j < i; j++ {
			dp[i][j] = min(dp[i-1][j-1], dp[i-1][j]) + triangle[i][j]
		}
		// 最右侧路径，因为最右侧路径只有一种选择(因为上一层缺少一个元素)
		dp[i][i] = dp[i-1][i-1] + triangle[i][i]
	}
	// 遍历 dp[n-1][0], dp[n-1][1] ... dp[n-1][n-1] 找到最小值
	ans := math.MaxInt32
	for i := 0; i < len(triangle); i++ {
		ans = min(ans, dp[len(triangle)-1][i])
	}
	return ans
}

func minimumTotal2(triangle [][]int) int {
	// 自底向上 dp
	n := len(triangle)
	// 遍历到当前层的时候到达每个位置的最短路径
	dp := make([]int, n)
	// 遍历 n-1 层
	for i := 0; i < n; i++ {
		dp[i] = triangle[n-1][i]
	}
	// 遍历 n-2 ~ 0 层
	for i := n - 2; i >= 0; i-- {
		for j := 0; j <= i; j++ {
			dp[j] = min(dp[j], dp[j+1]) + triangle[i][j]
		}
	}
	return dp[0]
}
