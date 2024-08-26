package main

import "fmt"

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func rob(nums []int) int {
	if len(nums) == 0 {
		return 0
	}
	// 只有一件房屋，则偷窃该房屋
	if len(nums) == 1 {
		return nums[0]
	}
	// 只有两件房屋，则选择其中最大值
	if len(nums) == 2 {
		return max(nums[0], nums[1])
	}
	// 当前房间偷不偷和前 1 个房间、前 2 个房间有关
	// 3 间则有 2 种选择 (0,2), (1) 取最大值
	// 4 间则是 (0,2),(0,3),(1,3),(2)
	// 对于 k 间房屋有 2 个选择，偷窃第 k 间，就不能偷窃 k-1 间，总金额为 k-2 间房屋最大金额 + k 间房屋金额
	// 不偷窃第 k 间房屋，偷窃总金额为前 k - 1 间房间的最高总金额
	// dp[i] 表示前 i 间房屋所能偷窃到的最高总金额，那么有如下状态转移方程
	// dp[i] = max(dp[i-2]+nums[i], dp[i-1])
	// dp[0] = nums[0]
	// dp[1] = max(nums[0], nums[1])
	dp := make([]int, len(nums))
	dp[0] = nums[0]
	dp[1] = max(nums[0], nums[1])
	for i := 2; i < len(nums); i++ {
		// 偷第 i 个房间，则 i-1 一定不能偷
		// 不偷第 i 个房间，可以考虑 i - 1 个房间
		dp[i] = max(dp[i-2]+nums[i], dp[i-1])
	}
	fmt.Println(dp)
	return dp[len(nums)-1]
}

func main() {
	nums := []int{2, 7, 9, 3, 1}
	fmt.Println(rob(nums))
}
