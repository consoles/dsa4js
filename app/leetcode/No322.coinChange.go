package main

import (
	"fmt"
	"math"
)

var cache = make(map[int]int, 0)

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func dfs(coins []int, amount int) int {
	if amount < 0 {
		return -1
	}
	if amount == 0 {
		return 0
	}
	if v, ok := cache[amount]; ok {
		return v
	}
	minCount := math.MaxInt
	for _, v := range coins {
		r := dfs(coins, amount-v)
		if r >= 0 && r < minCount {
			minCount = r + 1 // 加1是为了加上得到res结果的那个步骤中，兑换的一个硬币
		}
	}

	if minCount == math.MaxInt {
		cache[amount] = -1
	} else {
		cache[amount] = minCount
	}
	return cache[amount]
}

func coinChange(coins []int, amount int) int {
	if len(coins) == 0 {
		return -1
	}
	cache = make(map[int]int, 0)
	minCount := dfs(coins, amount)
	if minCount == math.MaxInt {
		return -1
	}
	return minCount
}

func coinChange2(coins []int, amount int) int {
	// https://www.bilibili.com/video/BV1qsvDeHEkg
	// dp
	// 假设 coins = [1,2,5], amount = 4
	// dp[4] = min(dp[4-1], dp[4-2]) + 1 之所以不使用 dp[4-5] 是因为越界了
	// dp[0] = 0
	dp := make([]int, amount+1)
	for i, _ := range dp {
		dp[i] = amount + 1 // 赋值 amount + 1，防止下面的运算出现溢出
	}
	dp[0] = 0
	for i := 1; i <= amount; i++ {
		for _, coin := range coins {
			if i-coin >= 0 {
				dp[i] = min(dp[i], dp[i-coin]+1)
			}
		}
	}
	if dp[amount] == amount+1 {
		return -1
	}
	return dp[amount]
}

func main() {
	// coins := []int{1, 2, 5}
	// amount := 11

	coins := []int{2}
	amount := 3
	r := coinChange2(coins, amount)
	fmt.Println(r)
}
