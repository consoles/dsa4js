package main

import (
	"fmt"
	"math"
)

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func minSubArrayLen(target int, nums []int) int {
	n := len(nums)
	if n == 0 {
		return 0
	}
	minLen := math.MaxInt
	for i := 0; i < n; i++ {
		// sum(nums[i:j])
		sum := 0
		for j := i; j < n; j++ {
			sum += nums[j]
			if sum >= target {
				minLen = min(minLen, j-i+1)
				break
			}
		}
	}
	if minLen == math.MaxInt {
		return 0
	}
	return minLen
}

func minSubArrayLen2(target int, nums []int) int {
	// 找到和大于等于 target 的长度最小的连续子数组
	n := len(nums)
	if n == 0 {
		return 0
	}
	minLen := math.MaxInt
	// sum = nums[start:end] 的元素和
	// 滑动窗口
	start, end := 0, 0
	sum := 0
	for end < n {
		sum += nums[end]
		// 固定中止位置，不断收缩起始位置
		for sum >= target {
			minLen = min(minLen, end-start+1)
			sum -= nums[start]
			start++
		}
		end++
	}
	if minLen == math.MaxInt {
		return 0
	}
	return minLen
}

func main() {
	// target := 7
	// nums := []int{
	// 	2, 3, 1, 2, 4, 3,
	// }

	// target := 4
	// nums := []int{
	// 	1, 4, 4,
	// }

	// target := 11
	// nums := []int{
	// 	1, 1, 1, 1, 1, 1, 1, 1,
	// }

	// target := 20
	// nums := []int{
	// 	2, 16, 14, 15,
	// }

	target := 213
	nums := []int{
		12, 28, 83, 4, 25, 26, 25, 2, 25, 25, 25, 12,
	}

	r := minSubArrayLen(target, nums)
	fmt.Println(r)
}
