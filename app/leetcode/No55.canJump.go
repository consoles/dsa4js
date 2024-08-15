package main

import "fmt"

var canGo = false

func dfs(nums []int, index int) {
	if index >= len(nums)-1 {
		canGo = true
		return
	}
	// 尝试所有可能的跳跃方式，优先尝试一次性跳跃的比较长一点
	for i := nums[index]; i >= 1; i-- {
		dfs(nums, index+i)
	}
}

func canJump(nums []int) bool {
	// 使用 dfs 尝试所有可能的跳跃方式
	canGo = false
	dfs(nums, 0)
	return canGo
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func canJump2(nums []int) bool {
	// 不妨可以想象有一个“右边界”的存在，遍历只是为了拓展右边界的范围，直至可以覆盖到数组的最后一个元素。
	// 遍历完当前右边界内的元素，仍不能使右边界拓展时，退出循环
	// 当遍历过程中，发现右边界已可以覆盖数组的最后一个元素，退出循环
	n := len(nums)
	rightMost := 0
	for i := 0; i < n; i++ {
		if i <= rightMost {
			rightMost = max(rightMost, i+nums[i])
			if rightMost >= n-1 {
				return true
			}
		}
	}
	return false
}

func main() {
	nums := []int{2, 3, 1, 1, 4}
	ok := canJump2(nums)
	fmt.Println(ok)
	nums = []int{3, 2, 1, 0, 4}
	ok = canJump2(nums)
	fmt.Println(ok)
	// dfs 超时
	nums = []int{
		8, 2, 4, 4, 4, 9, 5, 2, 5, 8, 8, 0, 8, 6, 9, 1, 1, 6, 3, 5, 1, 2, 6, 6, 0, 4, 8, 6, 0, 3, 2, 8, 7, 6, 5, 1, 7, 0, 3, 4, 8, 3, 5, 9, 0, 4, 0, 1, 0, 5, 9, 2, 0, 7, 0, 2, 1, 0, 8, 2, 5, 1, 2, 3, 9, 7, 4, 7, 0, 0, 1, 8, 5, 6, 7, 5, 1, 9, 9, 3, 5, 0, 7, 5,
	}
	ok = canJump2(nums)
	fmt.Println(ok)
}
