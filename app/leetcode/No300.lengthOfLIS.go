package main

import "fmt"

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// https://writings.sh/post/algorithm-longest-increasing-subsequence
// 如果已知一个递增序列和其尾巴元素，向它追加一个值更大的元素，构成的新的子序列也是递增序列。

func lengthOfLIS(nums []int) int {
	// dp[i] 为 nums[i] 结尾的选 nums[i] 的最长子序列的长度
	n := len(nums)
	dp := make([]int, n)
	for i := 0; i < n; i++ {
		dp[i] = 1
	}
	for i := 0; i < n; i++ {
		// dp[j] 表示以 nums[j] 结束的最长子序列的长度
		// dp[i] 则需要考察所有 nums[i] 之前的最长子序列的长度，并且当前元素要大于 nums[i]
		for j := 0; j < i; j++ {
			if nums[j] < nums[i] {
				dp[i] = max(dp[i], dp[j]+1)
			}
		}
	}
	maxLen := 1
	for i := 0; i < n; i++ {
		maxLen = max(maxLen, dp[i])
	}
	return maxLen
}

func lengthOfLIS2(nums []int) int {
	// dp[i] 为 nums[i] 结尾的选 nums[i] 的最长子序列的长度
	n := len(nums)
	dp := make([]int, n)
	maxLen := 1
	for i := 0; i < n; i++ {
		dp[i] = 1
		// dp[j] 表示以 nums[j] 结束的最长子序列的长度
		// dp[i] 则需要考察所有 nums[i] 之前的最长子序列的长度，并且当前元素要大于 nums[i]
		for j := 0; j < i; j++ {
			if nums[j] < nums[i] {
				dp[i] = max(dp[i], dp[j]+1)
			}
		}
		maxLen = max(maxLen, dp[i])
	}
	return maxLen
}

func lengthOfLIS3(nums []int) int {
	// lis 为最长上升子序列
	// 它的构造过程：从前向后找到第一个比当前元素大的元素（因为 lis 是递增的，所以可以用二分查找），进行替换，如果没有找到比它大的元素，则将它追加到 lis 的末尾作为 lis 的最后一个元素
	lis := make([]int, 0)
	for _, v := range nums {
		index := findLowerBounds(lis, v)
		if index == len(lis) {
			lis = append(lis, v)
		} else {
			lis[index] = v
		}
	}
	return len(lis)
}

func findLowerBounds(nums []int, v int) int {
	n := len(nums)
	l, r := 0, n-1
	res := n
	for l <= r {
		mid := l + (r-l)/2
		if v <= nums[mid] {
			res = mid // mid 可能是插入位置
			r = mid - 1
		} else {
			l = mid + 1
		}
	}
	return res
}

func main() {
	nums := []int{10, 9, 2, 5, 3, 7, 101, 18}
	r := lengthOfLIS2(nums)
	fmt.Println(r)

	// nums := []int{1, 2, 3, 4}
	// for i := 0; i <= 5; i++ {
	// 	r := findLowerBounds(nums, i)
	// 	fmt.Println(r)
	// }
}
