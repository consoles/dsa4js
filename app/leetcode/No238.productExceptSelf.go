package main

import "fmt"

func productExceptSelf(nums []int) []int {
	// 暴力法
	res := make([]int, len(nums))
	for i := 0; i < len(nums); i++ {
		r := 1
		for j := 0; j < len(nums); j++ {
			if i == j {
				continue
			}
			r *= nums[j]
		}
		res[i] = r
	}
	return res
}

func productExceptSelf2(nums []int) []int {
	// 前缀积和后缀积
	// prefix[i] = nums[0] * nums[1] * ... * nums[i-1]
	// suffix[i] = nums[i+1] * nums[i+2] * ... * nums[n-1]
	// res[i] = prefix[i] * suffix[i]
	n := len(nums)
	res := make([]int, n)
	prefix := make([]int, n)
	suffix := make([]int, n)
	// init
	prefix[0] = 1
	suffix[n-1] = 1
	// calc prefix
	for i := 1; i < n; i++ {
		prefix[i] = prefix[i-1] * nums[i-1]
	}
	// calc suffix
	for i := n - 2; i >= 0; i-- {
		suffix[i] = suffix[i+1] * nums[i+1]
	}
	// calc res
	for i := 0; i < n; i++ {
		res[i] = prefix[i] * suffix[i]
	}
	return res
}

func main() {
	// nums := []int{1, 2, 3, 4}
	nums := []int{-1, 1, 0, -3, 3}
	r := productExceptSelf(nums)
	fmt.Println(r)
}
