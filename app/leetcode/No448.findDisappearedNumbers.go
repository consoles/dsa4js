package main

import "fmt"

// 给你一个含 n 个整数的数组 nums ，其中 nums[i] 在区间 [1, n] 内。请你找出所有在 [1, n] 范围内但没有出现在 nums 中的数字，并以数组的形式返回结果。
func findDisappearedNumbers(nums []int) []int {
	// 将所有出现过的元素放入 map 中
	numMap := make(map[int]struct{})
	for _, num := range nums {
		numMap[num] = struct{}{}
	}
	n := len(nums)
	res := []int{}
	for i := 1; i <= n; i++ {
		if _, ok := numMap[i]; ok {
			continue
		}
		res = append(res, i)
	}
	return res
}

func main() {
	// nums := []int{4, 3, 2, 7, 8, 2, 3, 1}
	nums := []int{1, 1}
	r := findDisappearedNumbers(nums)
	fmt.Println(r)
}
