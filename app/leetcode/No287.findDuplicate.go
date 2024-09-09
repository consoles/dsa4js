package main

import "sort"

func findDuplicate(nums []int) int {
	numSet := make(map[int]struct{})
	for _, num := range nums {
		if _, ok := numSet[num]; ok {
			return num
		}
		numSet[num] = struct{}{}
	}
	return -1
}

func findDuplicate2(nums []int) int {
	// 排序，重复的肯定是相邻的
	sort.Ints(nums)
	for i := 1; i < len(nums); i++ {
		if nums[i] == nums[i-1] {
			return nums[i]
		}
	}
	return -1
}

// TODO: 二分法寻找重复的数字
