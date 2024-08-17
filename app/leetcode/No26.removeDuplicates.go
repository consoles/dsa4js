package main

import "fmt"

func removeDuplicates(nums []int) int {
	if len(nums) == 0 {
		return 0
	}
	// 利用 set 去重
	set := make(map[int]bool)
	l := 0
	for i := 0; i < len(nums); i++ {
		if !set[nums[i]] {
			set[nums[i]] = true
			nums[l] = nums[i]
			l++
		}
	}
	return l
}

func removeDuplicates2(nums []int) int {
	if len(nums) == 0 {
		return 0
	}
	// 题目说是非严格单调递增，因此后一个元素要么一定大于等于前一个元素 即 1,1,2 是可能得， 1,2,2,1 是不可能的
	// 也就是说如果出现重复元素，则重复的元素一定是相邻的
	k := 1
	for i := 1; i < len(nums); i++ {
		if nums[i] != nums[i-1] {
			nums[k] = nums[i]
			k++
		}
	}
	return k
}

func main() {
	nums := []int{0, 0, 1, 1, 1, 2, 2, 3, 3, 4}
	r := removeDuplicates2(nums)
	fmt.Println(r, nums)
	nums = []int{1, 1, 2}
	r = removeDuplicates2(nums)
	fmt.Println(r, nums)
}
