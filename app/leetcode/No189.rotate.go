package main

import (
	"fmt"
	"slices"
)

// 模拟的方法极端情况下会超时
func rotate(nums []int, k int) {
	size := len(nums)
	k = k % size
	// 将最后面的元素放到开头
	for i := 0; i < k; i++ {
		last := nums[size-1]
		for j := size - 1; j >= 1; j-- {
			nums[j] = nums[j-1]
		}
		nums[0] = last
	}
}

func rotate2(nums []int, k int) {
	size := len(nums)
	k = k % size
	// 将原数组下标为 i 位置的元素放到 (i + k) % n 的位置
	arr := make([]int, size)
	for i := 0; i < size; i++ {
		arr[(i+k)%size] = nums[i]
	}
	copy(nums, arr)
}

func rotate3(nums []int, k int) {
	size := len(nums)
	k = k % size
	slices.Reverse(nums)
	slices.Reverse(nums[:k])
	slices.Reverse(nums[k:])
}

func main() {
	nums := []int{1, 2, 3, 4, 5, 6, 7}
	rotate3(nums, 3)
	fmt.Println(nums)
	nums = []int{-1, -100, 3, 99}
	rotate3(nums, 2)
	fmt.Println(nums)
}
