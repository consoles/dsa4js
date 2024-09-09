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

func rotate4(nums []int, k int) {
	// 新声明 1 个数组，将新的数组追加到原来数组的后面，利用滑动窗的概念
	// 保持滑动窗的大小为数组元素的长度
	size := len(nums)
	k = k % size
	numsCopy := make([]int, 2*size)
	for i := 0; i < 2*size; i++ {
		index := i % size
		numsCopy[i] = nums[index]
	}
	// [1,2,3,4] => [1,2,3,4,1,2,3,4]
	// k = 0, [1,2,3,4]
	// k = 1, [4,1,2,3]
	index := size - 1
	for i := 2*size - k - 1; index >= 0; index-- {
		nums[index] = numsCopy[i]
		i--
	}
}

func main() {
	nums := []int{1, 2, 3, 4, 5, 6, 7}
	rotate4(nums, 3)
	fmt.Println(nums)
	nums = []int{-1, -100, 3, 99}
	rotate4(nums, 2)
	nums = []int{1}
	rotate4(nums, 0)
	fmt.Println(nums)
}
