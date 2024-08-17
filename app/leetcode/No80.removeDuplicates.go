package main

import "fmt"

// 给你一个有序数组 nums ，请你 原地 删除重复出现的元素，使得出现次数超过两次的元素只出现两次 ，返回删除后数组的新长度。
// 不要使用额外的数组空间，你必须在 原地 修改输入数组 并在使用 O(1) 额外空间的条件下完成。
func removeDuplicates(nums []int) int {
	// 使用计数器,记录每个元素出现的次数
	// 额外使用了 O(n) 的空间
	counter := make(map[int]int)
	l := 0
	for _, v := range nums {
		counter[v]++
		if counter[v] <= 2 {
			nums[l] = v
			l++
		}
	}
	return l
}

func removeDuplicates2(nums []int) int {
	// 注意条件中的有序数组
	i := 0
	for _, v := range nums {
		if i < 2 || nums[i-2] != v {
			nums[i] = v
			i++
		}
	}
	return i
}

func removeDuplicates3(nums []int) int {
	// 上面的算法可以泛化解决从有序数组中删除重复元素，但是保留k次重复的逻辑
	k := 2
	i := 0
	for _, v := range nums {
		if i < k || nums[i-k] != v {
			nums[i] = v
			i++
		}
	}
	return i
}

func removeDuplicates4(nums []int) int {
	// 快慢指针：
	// 移动快指针来找到新元素，将新元素复制到慢指针的位置上，从而在原地删除重复项
	// 当找到一个与nums[slow-k]不同的元素时，我们将新元素复制到nums[slow]

	const k = 2
	if len(nums) < k {
		return len(nums)
	}

	slow, fast := k, k
	for fast < len(nums) {
		if nums[fast] != nums[slow-k] {
			nums[slow] = nums[fast]
			slow++
		}
		fast++
	}
	return slow
}

func main() {
	nums := []int{0, 0, 1, 1, 1, 1, 2, 3, 3}
	r := removeDuplicates3(nums)
	fmt.Println(nums, r)
	nums = []int{1, 1, 1, 2, 2, 3}
	r = removeDuplicates3(nums)
	fmt.Println(nums, r)
}
