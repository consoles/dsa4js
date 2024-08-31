package main

import "fmt"

func findPeakElement(nums []int) int {
	// 数组的最大值一定是峰值元素
	idx := 0
	for i, v := range nums {
		if v > nums[idx] {
			idx = i
		}
	}
	return idx
}

func findPeakElement2(nums []int) int {
	// 二分法
	l, r := 0, len(nums)-1
	for l < r {
		mid := l + (r-l)/2
		if nums[mid] > nums[mid+1] {
			// 峰值可能在 mid 或者 mid 左侧
			r = mid
		} else {
			// 峰值一定在 mid 右侧
			l = mid + 1
		}
	}
	return r
}

func findPeakElement3(nums []int) int {
	// 使用线性扫描进行模拟
	n := len(nums)
	if n == 1 {
		return 0
	}
	for i := 0; i < n; i++ {
		// 针对头尾元素进行特殊处理
		if i == 0 {
			if i+1 < n && nums[i] > nums[i+1] {
				return 0
			}
		}
		if i == n-1 {
			if i-1 >= 0 && nums[i] > nums[i-1] {
				return n - 1
			}
		}
		if (i-1 >= 0 && nums[i] > nums[i-1]) && (i+1 < n) && nums[i] > nums[i+1] {
			return i
		}
	}
	return -1
}

func findPeakElement4(nums []int) int {
	// 还是模拟，简化方法 3 中很多的特殊判断
	n := len(nums)
	for i := 0; i < n; i++ {
		ok := true
		if i-1 >= 0 && nums[i] <= nums[i-1] {
			ok = false
		}
		if i+1 < n && nums[i] <= nums[i+1] {
			ok = false
		}
		if ok {
			return i
		}
	}
	return -1
}

func main() {
	nums := []int{1, 2, 3, 1}
	r := findPeakElement3(nums)
	fmt.Println(r)
}
