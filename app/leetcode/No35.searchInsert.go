package main

// 返回排序数组中的插入位置
func searchInsert(nums []int, target int) int {
	l, r := 0, len(nums)-1
	for l <= r {
		mid := l + (r-l)/2
		if nums[mid] == target {
			return mid
		}
		if nums[mid] < target {
			l = mid + 1
		} else {
			r = mid - 1
		}
	}
	return l
}

func searchInsert2(nums []int, target int) int {
	n := len(nums)
	l, r := 0, n-1
	res := n
	for l <= r {
		mid := l + (r-l)/2
		if target <= nums[mid] {
			r = mid - 1
			res = mid
		} else {
			l = mid + 1
		}
	}
	return res
}
