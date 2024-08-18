package main

// 已知一个长度为 n 的数组，预先按照升序排列，经由 1 到 n 次 旋转 后，得到输入数组。例如，原数组 nums = [0,1,2,4,5,6,7] 在变化后可能得到：
// 若旋转 4 次，则可以得到 [4,5,6,7,0,1,2]
// 若旋转 7 次，则可以得到 [0,1,2,4,5,6,7]
// 注意，数组 [a[0], a[1], a[2], ..., a[n-1]] 旋转一次 的结果为数组 [a[n-1], a[0], a[1], a[2], ..., a[n-2]] 。

// 给你一个元素值 互不相同 的数组 nums ，它原来是一个升序排列的数组，并按上述情形进行了多次旋转。请你找出并返回数组中的 最小元素 。

// 你必须设计一个时间复杂度为 O(log n) 的算法解决此问题。

func findMin(nums []int) int {
	// 4,5,6,7 0,1,2,3 最小元素的左右两侧都是有序数组
	low, high := 0, len(nums)-1
	for low < high {
		mid := low + (high-low)/2
		// 右半部分是递增的，最小值在左半边，收缩右边界
		if nums[mid] < nums[high] {
			high = mid // mid 可能是最小值，留给下次循环判断
		} else {
			// 中值大于右值，则经过了上升，下降的过程，最小值肯定在右侧，收缩左边界
			low = mid + 1 // mid 绝对不可能是最小值
		}
	}
	return nums[low]
}