package main

func containsNearbyDuplicate(nums []int, k int) bool {
	// 按照题目意思来就行
	n := len(nums)
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			if nums[i] == nums[j] && j-i <= k {
				return true
			}
		}
	}
	return false
}

func containsNearbyDuplicate2(nums []int, k int) bool {
	// 是否存在长度不超过的 k+1 窗口，窗口内有相同元素
	n := len(nums)
	set := make(map[int]bool)
	for i := 0; i < n; i++ {
		if i > k {
			set[nums[i-k-1]] = false
		}
		if set[nums[i]] {
			return true
		}
		set[nums[i]] = true
	}
	return false
}

func containsNearbyDuplicate3(nums []int, k int) bool {
	n := len(nums)
	m := map[int]int{}
	for i := 0; i < n; i++ {
		num := nums[i]
		index, ok := m[num]
		if ok && i-index <= k {
			return true
		}
		m[num] = i
	}
	return false
}
