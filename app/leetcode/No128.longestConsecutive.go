package main

import (
	"fmt"
)

func longestConsecutive(nums []int) int {
	numSet := map[int]bool{}
	for _, num := range nums {
		numSet[num] = true
	}
	longestStreak := 0
	// 每个数都判断 1 次这个数是不是连续序列开头的那个元素
	// 如果 num - 1 在序列中存在，则 num 一定不是序列的开头元素
	// 反之如果 num - 1 在序列中不存在，则 num 一定是序列的起点
	for num := range numSet {
		if !numSet[num-1] {
			curNum := num
			curStreak := 1
			// num 是开头元素，则向后面开始尝试看看是否连续
			for numSet[curNum+1] {
				curNum++
				curStreak++
			}
			if curStreak > longestStreak {
				longestStreak = curStreak
			}
		}
	}
	return longestStreak
}

func main() {
	// nums := []int{100, 4, 200, 1, 3, 2}
	nums := []int{1, 2, 0, 1}
	r := longestConsecutive(nums)
	fmt.Println(r)
}
