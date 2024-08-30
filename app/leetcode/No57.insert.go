package main

import (
	"fmt"
	"math"
	"sort"
)

func insert(intervals [][]int, newInterval []int) [][]int {
	start, end := newInterval[0], newInterval[1]
	res := make([][]int, 0)
	if len(intervals) == 0 {
		res = append(res, newInterval)
		return res
	}
	newStart := math.MaxInt
	newEnd := math.MinInt
	for _, interval := range intervals {
		left, right := interval[0], interval[1]
		if right < start || left > end {
			res = append(res, interval)
		} else {
			// 当前区间 [left, right] 和区间 [start, end] 肯定是重叠的
			newStart = min(newStart, left)
			newStart = min(newStart, start)
			newEnd = max(newEnd, right)
			newEnd = max(newEnd, end)
		}
	}
	// 为 [newStart, newEnd] 找到合适的插入位置并进行插入, 偷懒 append 到最后用了 sort 方法
	if newStart == math.MaxInt && newEnd == math.MinInt {
		newStart = start
		newEnd = end
	}
	res = append(res, []int{newStart, newEnd})
	sort.Slice(res, func(i, j int) bool {
		return res[i][0] < res[j][0]
	})
	return res
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func insert2(intervals [][]int, newInterval []int) (res [][]int) {
	// 对于区间 s1=[l1,r1], s2=[l2,r2] 如果他们没有交集
	// 要么 s1 在 s2 左侧, r1 < l2
	// 要么 s1 在 s2 右侧, l1 > r2
	// 如果他们存在交集，则他们的并集 s1 ∪ s2 覆盖 s1 和 s2
	// s1 ∪ s2 = (min(l1,l1), max(r1, r2))
	start, end := newInterval[0], newInterval[1]
	merged := false
	for _, interval := range intervals {
		left, right := interval[0], interval[1]
		if right < start {
			// 无交集：在插入区间左侧
			res = append(res, interval)
		} else if left > end {
			// 无交集：在插入区间右侧
			if !merged {
				res = append(res, []int{start, end})
				merged = true
			}
			res = append(res, interval)
		} else {
			// 当前区间和插入区间存在交集，计算一个区间能同时覆盖 当前区间和插入区间
			// 这个区间就是他们的并集
			start = min(left, start)
			end = max(right, end)
		}
	}
	if !merged {
		res = append(res, []int{start, end})
	}
	return
}

func insert3(intervals [][]int, newInterval []int) (res [][]int) {
	// 1. 插入区间左侧的元素
	// 2. 覆盖插入区间的元素
	// 3. 插入区间右侧的元素
	start, end := newInterval[0], newInterval[1]
	l := len(intervals)
	i := 0
	for i < l && intervals[i][1] < start {
		res = append(res, intervals[i])
		i++
	}
	for i < l && intervals[i][0] <= end {
		start = min(start, intervals[i][0])
		end = max(end, intervals[i][1])
		i++
	}
	res = append(res, []int{start, end})
	for i < l && intervals[i][0] > end {
		res = append(res, intervals[i])
		i++
	}
	return res
}

func main() {
	intervals := [][]int{{1, 3}, {6, 9}}
	newInterval := []int{2, 5}

	// intervals := [][]int{{1, 2}, {3, 5}, {6, 7}, {8, 10}, {12, 16}}
	// newInterval := []int{4, 8}
	r := insert3(intervals, newInterval)
	fmt.Println(r)
}
