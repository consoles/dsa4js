package main

import (
	"fmt"
	"sort"
)

func hIndex(citations []int) int {
	// 读懂题意：
	// 至少有1篇论文的引用次数>=1
	// 至少有2篇论文的引用次数>=2
	// 至少有3篇论文的引用次数>=3
	// 至少有4篇论文的引用次数>=4
	// 至少有5篇论文的引用次数>=5
	// 给定一个数组，求一个最大的 h，使得数组中至少有 h 个数都大于等于 h
	// [3, 0, 6, 1, 5]
	// [0, 1, 3, 5, 6]
	// 从后向前遍历
	// 到 6 的时候可以确定至少有 1 篇论文引用次数大于等于 1
	// 到 5 的时候可以确定至少有 2 篇论文引用次数大于等于 2
	// 到 3 的时候可以确定至少有 3 篇论文引用次数大于等于 3
	sort.Ints(citations)
	h := 0
	for i := len(citations) - 1; i >= 0 && citations[i] > h; i-- {
		h++
	}
	return h
}

func main() {
	// citations := []int{3, 0, 6, 1, 5}
	citations := []int{1, 3, 1}
	r := hIndex(citations)
	fmt.Println(r)
}
