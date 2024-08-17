package main

import (
	"fmt"
	"math/rand"
	"sort"
)

func majorityElement(nums []int) int {
	counter := make(map[int]int)
	for _, num := range nums {
		counter[num]++
		if counter[num] > len(nums)/2 {
			return num
		}
	}
	return -1
}

func majorityElement2(nums []int) int {
	// 如果数组中存在多数元素，那么至少有大于一半的元素是相同的, 所以可以先排序，然后直接取中间的数
	sort.Ints(nums)
	return nums[len(nums)/2]
}

func checkMajority(nums []int, target int) bool {
	counter := 0
	for _, num := range nums {
		if num == target {
			counter++
		}
	}
	return counter > len(nums)/2
}

func majorityElement3(nums []int) int {
	// 随机化，因为超过一半的元素是相同的，所以我们随机挑选一个元素，有很大概率该元素就是多数元素
	// 理论时间复杂度为 O(∞)，均摊复杂度确是O(n)
	for {
		randIndex := rand.Intn(len(nums))
		if checkMajority(nums, nums[randIndex]) {
			return nums[randIndex]
		}
	}
}

func countInRange(nums []int, target, start, end int) int {
	counter := 0
	for i := start; i <= end; i++ {
		if nums[i] == target {
			counter++
		}
	}
	return counter
}

func calculateMajority(nums []int, start, end int) int {
	if start == end {
		return nums[start]
	}
	mid := (start + end) / 2
	l := calculateMajority(nums, start, mid)
	r := calculateMajority(nums, mid+1, end)

	// 左右 2 部分有相同的众数
	if l == r {
		return l
	}
	leftCount := countInRange(nums, l, start, end)
	rightCount := countInRange(nums, r, start, end)
	if leftCount > rightCount {
		return l
	}
	return r
}

func majorityElement4(nums []int) int {
	// 如果 a 是 nums 的众数，那么如果把 nums 分成 2 部分，则 a 至少是1部分的众数（可以用反证法证明）
	return calculateMajority(nums, 0, len(nums)-1)
}

func majorityElement5(nums []int) int {
	// Boyer-Moore 投票算法
	// 同归于尽消杀法
	// 第一个到来的士兵，直接插上自己阵营的旗帜占领这块高地，此时领主 winner 就是这个阵营的人，现存兵力 count = 1。
	// 如果新来的士兵和前一个士兵是同一阵营，则集合起来占领高地，领主不变，winner 依然是当前这个士兵所属阵营，现存兵力 count 加一；
	// 如果新来到的士兵不是同一阵营，则前方阵营派一个士兵和它同归于尽。 此时前方阵营兵力-1, 即使双方都死光，这块高地的旗帜 winner 不变，没有可以去换上自己的新旗帜。
	// 当下一个士兵到来，发现前方阵营已经没有兵力，新士兵就成了领主，winner 变成这个士兵所属阵营的旗帜，现存兵力 count ++。
	// 就这样各路军阀一直厮杀以一敌一同归于尽的方式下去，直到少数阵营都死光，剩下几个必然属于多数阵营的，winner 是多数阵营。
	count := 0
	candidate := -1
	for _, num := range nums {
		if count == 0 {
			candidate = num
		}
		if num == candidate {
			count++
		} else {
			count--
		}
	}
	return candidate
}

func main() {
	nums := []int{3, 2, 3}
	num := majorityElement5(nums)
	fmt.Println(num)

	nums = []int{2, 2, 1, 1, 1, 2, 2}
	num = majorityElement5(nums)
	fmt.Println(num)
}
