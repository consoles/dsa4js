package main

import (
	"fmt"
	"math"
	"sort"
)

func largestSumAfterKNegations(nums []int, k int) int {
	// 贪心算法：
	// 按照从小到大进行排序，从数轴左侧向右侧将负数尽可能转化为正数, 每转化 1 次，消耗 1 次 k
	sort.Ints(nums)
	sum := 0
	hasZero := false
	for i, num := range nums {
		if num < 0 && k > 0 {
			nums[i] = -num
			k--
		}
		if num == 0 {
			hasZero = true
		}
		sum += nums[i]
	}
	// k 在转化为负数的过程中刚好消耗完了
	if k == 0 {
		return sum
	}
	// 如果有零，则剩余所有的次数都去消耗对 0 进行相反数操作，最终和不变
	if hasZero {
		return sum
	}
	// 如果剩余的次数为偶数，则不断对同一个正数进行相反数的操作，最终和不变
	if k%2 == 0 {
		return sum
	}
	// 负数全部转为正数了，但是剩余的转化次数为奇数 -> 则最终一定要有 1 个正数被转化
	// 此时在负数转为正数的过程中相比于原来的数组可能产生了更小的正数，我们找到那个最小的正数
	min := math.MaxInt
	for _, num := range nums {
		if num < min {
			min = num
		}
	}
	return sum - 2*min
}

func main() {
	nums := []int{4, 2, 3}
	r := largestSumAfterKNegations(nums, 1)
	fmt.Println(r)
}
