package main

import (
	"fmt"
	"sort"
)

func sum(nums []int) int {
	s := 0
	for _, num := range nums {
		s += num
	}
	return s
}

// 查找和最小的 k 对数字
func kSmallestPairs(nums1 []int, nums2 []int, k int) [][]int {
	// 暴力法：先求出所有的数字对，再按照数字对的和进行排序，取前 k 个元素
	// 这个方法在 leetcode 中会 OOM
	result := make([][]int, 0)
	for _, num1 := range nums1 {
		for _, num2 := range nums2 {
			result = append(result, []int{num1, num2})
		}
	}
	sort.Slice(result, func(i, j int) bool {
		sum1 := sum(result[i])
		sum2 := sum(result[j])
		return sum1 < sum2
	})
	return result[0:k]
}

type MaxHeap struct {
	res [][]int
}

func NewMaxHeap() *MaxHeap {
	return &MaxHeap{
		res: make([][]int, 0),
	}
}

func less(pair1, pair2 []int) bool {
	return pair1[0]+pair1[1] < pair2[0]+pair2[1]
}

func (this *MaxHeap) Put(val []int) {
	// 堆是完全二叉树，通常使用数组表示
	// 新加入的元素首先会被放到完全二叉树的最后一个节点，然后 swim 上浮到指定的位置
	this.res = append(this.res, val)
	index := len(this.res) - 1
	this.swim(index)
}

func (this *MaxHeap) swap(i, j int) {
	if i == j {
		return
	}
	this.res[i], this.res[j] = this.res[j], this.res[i]
}

func (this *MaxHeap) swim(index int) {
	// 把 index 位置的元素 swim 放置到正确的位置
	// 如果完全二叉树的根节点索引从 0 开始, 当前节点索引为 k，则 lChild = 2k+1, rChild = 2k+2
	val := this.res[index]
	for {
		parentIndex := (index - 1) / 2
		if less(this.res[parentIndex], val) {
			this.swap(index, parentIndex)
			index = parentIndex
		} else {
			break
		}
	}
}

func (this *MaxHeap) sink(index int) {
	// 如果最大堆中 父节点 < 左右孩子中的任意一个节点，则需要将左右孩子中较大的元素和它交换位置
	size := this.Size()
	if index < 0 || index >= size {
		return
	}
	val := this.res[index]
	for {
		l := 2*index + 1
		r := 2*index + 2
		if l >= size {
			break
		}
		maxIndex := l
		if r < size && less(this.res[l], this.res[r]) {
			maxIndex = r
		}
		if less(val, this.res[maxIndex]) {
			this.swap(maxIndex, index)
			index = maxIndex
		} else {
			break
		}
	}
}

func (this *MaxHeap) Offer() []int {
	// 将堆顶元素和数组中的最后一个元素交换位置，删除掉最后一个元素，同时新的堆顶元素需要向下 sink 到合适的位置
	v := this.Peek()
	lastIndex := this.Size() - 1
	this.swap(0, lastIndex)
	this.res = this.res[:lastIndex]
	this.sink(0)
	return v
}

func (this *MaxHeap) Peek() []int {
	return this.res[0]
}

func (this *MaxHeap) Empty() bool {
	return len(this.res) == 0
}

func (this *MaxHeap) Size() int {
	return len(this.res)
}

func kSmallestPairs2(nums1 []int, nums2 []int, k int) [][]int {
	// 取前 k 小：用最大堆
	// 初始化堆的容量为 k
	// 前 k 个元素依次放入堆
	// 后续的每个元素需要和堆顶元素比较大小，
	// 如果比堆顶元素小就放入堆中，对堆进行再平衡
	// 如果新的元素和堆顶元素相等或者更大，就没有必要放进去了
	heap := NewMaxHeap()
	for _, num1 := range nums1 {
		for _, num2 := range nums2 {
			cur := []int{num1, num2}
			if heap.Size() < k {
				heap.Put(cur)
			} else if less(cur, heap.Peek()) {
				heap.Offer()
				heap.Put(cur)
			} else {
				break
			}
		}
	}
	res := make([][]int, 0)
	for !heap.Empty() {
		r := heap.Offer()
		res = append(res, r)
	}
	return res
}

func main() {
	// nums1 := []int{1, 7, 11}
	// nums2 := []int{2, 4, 6}
	// k := 3

	nums1 := []int{1, 1, 2}
	nums2 := []int{1, 2, 3}
	k := 2
	r := kSmallestPairs2(nums1, nums2, k)
	fmt.Println(r)

	// heap := NewMaxHeap()
	// k := 3
	// for _, v := range []int{1, 2, 3, 4, 5} {
	// 	cur := []int{v, 0}
	// 	if heap.Size() < k {
	// 		heap.Put(cur)
	// 	} else if less(cur, heap.Peek()) {
	// 		heap.Offer()
	// 		heap.Put(cur)
	// 	}
	// }
	// for !heap.Empty() {
	// 	fmt.Println(heap.Offer())
	// }
}
