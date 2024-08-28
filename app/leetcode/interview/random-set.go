package main

import (
	"fmt"
	"math/rand/v2"
)

// O(1) 时间插入，删除，返回随机元素
type RandomSet struct {
	val2idx map[int]int
	values  []int
}

func Constructor() RandomSet {
	return RandomSet{
		val2idx: make(map[int]int),
		values:  make([]int, 0),
	}
}

func (this *RandomSet) Insert(val int) bool {
	if _, ok := this.val2idx[val]; ok {
		return false
	}
	this.val2idx[val] = len(this.values)
	this.values = append(this.values, val)
	return true
}

func (this *RandomSet) Remove(val int) bool {
	if _, ok := this.val2idx[val]; !ok {
		return false
	}
	idx := this.val2idx[val]
	// 将最后一个元素和 idx 位置的元素交换，然后删除最后一个元素
	this.values[idx] = this.values[len(this.values)-1]
	this.values = this.values[:len(this.values)-1]
	delete(this.val2idx, val)
	return true
}

func (this *RandomSet) GetRandom() int {
	l := len(this.values)
	return this.values[rand.IntN(l)]
}

func main() {
	s := Constructor()
	s.Insert(1)
	s.Insert(2)
	s.Insert(3)
	s.Insert(4)
	for i := 0; i < 10; i++ {
		fmt.Println(s.GetRandom())
	}
	fmt.Println("-------------")
	s.Remove(2)
	for i := 0; i < 10; i++ {
		fmt.Println(s.GetRandom())
	}
}
