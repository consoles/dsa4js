package main

import "fmt"

type Stack struct {
	arr [][2]int // 保存温度和这个温度所在的索引
}

func NewStack() *Stack {
	return &Stack{
		arr: make([][2]int, 0),
	}
}

func (s *Stack) Peek() [2]int {
	return s.arr[len(s.arr)-1]
}

func (s *Stack) Push(index, value int) {
	s.arr = append(s.arr, [2]int{index, value})
}

func (s *Stack) Pop() [2]int {
	v := s.arr[len(s.arr)-1]
	s.arr = s.arr[:len(s.arr)-1]
	return v
}

func (s *Stack) Empty() bool {
	return len(s.arr) == 0
}

func dailyTemperatures(temperatures []int) []int {
	// 通俗的显而易见的暴力法会超时 O(N^2)
	// n := len(temperatures)
	// res := make([]int, n)
	// for i := 0; i < n - 1; i++ {
	// 	t := temperatures[i]
	// 	for j := i + 1; j < n; j++ {
	// 		if temperatures[j] > t {
	// 			res[i] = j - i
	// 			break
	// 		}
	// 	}
	// }
	// return res

	// 使用单调栈
	// 保持从栈顶 -> 栈底 是递增的
	s := NewStack()
	res := make([]int, len(temperatures))
	for i := 0; i < len(temperatures); i++ {
		v := temperatures[i]
		for !s.Empty() && v > s.Peek()[1] {
			kv := s.Pop()
			index := kv[0]
			// value:=kv[1]
			res[index] = i - index
		}
		s.Push(i, v)
	}
	return res
}

func dailyTemperatures2(temperatures []int) []int {
	// 不自己封装栈这种数据结构，直接使用数组
	n := len(temperatures)
	res := make([]int, n)
	stack := make([]int, 0)
	for i := 0; i < n; i++ {
		t := temperatures[i]
		for len(stack) > 0 && t > temperatures[stack[len(stack)-1]] {
			index := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			res[index] = i - index
		}
		stack = append(stack, i)
	}
	return res
}

func main() {
	temperatures := []int{73, 74, 75, 71, 69, 72, 76, 73}
	// temperatures := []int{30, 40, 50, 60}
	// temperatures := []int{30, 60, 90}
	r := dailyTemperatures(temperatures)
	fmt.Println(r)
}
