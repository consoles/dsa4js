package main

import (
	"fmt"
	"sort"
)

func frequencySort(s string) string {
	// 统计每个字符出现的次数
	// 将字符切片按照出现的次数从大到小进行排序
	// 重建字符串
	// ch -> coun
	counter := make(map[rune]int, 0)
	for _, ch := range s {
		counter[ch]++
	}
	var chList []rune
	for k, _ := range counter {
		chList = append(chList, k)
	}
	sort.Slice(chList, func(i, j int) bool {
		ch1 := chList[i]
		ch2 := chList[j]
		return counter[ch2] < counter[ch1]
	})
	res := ""
	for _, ch := range chList {
		count := counter[ch]
		for i := 0; i < count; i++ {
			res += string(ch)
		}
	}
	return res
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func frequencySort2(s string) string {
	// 使用桶排序的思想
	// 1. 遍历字符串，统计每个字符出现的频率，同时记录最高频率 maxFreq
	counter := make(map[rune]int, 0)
	maxFreq := 0
	for _, ch := range s {
		counter[ch]++
		maxFreq = max(maxFreq, counter[ch])
	}
	// 2. 创建桶，存储从 1 到 maxFreq 的每个出现频率的字符
	buckets := make([][]rune, maxFreq+1)
	for ch, count := range counter {
		buckets[count] = append(buckets[count], ch)
	}
	// 3. 按照出现频率从大到小的顺序遍历桶，对于每个出现频率，获得对应的字符，然后将每个字符按照出现频率拼接到排序后的字符串
	res := ""
	for i := maxFreq; i >= 0; i-- {
		for _, ch := range buckets[i] {
			for j := 0; j < i; j++ {
				res += string(ch)
			}
		}
	}
	return res
}

func main() {
	// s := "tree"
	// s := "cccaaa"
	s := "Aabb"
	r := frequencySort2(s)
	fmt.Println(r)
}
