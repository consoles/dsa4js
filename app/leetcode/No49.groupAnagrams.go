package main

import (
	"fmt"
	"sort"
)

func groupAnagrams(strs []string) [][]string {
	// 声明一个 map，键为 string ，值为 []string
	m := map[string][]string{}
	for _, str := range strs {
		s := []byte(str)
		sort.Slice(s, func(i, j int) bool { return s[i] < s[j] })
		sortedStr := string(s)
		m[sortedStr] = append(m[sortedStr], str)
	}
	res := [][]string{}
	for _, v := range m {
		res = append(res, v)
	}
	return res
}

func main() {
	strs := []string{"eat", "tea", "tan", "ate", "nat", "bat"}
	fmt.Println(groupAnagrams(strs))
}
