package main

import "fmt"

func wordBreak(s string, wordDict []string) bool {
	// leetcode 是否 break，可以拆分为
	// l 是否是单词表的单词，剩余子串能否 break
	// le 是否是单词表的单词，剩余子串能否 break
	// ...
	wordMap := map[string]bool{}
	for _, word := range wordDict {
		wordMap[word] = true
	}
	return canBreak(0, s, wordMap)
}

func canBreak(start int, s string, wordMap map[string]bool) bool {
	if start == len(s) {
		return true
	}
	for i := start + 1; i <= len(s); i++ {
		prefix := s[start:i]
		if wordMap[prefix] && canBreak(i, s, wordMap) {
			return true
		}
	}
	return false
}

func canBreak2(start int, s string, wordMap map[string]bool, memo map[int]bool) bool {
	if start == len(s) {
		return true
	}
	// ok 表示 键 start 是否在 memo 中存在， res 则是取 memo 中键为 start 的值
	if res, ok := memo[start]; ok {
		return res
	}
	for i := start + 1; i <= len(s); i++ {
		prefix := s[start:i]
		if wordMap[prefix] && canBreak2(i, s, wordMap, memo) {
			memo[start] = true
			return true
		}
	}
	memo[start] = false
	return false
}

func wordBreak2(s string, wordDict []string) bool {
	wordMap := map[string]bool{}
	for _, word := range wordDict {
		wordMap[word] = true
	}
	memo := map[int]bool{}
	return canBreak2(0, s, wordMap, memo)
}

func wordBreak3(s string, wordDict []string) bool {
	// BFS
	// 指针 0 入列，随后它出列，1,2,3,4,5,6,7 是它的子节点，分别与 0 构成前缀子串，是单词就让它入列，继续考察以它为起点的剩余子串，不是单词则对应的指针不入列
	// 指针越界就表示没有剩余子串了，如果前缀子串是单词，则说明一直在切出单词，返回 true
	// 如果时钟没有返回 true，则返回 false
	l := len(s)
	wordMap := map[string]bool{}
	for _, v := range wordDict {
		wordMap[v] = true
	}
	queue := []int{}
	queue = append(queue, 0)
	for len(queue) > 0 {
		start := queue[0]
		queue = queue[1:]
		for i := start + 1; i <= l; i++ {
			prefix := s[start:i]
			if wordMap[prefix] {
				if i < l {
					queue = append(queue, i)
				} else {
					return true
				}
			}
		}
	}
	return false
}

func wordBreak4(s string, wordDict []string) bool {
	l := len(s)
	wordMap := map[string]bool{}
	for _, v := range wordDict {
		wordMap[v] = true
	}
	queue := []int{}
	queue = append(queue, 0)
	visited := map[int]bool{}
	for len(queue) > 0 {
		start := queue[0]
		queue = queue[1:]
		if visited[start] {
			continue
		}
		visited[start] = true
		for i := start + 1; i <= l; i++ {
			prefix := s[start:i]
			if wordMap[prefix] {
				if i < l {
					queue = append(queue, i)
				} else {
					return true
				}
			}
		}
	}
	return false
}

func wordBreak5(s string, wordDict []string) bool {
	wordDictSet := map[string]bool{}
	for _, word := range wordDict {
		wordDictSet[word] = true
	}

	// dp[i] 表示字符串s的前i个字符是否可以被空格拆分成字典中的单词
	dp := make([]bool, len(s)+1)
	dp[0] = true // 空字符串总是可以拆分的
	for i := 1; i <= len(s); i++ {
		for j := 0; j < i; j++ {
			// 尝试所有的子字符串 s[j:i]
			if dp[j] && wordDictSet[s[j:i]] {
				dp[i] = true
				break
			}
		}
	}
	return dp[len(s)]
}

func main() {
	s := "leetcode"
	wordDict := []string{"leet", "code"}
	f := wordBreak3(s, wordDict)
	fmt.Println(f)
}
