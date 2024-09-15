package main

import "fmt"

func findAndReplacePattern(words []string, pattern string) []string {
	var check func(string, string) bool
	check = func(word string, pattern string) bool {
		if len(word) != len(pattern) {
			return false
		}
		// 建立 word -> pattern, pattern -> word 的双向映射
		// 在数学中被称为双射
		word2Pattern := make(map[byte]byte, 0)
		pattern2Word := make(map[byte]byte, 0)
		for i := 0; i < len(word); i++ {
			wordC := word[i]
			patternC := pattern[i]
			patternV, patternExist := word2Pattern[wordC]
			if patternExist && patternC != patternV {
				return false
			}
			word2Pattern[wordC] = patternC
			wordV, wordExists := pattern2Word[patternC]
			if wordExists && wordV != wordC {
				return false
			}
			pattern2Word[patternC] = wordC
		}
		return true
	}
	res := make([]string, 0)
	for _, word := range words {
		if check(word, pattern) {
			res = append(res, word)
		}
	}
	return res
}

func findAndReplacePattern2(words []string, pattern string) []string {
	res := make([]string, 0)
	s := normalization(pattern)
	for _, word := range words {
		s2 := normalization(word)
		if s2 == s {
			res = append(res, word)
		}
	}
	return res
}

func normalization(word string) string {
	// 字符串归一化
	// https://leetcode.cn/problems/find-and-replace-pattern/solutions/1595690/by-xiaohu9527-4xr2/
	// aab -> aab
	// ccf -> aab
	s := ""
	cur := 'a'
	mp := make(map[rune]rune, 0)
	for _, ch := range word {
		_, ok := mp[ch]
		if !ok {
			mp[ch] = cur
			cur++
		}
		v := mp[ch]
		s += string(v)
	}
	return s
}

func main() {
	words := []string{"abc", "deq", "mee", "aqq", "dkd", "ccc"}
	r := findAndReplacePattern2(words, "abb")
	// r := normalization("aab")
	// r := normalization("ccf")
	fmt.Println(r)
}
