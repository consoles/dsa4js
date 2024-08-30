package main

import (
	"fmt"
	"math"
)

var count = math.MaxInt

func minMutation(startGene string, endGene string, bank []string) int {
	count = math.MaxInt
	used := make([]bool, len(bank))
	dfs(startGene, endGene, bank, used, 0)
	if count == math.MaxInt {
		return -1
	}
	return count
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func dfs(start, end string, bank []string, used []bool, step int) {
	if start == end {
		count = min(count, step)
		return
	}
	for i := 0; i < len(bank); i++ {
		if used[i] {
			continue
		}
		ok := checkChangeOnce(start, bank[i])
		if ok {
			used[i] = true
			dfs(bank[i], end, bank, used, step+1)
			used[i] = false
		}
	}
}

func checkChangeOnce(a, b string) bool {
	// 判断字符串从 a -> b 是否变化了 1 次
	count := 0
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			count++
		}
	}
	return count == 1
}

func minMutation2(start, end string, bank []string) int {
	q := make([]string, 0)
	q = append(q, start)
	visited := make(map[string]bool)
	step := 0
	for len(q) > 0 {
		size := len(q)
		for i := 0; i < size; i++ {
			cur := q[0]
			q = q[1:]
			if cur == end {
				return step
			}
			for _, next := range bank {
				if visited[next] {
					continue
				}
				ok := checkChangeOnce(cur, next)
				if ok {
					visited[next] = true
					q = append(q, next)
				}
			}
		}
		step++
	}
	return -1
}

func main() {
	start := "AACCGGTT"
	end := "AACCGGTA"
	bank := []string{"AACCGGTA"}

	// start := "AACCGGTT"
	// end := "AAACGGTA"
	// bank := []string{"AACCGGTA", "AACCGCTA", "AAACGGTA"}

	// start := "AAAAACCC"
	// end := "AACCCCCC"
	// bank := []string{"AAAACCCC", "AAACCCCC", "AACCCCCC"}
	c := minMutation2(start, end, bank)
	fmt.Println(c)
}
