package main

import "fmt"

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func integerReplacement(n int) int {
	if n == 1 {
		return 0
	}
	if n%2 == 0 {
		return 1 + integerReplacement(n/2)
	}
	// 当 n 为奇数的时候，要么选择将 n + 1，要么选择将 n - 1
	// 下一步一定是 n / 2
	return 2 + min(integerReplacement(n/2), integerReplacement(n/2+1))
}

func dfs(n int64, cache map[int64]int) int {
	if n == 1 {
		return 0
	}
	if v, ok := cache[n]; ok {
		return v
	}
	var v int
	if n%2 == 0 {
		v = 1 + dfs(n/2, cache)
	} else {
		v = min(dfs(n+1, cache), dfs(n-1, cache)) + 1
	}
	cache[n] = v
	return v
}

func integerReplacement2(n int) int {
	cache := make(map[int64]int, 0)
	return dfs(int64(n), cache)
}

func main() {
	n := 8
	r := integerReplacement2(n)
	fmt.Println(r)
}
