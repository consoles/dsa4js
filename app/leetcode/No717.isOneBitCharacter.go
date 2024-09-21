package main

func isOneBitCharacter(bits []int) bool {
	// 遇到 0 走 1 步，遇到 1 走 2 步
	// 题目告诉我们最后一个元素一定是 0
	// 因此如果最后恰好到达 n-1 则满足题意了
	pos := 0
	n := len(bits)
	for pos < n-1 {
		v := bits[pos]
		if v == 0 {
			pos++
		} else if v == 1 {
			pos += 2
		}
	}
	return pos == n-1
}
