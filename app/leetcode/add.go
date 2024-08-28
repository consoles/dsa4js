package main

import (
	"fmt"
	"strings"
)

// 大整数减法，假设 num1 >= num2
func sub(num1, num2 string) string {
	var result strings.Builder
	borrow := 0

	maxLen := max(len(num1), len(num2))
	num1 = padLeft(num1, maxLen)
	num2 = padLeft(num2, maxLen)

	for i := maxLen - 1; i >= 0; i-- {
		a := int(num1[i] - '0')
		b := int(num2[i] - '0')
		// 处理借位，被减数需要减 1
		a -= borrow

		// 计算是否需要借位
		if a < b {
			a += 10
			borrow = 1
		} else {
			borrow = 0
		}

		c := a - b
		result.WriteByte(byte(c + '0'))
	}
	finalResult := reverseString(result.String())
	// 因为第一步都是格式化了前导 0，这里需要进行处理
	finalResult = strings.TrimLeft(finalResult, "0")
	if finalResult == "" {
		return "0"
	}
	return finalResult
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func reverseString(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

// 填充左前导 0
func padLeft(num string, length int) string {
	if len(num) < length {
		return strings.Repeat("0", length-len(num)) + num
	}
	return num
}

func main() {
	r := sub("123", "123")
	fmt.Println(r)
}
