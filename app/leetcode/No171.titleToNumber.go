package main

import (
	"fmt"
)

func titleToNumber(columnTitle string) int {
	// 26 进制
	res := 0
	mul := 1
	// AB => 2 * 26^0 + 1 * 26^1
	for i := len(columnTitle) - 1; i >= 0; i-- {
		n := int(columnTitle[i]-'A') + 1
		res += n * mul
		mul *= 26
	}
	return res
}

func titleToNumber2(columnTitle string) int {
	// 正向
	res := 0
	for i := 0; i < len(columnTitle); i++ {
		n := int(columnTitle[i]-'A') + 1
		res = res*26 + n
	}
	return res
}

func main() {
	x := titleToNumber("A")
	fmt.Println(x)
}
