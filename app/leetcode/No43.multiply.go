package main

import (
	"fmt"
)

func str2int(s string) int {
	r := 0
	pow := 1
	for i := len(s) - 1; i >= 0; i-- {
		// fmt.Println(s[i])
		r += int((s[i] - '0')) * pow
		pow *= 10
	}
	return r
}

// 给定两个以字符串形式表示的非负整数 num1 和 num2，返回 num1 和 num2 的乘积，它们的乘积也表示为字符串形式。
func multiply(num1 string, num2 string) string {
	// 下面的实现返回的是数字形式
	// 10^0 * 123 * 6 + 10^1 * 123 * 5 + 10^2 * 123 * 4
	// n1 := str2int(num1)
	// pow := 1
	// s := 0
	// for i := len(num2) - 1; i >= 0; i-- {
	// 	s += n1 * int(num2[i]-'0') * pow
	// 	pow *= 10
	// }
	// return s

	if num1 == "0" || num2 == "0" {
		return "0"
	}

	// 返回字符串形式
	// 竖式乘法表示如下
	//   123
	//   456
	//   738 0 个0 : s1
	//  6150 1 个0 : s2
	// 49200 2 个0 : s3
	//
	ans := ""
	zeroCount := 0
	for i := len(num2) - 1; i >= 0; i-- {
		cur := mul(num1, int(num2[i]-'0'))
		for k := 0; k < zeroCount; k++ {
			cur += "0"
		}
		ans = add(ans, cur)
		zeroCount++
	}

	return ans
}

func mul(m string, n int) string {
	// n 只有 1 位
	res := ""
	carry := 0
	for i := len(m) - 1; i >= 0; i-- {
		c := int(m[i] - '0')
		r := carry + c*n
		res = fmt.Sprintf("%d", r%10) + res
		carry = r / 10
	}
	if carry > 0 {
		res = fmt.Sprintf("%d", carry) + res
	}
	return res
}

func add(num1 string, num2 string) string {
	ans := ""
	i := len(num1) - 1
	j := len(num2) - 1
	carry := 0
	for i >= 0 || j >= 0 || carry != 0 {
		m := 0
		if i >= 0 {
			m = int(num1[i] - '0')
			i--
		}
		n := 0
		if j >= 0 {
			n = int(num2[j] - '0')
			j--
		}
		s := m + n + carry
		// println(s)
		ans = fmt.Sprintf("%d", s%10) + ans
		carry = s / 10
	}

	return ans
}

func main() {
	// fmt.Println("hello")
	fmt.Println(multiply("123", "456")) // 56088
	fmt.Println(multiply("2", "3"))     // 6
	fmt.Println(multiply("12", "3"))    // 36
	// print(str2int("123"))
	// fmt.Println(add("1234", "456"))
	// fmt.Println(mul("123", 6))
}
