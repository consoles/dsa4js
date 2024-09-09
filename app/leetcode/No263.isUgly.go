package main

import "fmt"

func isUgly(n int) bool {
	// for n != 0 {
	// 	if n%2 == 0 {
	// 		n /= 2
	// 	} else if n%3 == 0 {
	// 		n /= 3
	// 	} else if n%5 == 0 {
	// 		n /= 5
	// 	} else {
	// 		break
	// 	}
	// }
	if n <= 0 {
		return false
	}
	factors := [3]int{2, 3, 5}
	for _, factor := range factors {
		for n%factor == 0 {
			n /= factor
		}
	}
	return n == 1
}

func main() {
	n := 14
	r := isUgly(n)
	fmt.Println(r)
}
