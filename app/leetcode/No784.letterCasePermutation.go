package main

import "fmt"

func letterCasePermutation(s string) []string {
	chars := []rune(s)
	res := make([]string, 0)
	var dfs func(int)
	dfs = func(index int) {
		for index < len(s) && s[index] >= '0' && s[index] <= '9' {
			index++
		}
		if index == len(s) {
			res = append(res, string(chars))
			return
		}
		c := chars[index]
		dfs(index + 1)
		change := false
		if c >= 'a' && c <= 'z' {
			chars[index] = c - 32
			change = true
		} else if c >= 'A' && c <= 'Z' {
			chars[index] = c + 32
			change = true
		}
		dfs(index + 1)
		if change {
			chars[index] = c
		}
	}
	dfs(0)
	return res
}

func letterCasePermutation2(s string) []string {
	res := make([]string, 0)
	q := []string{""}
	for len(q) > 0 {
		cur := q[0]
		if len(cur) == len(s) {
			res = append(res, cur)
			q = q[1:]
		} else {
			pos := len(cur)
			c := s[pos]
			if c >= 'a' && c <= 'z' {
				q = append(q, cur+string(c-32))
			} else if c >= 'A' && c <= 'Z' {
				q = append(q, cur+string(c+32))
			}
			q[0] += string(c)
		}
	}
	return res
}

func main() {
	s := "a1b2"
	r := letterCasePermutation2(s)
	fmt.Println(r)
}
