package main

import "fmt"

func isIsomorphic(s string, t string) bool {
	if len(s) != len(t) {
		return false
	}
	// s 和 t 是一一对应的
	s2t := make(map[byte]byte)
	t2s := make(map[byte]byte)
	for i := range s {
		x, y := s[i], t[i]
		// fmt.Println(i, x, y)
		if s2t[x] > 0 && s2t[x] != y {
			return false
		}
		if t2s[y] > 0 && t2s[y] != x {
			return false
		}
		s2t[x] = y
		t2s[y] = x
	}
	return true
}

func main() {
	s := "egg"
	t := "add"
	fmt.Println(isIsomorphic(s, t))
	s = "foo"
	t = "bar"
	fmt.Println(isIsomorphic(s, t))
	s = "paper"
	t = "title"
	fmt.Println(isIsomorphic(s, t))
	s = "badc"
	t = "baba"
	fmt.Println(isIsomorphic(s, t))
	// fmt.Println(s[0])
}
