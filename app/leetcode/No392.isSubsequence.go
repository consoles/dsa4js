package main

// s 是否是 t 的子序列
func isSubsequence(s string, t string) bool {
	if len(s) > len(t) {
		return false
	}
	lastIndex := -1
	for i := 0; i < len(s); i++ {
		char := s[i : i+1]
		for j := 0; i < len(t); j++ {
			index := findStr(lastIndex+1, t, char)
			if index > lastIndex {
				lastIndex = index
				break
			}
			return false
		}
	}
	return true
}

// 查找特定字符串 char 是否在从 t[start:] 中
func findStr(start int, t string, char string) int {
	for i := start; i < len(t); i++ {
		if t[i] == char[0] {
			return i
		}
	}
	return -1
}

func isSubsequence2(s string, t string) bool {
	// 双指针，i 和 j 分别指向 s 和 t 的初始位置
	i, j := 0, 0
	for i < len(s) && j < len(t) {
		if s[i] == t[j] {
			i++
		}
		j++
	}
	return i == len(s)
}

func main() {
	s := "abc"
	t := "ahbgdc"
	println(isSubsequence2(s, t))
}
