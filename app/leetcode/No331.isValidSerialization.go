package main

import (
	"fmt"
	"strings"
)

func isValidSerialization(preorder string) bool {
	// 不断将叶子节点替换为空节点，例如将 4## 替换为 #
	// 最终根节点也会被替换成为空节点
	// https://blog.csdn.net/qq_45934285/article/details/137194240
	tokens := strings.Split(preorder, ",")
	stack := make([]string, 0)
	for _, token := range tokens {
		stack = append(stack, token)
		for len(stack) >= 3 && stack[len(stack)-1] == "#" && stack[len(stack)-2] == "#" && stack[len(stack)-3] != "#" {
			stack = stack[:len(stack)-3]
			stack = append(stack, "#")
		}
	}
	return len(stack) == 1 && stack[0] == "#"
}

func main() {
	preorder := "9,3,4,#,#,1,#,#,2,#,6,#,#"
	// preorder := "1,#"
	// preorder := "9,#,#,1"
	r := isValidSerialization(preorder)
	fmt.Println(r)
}
