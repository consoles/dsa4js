package main

import (
	"fmt"
	"strings"
)

func simplifyPath(path string) string {
	stack := []string{} // 存储有效的目录名
	arr := strings.Split(path, "/")
	for _, name := range arr {
		if name == ".." {
			if len(stack) > 0 {
				// 出栈
				stack = stack[:len(stack)-1]
			}
		} else if name != "" && name != "." {
			stack = append(stack, name)
		}
	}
	return "/" + strings.Join(stack, "/")
}

func main() {
	path := "/home/"
	path = "/home//foo/"
	// path = "/home/user/Documents/../Pictures"
	// path = "/../"
	// path = "/.../a/../b/c/../d/./"
	r := simplifyPath(path)
	fmt.Println(r)
}
