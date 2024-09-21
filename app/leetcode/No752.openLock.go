package main

import "fmt"

func openLock(deadends []string, target string) int {
	const start = "0000"
	if target == start {
		return 0
	}
	// array -> map
	// o(n) -> o(1)
	dead := map[string]bool{}
	for _, s := range deadends {
		dead[s] = true
	}
	if dead[start] {
		return -1
	}

	// 枚举 status 通过 1 次旋转得到的数字
	get := func(status string) []string {
		ret := []string{}
		s := []byte(status)
		for i, b := range s {
			s[i] = b - 1
			if s[i] < '0' {
				s[i] = '9'
			}
			ret = append(ret, string(s))
			s[i] = b + 1
			if s[i] > '9' {
				s[i] = '0'
			}
			ret = append(ret, string(s))
			s[i] = b // recover
		}
		return ret
	}

	type pair struct {
		status string
		step   int
	}

	// bfs
	q := []pair{{start, 0}}
	seen := map[string]bool{start: true}
	for len(q) > 0 {
		current := q[0]
		q = q[1:]
		for _, nxt := range get(current.status) {
			if !seen[nxt] && !dead[nxt] {
				if nxt == target {
					return current.step + 1
				}
				seen[nxt] = true
				q = append(q, pair{nxt, current.step + 1})
			}
		}
	}

	return -1
}

func nextNums(status string) []string {
	ret := []string{}
	s := []byte(status)
	for i, b := range s {
		s[i] = b - 1
		if s[i] < '0' {
			s[i] = '9'
		}
		ret = append(ret, string(s))
		s[i] = b + 1
		if s[i] > '9' {
			s[i] = '0'
		}
		ret = append(ret, string(s))
		s[i] = b // recover
	}
	return ret
}

func openLock2(deadends []string, target string) int {
	const start = "0000"
	if target == start {
		return 0
	}
	dead := map[string]bool{}
	for _, s := range deadends {
		dead[s] = true
	}
	if dead[start] {
		return -1
	}

	bfs := func(q *[]string, mine, other map[string]int) int {
		front := (*q)[0]
		*q = (*q)[1:]
		step := mine[front]
		for _, nxt := range nextNums(front) {
			_, searched := mine[nxt]
			if !searched && !dead[nxt] {
				// 如果另外一个搜索也存在该节点，说明可以连接成 1 条通路
				step2, otherSearched := other[nxt]
				if otherSearched {
					return step + step2 + 1
				}
				mine[nxt] = step + 1
				*q = append(*q, nxt)
			}
		}
		return -1
	}

	// 双向 BFS，从起点和终点分别分别向中间搜索
	// 如果两边搜索的过程中有共有节点，说明路径通了，返回 2 个搜索对应的树高和
	// 针对朴素的 BFS ，本题的树高 h 非常大，后期会造成单个队列中的元素非常多
	// 双向 bfs 使用分治的思想降低了空间复杂度
	seen1 := map[string]int{}
	seen2 := map[string]int{}
	seen1["0000"] = 0
	seen2[target] = 0
	q1 := []string{"0000"}
	q2 := []string{target}
	res := -1
	// 只有 2 个队列都不为空的时候才进行搜索，其中一个为空表示从1端搜到底都搜索不到，说明无法到达
	for len(q1) > 0 && len(q2) > 0 {
		// 这里为了 bfs 更加平均，选择较小的队列进行搜索
		if len(q1) < len(q2) {
			res = bfs(&q1, seen1, seen2)
		} else {
			res = bfs(&q2, seen2, seen1)
		}
		if res != -1 {
			return res
		}
	}
	return -1
}

func main() {
	// deadends := []string{"0201", "0101", "0102", "1212", "2002"}
	// target := "0202"

	// deadends := []string{"8888"}
	// target := "0009"

	deadends := []string{"8887", "8889", "8878", "8898", "8788", "8988", "7888", "9888"}
	target := "8888"
	r := openLock2(deadends, target)
	fmt.Println(r)
}
