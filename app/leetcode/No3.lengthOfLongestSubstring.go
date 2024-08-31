package main

import "fmt"

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func lengthOfLongestSubstring(s string) int {
	n := len(s)
	if n == 0 {
		return 0
	}
	l := -1
	// 暴力法
	for i := 0; i < n; i++ {
		set := map[byte]struct{}{}
		for j := i; j < n; j++ {
			if _, ok := set[s[j]]; ok {
				break
			} else {
				set[s[j]] = struct{}{}
			}
		}
		l = max(l, len(set))
	}
	return l
}

func lengthOfLongestSubstring2(s string) int {
	// 维护一个滑动窗口，保证窗口内不存在重复元素
	// 对字符串进行遍历，每遍历一个字符就把它加入到窗口之中，同时更新窗口的左边界，保证窗口内没有重复元素
	// 可以看出窗口左边界的维护，依赖于新加入字符上一次出现的位置

	// 滑动窗口: 解题模板
	//外层循环扩展右边界，内层循环扩展左边界
	// for (int l = 0, r = 0 ; r < n ; r++) {
	// 	//当前考虑的元素
	// 	while (l <= r && check()) {//区间[left,right]不符合题意
	//         //扩展左边界
	//     }
	//     //区间[left,right]符合题意，统计相关信息
	// }
	set := map[byte]bool{}
	maxLen := 0
	for left, right := 0, 0; right < len(s); right++ { // 每一轮扩展右边界
		ch := s[right] // right 指向的元素是当前需要考虑的元素
		for left < right && set[ch] {
			delete(set, s[left]) // set 中有元素，则缩短左边界，同时从 set 中移除元素
			left++
		}
		set[ch] = true // 将当前元素加入
		maxLen = max(maxLen, right-left+1)
	}
	return maxLen
}

func lengthOfLongestSubstring3(s string) int {
	mp := map[byte]int{} // 记录每个字符上一次出现的位置
	maxLen := 0
	for l, r := 0, 0; r < len(s); r++ {
		// 上面滑动窗口的优化，左边界不需要一个个向右侧移动，只需要一次性到位，直接跳到下一个不重复的位置就行
		if index, ok := mp[s[r]]; ok && index >= l {
			l = index + 1
		}
		mp[s[r]] = r
		maxLen = max(maxLen, r-l+1)
	}
	return maxLen
}

func main() {
	// s := "abcabcbb"
	// s := "bbbbb"
	s := "pwwkew"
	r := lengthOfLongestSubstring2(s)
	fmt.Println(r)
}
