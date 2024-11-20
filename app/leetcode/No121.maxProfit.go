package main

import "fmt"

func maxProfit(prices []int) int {
	// 暴力解法
	res := 0
	n := len(prices)
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			m := prices[j] - prices[i]
			if m > res {
				res = m
			}
		}
	}
	return res
}

func maxProfit2(prices []int) int {
	// 每次都假设是今天卖出，然后求今天之前的历史最低点。而这个历史最低点并不需要额外遍历，而是每天考虑的时候顺带记录的
	// 实际上就是找两个数的的差值最大，假设当前位置卖出股票，那么当前股票价格就定了，那么就只要找前面股票价格最低时候，但是此时不要循环，因为你一开始遍历过来时就可以把最小价格找出来，所以只需要一次遍历
	res := 0
	buyPrice := prices[0] // 保存[0,i) 区间内的最低价（即股票买入价）
	for i := 1; i < len(prices); i++ {
		if prices[i] < buyPrice {
			buyPrice = prices[i]
		} else {
			p := prices[i] - buyPrice
			if p > res {
				res = p
			}
		}
	}
	return res
}

func main() {
	prices := []int{7, 1, 5, 3, 6, 4}
	r := maxProfit2(prices)
	fmt.Println(r)
}
