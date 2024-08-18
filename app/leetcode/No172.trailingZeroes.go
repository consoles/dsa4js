package main

func trailingZeroes(n int) int {
	// 给定一个整数 n ，返回 n! 结果中尾随零的数量
	// 5! = 5*4*3*2*1 = 120
	// 6! = 6*5*4*3*2*1 = 720
	// 0 是由 2 的 5相乘产生的
	// 分解所有乘数，看看由多少个 (2,5) 对
	// 转为求 n! 中质因子 2 的个数和 质因子 5 的个数的最小值
	// 由于 质因子 5 的个数一定小于等于质因子 2 的个数：对于质因子 5 ，我们总能找到一个质因子 2 与其对应
	// 所以我们统计质因子 5 的个数即可
	// n/5 + n / (5*5) + n / (5*5*5) + ...
	// 每隔 5 出现 1 次 5
	// 每隔 25 出现 2 次 5
	// 每隔 125 出现 3 次 5
	count := 0
	interval := 5
	for n >= interval {
		count += n / interval
		interval *= 5
	}
	return count
}