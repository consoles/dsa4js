package main

import "math"

func mySqrt(x int) int {
	ans := 0
	for ans*ans <= x {
		ans++
	}
	if ans*ans > x {
		ans--
	}
	return ans
}

func mySqrt2(x int) int {
	// 如果这个整数的平方恰好等于输入的整数，我们就找到了这个整数
	// 如果这个整数的平方严格大于输入整数，那么这个整数肯定不是我们要找的那个数
	// 如果这个整数的平方严格小于输入整数，那么这个整数【可能】是我们要找的那个数

	// 可以使用【二分查找】不断缩小范围去猜
	if x == 0 || x == 1 {
		return x
	}

	l, r, ans := 0, x, -1
	for l <= r {
		mid := l + (r-l)/2
		res := mid * mid
		if res == x {
			return mid
		}
		if res < x {
			l = mid + 1
			ans = mid
		} else {
			r = mid - 1
		}
	}
	return ans
}

func mySqrt3(x int) int {
	// 牛顿迭代法
	if x == 0 || x == 1 {
		return x
	}

	x0 := float64(x)
	C := float64(x)
	for {
		xi := 0.5 * (x0 + C/x0)
		if math.Abs(x0-xi) < 1e-7 {
			break
		}
		x0 = xi
	}
	return int(x0)
}
