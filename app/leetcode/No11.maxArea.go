package main

func min(num1, num2 int) int {
	if num1 < num2 {
		return num1
	} else {
		return num2
	}
}

func max(num1, num2 int) int {
	if num1 > num2 {
		return num1
	} else {
		return num2
	}
}

func maxArea(height []int) int {
	// 方法1：暴力法，面积 = dist * height
	s := -1
	for i := 0; i < len(height); i++ {
		for j := i + 1; j < len(height); j++ {
			dist := j - i
			height := min(height[i], height[j])
			s = max(s, dist*height)
		}
	}
	return s
}

func maxArea2(height []int) int {
	// 双指针：移动高度较小的那一侧指针
	i, j := 0, len(height)-1
	s := -1
	for i < j {
		dist := j - i
		s = max(s, dist*min(height[i], height[j]))
		if height[i] < height[j] {
			i++
		} else {
			j--
		}
	}
	return s
}

func main() {

}
