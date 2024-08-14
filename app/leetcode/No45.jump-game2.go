package main

// 跳跃游戏2
// https://leetcode.cn/problems/jump-game-ii/description/
// 给定一个长度为 n 的 0 索引整数数组 nums。初始位置为 nums[0]。
// 每个元素 nums[i] 表示从索引 i 向前跳转的最大长度。换句话说，如果你在 nums[i] 处，你可以跳转到任意 nums[i + j] 处:
// 0 <= j <= nums[i]
// i + j < n
// 返回到达 nums[n - 1] 的最小跳跃次数。生成的测试用例可以到达 nums[n - 1]。
// 示例 1:
// 输入: nums = [2,3,1,1,4]
// 输出: 2
// 解释: 跳到最后一个位置的最小跳跃数是 2。
//      从下标为 0 跳到下标为 1 的位置，跳 1 步，然后跳 3 步到达数组的最后一个位置。
// 示例 2:
// 输入: nums = [2,3,0,1,4]
// 输出: 2
func jump(nums []int) int {
	// 我们的目标是到达数组的最后一个位置，因此我们可以考虑最后一步跳跃之前所在的位置，该位置通过1步就可以达到最后的位置
	// 如果有多个位置通过跳跃都能够到达最后一个位置，那么我们应该如何进行选择呢？直观上来看，我们可以「贪心」地选择距离最后一个位置最远的那个位置，也就是对应下标最小的那个位置。因此，我们可以从左到右遍历数组，选择第一个满足要求的位置。
	// 找到最后一步跳跃前所在的位置之后，我们继续贪心地寻找倒数第二步跳跃前所在的位置，以此类推，直到找到数组的开始位置。
	position := len(nums) - 1
	steps := 0
	for position > 0 {
		for i := 0; i < position; i++ {
			if i+nums[i] >= position {
				position = i
				steps++
				break
			}
		}
	}
	return steps
}

func max(x, y int) int {
	if x > y {
		return x
	}
	return y
}

func jump2(nums []int) int {
	// 贪心地进行正向查找，每次都找到可到达的最远位置
	// 例如，对于数组 [2,3,1,2,4,2,3]，初始位置是下标 0，从下标 0 出发，最远可到达下标 2。下标 0 可到达的位置中，下标 1 的值是 3，从下标 1 出发可以达到更远的位置，因此第一步到达下标 1。
	// 从下标 1 出发，最远可到达下标 4。下标 1 可到达的位置中，下标 4 的值是 4 ，从下标 4 出发可以达到更远的位置，因此第二步到达下标 4。

	end := 0         // 当前能跳跃的边界，遍历数组的时候如果到了边界，就重新更新新的边界
	maxPosition := 0 // 遍历过程中，记录当前考虑的所有跳跃可能性中，能够到达的最远位置。
	steps := 0
	// 在遍历数组时，我们不访问最后一个元素，这是因为在访问最后一个元素之前，我们的边界一定大于等于最后一个位置，否则就无法跳到最后一个位置了。如果访问最后一个元素，在边界正好为最后一个位置的情况下，我们会增加一次「不必要的跳跃次数」，因此我们不必访问最后一个元素。
	for i := 0; i < len(nums)-1; i++ {
		maxPosition = max(maxPosition, i+nums[i])
		// 到达边界就更新新的边界，并且步数加1
		if i == end {
			end = maxPosition
			steps++
		}
	}
	return steps
}

var curSteps = 0
var minSteps = 99999

func dfs(nums []int, index int) {
	if index >= len(nums)-1 {
		if curSteps < minSteps {
			minSteps = curSteps
		}
		return
	}
	curSteps += 1
	// 假设 nums[index] = 4，则分别尝试1次跳4,3,2,1步
	for i := nums[index]; i >= 1; i-- {
		dfs(nums, index+i)
	}
	curSteps -= 1
}

func jump3(nums []int) int {
	// 枚举所有可能
	dfs(nums, 0)
	return minSteps
}

func main() {
	s1 := jump3([]int{2, 3, 1, 1, 4})
	println(s1)
	s2 := jump3([]int{2, 3, 0, 1, 4})
	println(s2)
}
