package main

import "fmt"

// 给你一个数组 nums 和一个值 val，你需要 原地 移除所有数值等于 val 的元素。元素的顺序可能发生改变。然后返回 nums 中与 val 不同的元素的数量。

// 假设 nums 中不等于 val 的元素数量为 k，要通过此题，您需要执行以下操作：

// 更改 nums 数组，使 nums 的前 k 个元素包含不等于 val 的元素。nums 的其余元素和 nums 的大小并不重要。
// 返回 k。
// 用户评测：

// 评测机将使用以下代码测试您的解决方案：

// int[] nums = [...]; // 输入数组
// int val = ...; // 要移除的值
// int[] expectedNums = [...]; // 长度正确的预期答案。
//                             // 它以不等于 val 的值排序。

// int k = removeElement(nums, val); // 调用你的实现

// assert k == expectedNums.length;
// sort(nums, 0, k); // 排序 nums 的前 k 个元素
// for (int i = 0; i < actualLength; i++) {
//     assert nums[i] == expectedNums[i];
// }
// 如果所有的断言都通过，你的解决方案将会 通过。

// 示例 1：

// 输入：nums = [3,2,2,3], val = 3
// 输出：2, nums = [2,2,_,_]
// 解释：你的函数函数应该返回 k = 2, 并且 nums 中的前两个元素均为 2。
// 你在返回的 k 个元素之外留下了什么并不重要（因此它们并不计入评测）。
// 示例 2：

// 输入：nums = [0,1,2,2,3,0,4,2], val = 2
// 输出：5, nums = [0,1,4,0,3,_,_,_]
// 解释：你的函数应该返回 k = 5，并且 nums 中的前五个元素为 0,0,1,3,4。
// 注意这五个元素可以任意顺序返回。
// 你在返回的 k 个元素之外留下了什么并不重要（因此它们并不计入评测）。

// 提示：

// 0 <= nums.length <= 100
// 0 <= nums[i] <= 50
// 0 <= val <= 100

func removeElement(nums []int, val int) int {
	i, j := 0, len(nums)-1
	for i <= j {
		// 从前向后找到第一个等于 val 的元素
		if nums[i] != val {
			i++
		}
		// 从后向前找到第一个不等于 val 的元素
		if nums[j] == val {
			j--
		}
		if i > j {
			break
		}
		nums[i], nums[j] = nums[j], nums[i]
	}
	return i
}

func removeElement2(nums []int, val int) int {
	// 左指针 left 指向下一个将要赋值的位置
	// 右指针 right 指向当前将要处理的元素
	// 保证 [0,left) 中的元素都不等于 val
	left := 0
	for right := 0; right < len(nums); right++ {
		// 如果右指针指向的元素不等于 val，它一定是输出数组的一个元素，我们将右指针指向的元素复制到左指针的位置，然后左右指针同时右移
		// 如果右指针指向的元素等于 val，它不能在输出数组利，此时左指针不动，右指针右移
		if nums[right] != val {
			nums[left] = nums[right]
			left++
		}
	}
	return left

	// 算法缺点：如果要移除的元素恰好在数组开头，例如:[1,2,3,4,5]，中移除 1，我们需要将 [2,3,4,5] 都左移一位，实际上我们将最后一个元素 5 取代开头的 1 即可，这个优化在序列中 val 较少的时候非常有效
}

func removeElement3(nums []int, val int) int {
	left, right := 0, len(nums)
	for left < right {
		if nums[left] == val {
			nums[left] = nums[right-1]
			right--
		} else {
			left++
		}
	}
	return left
}

func removeElement4(nums []int, val int) int {
	l := 0
	for _, v := range nums {
		if v != val {
			nums[l] = v
			l++
		}
	}
	return l
}

func main() {
	nums := []int{3, 2, 2, 3}
	val := 3
	x := removeElement4(nums, val)
	fmt.Println(nums, x)
	nums = []int{0, 1, 2, 2, 3, 0, 4, 2}
	val = 2
	x = removeElement4(nums, val)
	fmt.Println(nums, x)
}
